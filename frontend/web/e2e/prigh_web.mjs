// Drives a real browser against `prigh serve -web ... -faux-script`
// (see prigh_web.sh) and prints normalised snapshots of what the page shows.
// It owns the backend so that it can restart it. The expected output lives
// in ENGINE.expected.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE (the built site),
// TEST_DIR (scratch: home/, cwd/, the faux scripts), TOKEN, and optionally
// SHOTS (a directory for screenshots of each step).
import { execFileSync, spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from "node:fs";
import { hostname } from "node:os";
import { join } from "node:path";
import { png, pngSize } from "./png.mjs";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: prigh_web.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TEST_DIR: dir, TOKEN: token } = process.env;
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });

// The model's replies, in order, across every session of one backend run.
const script = [
  { text: "let me look", tool_calls: [{ id: "c1", name: "bash", arguments: { command: "printf 'one\\ntwo\\nthree\\n'" } }] },
  { text: "three lines" },
  { text: "counting on", tool_calls: [{ id: "c2", name: "bash", arguments: { command: "seq 1 30" } }] },
  { text: "thirty lines" },
  { text: "opening it", tool_calls: [{ id: "r1", name: "read", arguments: { path: "flag.png" } }] },
  { text: "a red flag with a blue stripe" },
  { text: "you sent me two images" },
  { text: "second session reply" },
];
// After the restart.
const script2 = [
  { text: "back again" },
  // A background subagent that runs a synchronous one, and a background job;
  // the main agent sleeps meanwhile, so the order is fixed.
  {
    text: "delegating",
    tool_calls: [
      { id: "s1", name: "subagent", arguments: { task: "Survey the project" } },
      { id: "j1", name: "bash", arguments: { command: "sleep 1; echo built", background: true } },
      { id: "w1", name: "bash", arguments: { command: "sleep 2" } },
    ],
  },
  { text: "asking a helper", tool_calls: [{ id: "n1", name: "subagent", arguments: { task: "Count the files" } }] },
  { text: "There are **3** files." },
  { text: "The survey: 3 files." },
  { text: "Started a survey and a job." },
  { text: "Noted." },
  { text: "Noted." },
];
// After another restart: a bash call that asks first, a question, a plan to
// approve, a run to queue messages in, then a session to archive.
const script3 = [
  { text: "building", tool_calls: [{ id: "b1", name: "bash", arguments: { command: "echo built-ok" } }] },
  { text: "Built." },
  {
    text: "one question",
    tool_calls: [{
      id: "q1", name: "ask_user",
      arguments: { questions: [{ question: "Which database?", header: "Storage",
        options: [{ label: "Postgres", description: "the one in prod" }, { label: "SQLite" }] }] },
    }],
  },
  { text: "SQLite it is." },
  {
    text: "here is the plan",
    tool_calls: [
      { id: "p1", name: "update_plan",
        arguments: { plan: [{ step: "Write the schema", status: "in_progress" }, { step: "Test it", status: "pending" }] } },
      { id: "p2", name: "exit_plan_mode", arguments: { plan: "1. Write the **schema**\n2. Test it" } },
    ],
  },
  { text: "Implementing." },
  // Long enough to queue messages while it runs.
  { text: "waiting", tool_calls: [{ id: "w2", name: "bash", arguments: { command: "sleep 4" } }] },
  { text: "Done waiting." },
  { text: "Noted." },
  { text: "Read the attachments." },
  { text: "Like this:\n\n```ocaml\nlet rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2) (* slow *)\n```\n\n```json\n{\"name\": \"prigh\", \"ok\": true, \"n\": 3}\n```" },
];
writeFileSync(join(dir, "script.json"), JSON.stringify(script));
// Snippets for the composer (/snippet, Alt+N, #name), and questions
// without a deadline: its time would differ between runs.
const configFile = join(home, ".prigh", "config.json");
mkdirSync(join(home, ".prigh"), { recursive: true });
writeFileSync(configFile, JSON.stringify({ ask_user_timeout_seconds: 0, snippets: [
  { name: "review", text: "Review the diff for bugs.\nList them by severity." },
  { name: "tests", text: "Run the tests and fix failures." },
] }));
writeFileSync(join(dir, "script2.json"), JSON.stringify(script2));
writeFileSync(join(dir, "script3.json"), JSON.stringify(script3));

const blue = [40, 80, 220];
const red = [220, 40, 40];
writeFileSync(join(cwd, "flag.png"), png(24, 16, (x, y) => (y < 4 ? blue : red)));
const pasted = png(20, 10, () => [30, 160, 90]);
const dropped = png(12, 30, () => [200, 200, 40]);

// ---- the backend

let server;
const startServer = (listen, scriptFile) =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-web", listen, "-web-root", site, "-faux-script", scriptFile,
        "-token", token, "-cwd", cwd],
      // The terminal's tmux server is ours (not a shared -L prigh), so its
      // shell is $SHELL.
      { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh" },
        stdio: ["ignore", "ignore", "pipe"] },
    );
    let err = "";
    server.stderr.on("data", data => {
      err += data;
      process.env.DEBUG_SERVER && process.stderr.write(data);
      const m = err.match(/^prigh: prigh-web on (\S+)$/m);
      if (m) resolve(m[1]);
    });
    server.on("exit", code => reject(new Error(`backend exited (${code}):\n${err}`)));
  });
const stopServer = () =>
  new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null || server.signalCode !== null) return resolve();
    server.on("exit", resolve);
    server.kill();
  });

const url = await startServer("127.0.0.1:0", join(dir, "script.json"));
const port = new URL(url).port;

// ---- the browser

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
const page = await context.newPage();
// [window.__delay] holds back what the backend says by that many ms, so that
// what the page shows while it waits can be seen. Messages keep their order:
// those that come after the delay is lifted wait for the held ones.
await page.addInitScript(() => {
  const Real = window.WebSocket;
  window.__delay = 0;
  window.WebSocket = class extends Real {
    set onmessage(f) {
      const held = [];
      let timer = null;
      const pump = () => {
        timer = null;
        while (held.length && held[0].at <= Date.now()) f(held.shift().e);
        if (held.length) timer = setTimeout(pump, held[0].at - Date.now());
      };
      super.onmessage = e => {
        const delay = window.__delay;
        if (!delay && !held.length) return f(e);
        const last = held.length ? held[held.length - 1].at : 0;
        held.push({ e, at: Math.max(Date.now() + delay, last) });
        if (!timer) pump();
      };
    }
    get onmessage() { return super.onmessage; }
  };
});
const holdBack = ms => page.evaluate(ms => { window.__delay = ms; }, ms);
const errors = [];
page.on("console", message => {
  const text = message.text();
  // The backend going away (step 7) is logged by the browser.
  if (message.type() === "error" && !/favicon\.ico|WebSocket|ERR_CONNECTION_REFUSED|establish a connection/.test(text)) {
    errors.push(`console: ${text}`);
  }
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

// The session's card shows the cwd's last directory apart (a line of its
// own in innerText).
const cwdApart = `${cwd.slice(0, cwd.lastIndexOf("/"))}\n${cwd.slice(cwd.lastIndexOf("/"))}`;
const clean = text => text
  .replaceAll(cwdApart, "<CWD>")
  .replaceAll(cwd, "<CWD>")
  .replace(/[0-9a-f]{16}/g, "<ID>")
  .replace(/ws:\/\/127\.0\.0\.1:\d+/g, "ws://127.0.0.1:<PORT>")
  .replace(/127\.0\.0\.1:\d+/g, "127.0.0.1:<PORT>")
  .replace(/just now|\d+[smhd] ago/g, "<AGE>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .replace(/\d+% ctx/g, "<CTX>% ctx")
  // Messages' times, and the day separator a run past midnight would add.
  .replace(/(Yesterday )?\b\d\d:\d\d\b/g, "<TIME>")
  .replaceAll(hostname(), "<HOSTNAME>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "" && line !== "Today")
  .join("\n");

const section = label => console.log(`=== ${engineName}: ${label} ===`);

let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  const name = `${engineName}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`;
  await page.screenshot({ path: join(process.env.SHOTS, name) });
};

// Tool calls and thinking are collapsible: open them so that their text shows
// (not the margin notes, which fold away what the model saw).
const show = async (label, selector) => {
  await page.evaluate(() => document.querySelectorAll("details:not(.image):not(.note)").forEach(d => { d.open = true; }));
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await screenshot(label);
};

// The status, and what applies after it.
const showStatus = async label => {
  section(label);
  console.log(clean(await page.evaluate(() =>
    [...document.querySelectorAll(".session-card .card-status > :not(.status-extras:empty), .session-bar :is(.status, .status-extras)")].map(e => e.innerText).join("\n"))));
  await screenshot(label);
};

// The session's card (or bar), with each select showing its chosen option.
const showSession = async label => {
  section(label);
  console.log(clean(await page.evaluate(() =>
    [...document.querySelectorAll(":is(.session-card, .session-bar) *")]
      .filter(e => e.tagName === "SELECT" || e.matches(".where")
        || (e.children.length === 0 && e.tagName !== "OPTION" && !e.closest(".where")))
      .map(e => (e.tagName === "SELECT" ? `[${e.selectedOptions[0]?.text ?? ""}]` : e.textContent))
      .join("\n"))));
};

// From the last prompt to the end of the chat.
const showTurn = async label => {
  await page.evaluate(() => document.querySelectorAll("details:not(.image):not(.note)").forEach(d => { d.open = true; }));
  section(label);
  console.log(clean(await page.evaluate(() => {
    const prompts = document.querySelectorAll(".chat .msg.user");
    const last = prompts[prompts.length - 1];
    // With its margin notes, if it has any.
    const start = last && last.parentElement.classList.contains("noted") ? last.parentElement : last;
    const turn = [];
    for (let e = start; e; e = e.nextElementSibling) turn.push(e.innerText);
    return turn.join("\n");
  })));
  await screenshot(label);
};

// What each image [locator] matches shows, once loaded.
const showImages = async (label, locator) => {
  const describe = () => locator.evaluateAll(list =>
    list.every(img => img.complete && img.naturalWidth > 0)
      ? list.map(img => `${img.src.slice(0, img.src.indexOf(",") + 1)} ${img.naturalWidth}x${img.naturalHeight}`)
      : null);
  let images;
  while (!(images = await describe())) await page.waitForTimeout(50);
  section(label);
  console.log(images.join("\n"));
};

// What is on top at the middle of each of the composer's chips: [cover]
// (what should be over them), the chip itself, or something else. Hit
// testing skips what is inert (what a dialog or a drawer covers is): it is
// lifted for the test, so that it sees what is drawn.
const showChips = async (label, cover) => {
  const seen = await page.evaluate(cover => {
    const inert = [...document.querySelectorAll("[inert]")];
    inert.forEach(e => e.removeAttribute("inert"));
    const seen = [...document.querySelectorAll(".composer-controls .chip-anchor")].map(anchor => {
      const r = anchor.querySelector(".chip-button").getBoundingClientRect();
      const top = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
      const what = !top ? "nothing"
        : top.closest(cover) ? `under ${cover}`
        : anchor.contains(top) ? "the chip, on top"
        : `${top.tagName.toLowerCase()}.${[...top.classList].join(".")}`;
      return `${anchor.classList[1]}: ${what}`;
    });
    inert.forEach(e => e.setAttribute("inert", ""));
    return seen;
  }, cover);
  section(label);
  console.log(seen.join("\n"));
  await screenshot(label);
};

// The agents panel, with its elapsed times replaced.
const showPanel = async label => {
  section(label);
  console.log(clean(await page.locator(".agents-panel").innerText()).replace(/\b\d+(m \d\d)?s\b/g, "<T>"));
  await page.waitForTimeout(200); // its slide in
  await screenshot(label);
};

// Whether everything in [selector] is laid out without overlaps or overflow:
// each shown element inside its parent, no two siblings over each other, no
// text cut off without an ellipsis.
const layoutProblems = selector => page.evaluate(selector => {
  const root = document.querySelector(selector);
  const shown = e => {
    const s = getComputedStyle(e);
    const r = e.getBoundingClientRect();
    return s.display !== "none" && s.visibility !== "hidden" && r.width > 0 && r.height > 0 && !e.matches(":popover-open");
  };
  const name = e => `${e.tagName.toLowerCase()}.${[...e.classList].join(".")}`;
  const problems = [];
  const all = [root, ...root.querySelectorAll("*")].filter(e => (e instanceof HTMLElement || e.tagName === "svg") && shown(e) && !e.closest(":popover-open"));
  // A box without one of its own (display: contents) lays its children out
  // in its parent's.
  const contents = e => getComputedStyle(e).display === "contents";
  const boxParent = e => {
    let p = e.parentElement;
    while (p !== root && contents(p)) p = p.parentElement;
    return p;
  };
  const boxKids = e => [...e.children].flatMap(k => (contents(k) ? boxKids(k) : [k]));
  for (const e of all) {
    const r = e.getBoundingClientRect();
    if (e !== root) {
      const parent = boxParent(e);
      const p = parent.getBoundingClientRect();
      if (r.left < p.left - 0.5 || r.right > p.right + 0.5) problems.push(`${name(e)} sticks out of ${name(parent)}`);
    }
    const kids = boxKids(e).filter(k => all.includes(k));
    for (let i = 0; i < kids.length; i++)
      for (let j = i + 1; j < kids.length; j++) {
        const a = kids[i].getBoundingClientRect(), b = kids[j].getBoundingClientRect();
        const w = Math.min(a.right, b.right) - Math.max(a.left, b.left);
        const h = Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top);
        if (w > 0.5 && h > 0.5) problems.push(`${name(kids[i])} overlaps ${name(kids[j])}`);
      }
    const s = getComputedStyle(e);
    const clips = s.overflowX !== "visible" || e === root;
    if (clips && e.scrollWidth > e.clientWidth + 1 && s.textOverflow !== "ellipsis")
      problems.push(`${name(e)} cut off (${e.scrollWidth} > ${e.clientWidth})`);
  }
  return problems;
}, selector);

// The composer's chips as shown: each chip's text, or "icon" for an icon
// alone.
const controlsShown = () => page.evaluate(() => {
  return [...document.querySelectorAll(".composer-controls .chip-button")].map(chip => {
    const after = getComputedStyle(chip, "::after");
    const extra = after.display !== "none" && after.content.startsWith('"') ? after.content.slice(1, -1) : "";
    const text = (chip.innerText.trim().replace(/\s+/g, " ") + " " + extra).trim();
    return text || "icon";
  }).join(", ");
});

// Where a chip's menu stands: above the composer's box or below its chip,
// at the chip's left edge (or as wide as the box), inside the window.
const popoverPlace = () => page.evaluate(() => {
  const menu = document.querySelector("#chip-popover");
  const r = menu.getBoundingClientRect();
  const chip = menu.parentNode.querySelector(".chip-button").getBoundingClientRect();
  const box = document.querySelector(".composer-box").getBoundingClientRect();
  const near = (a, b) => Math.abs(a - b) <= 4;
  const where = r.bottom <= box.top ? "above the composer" : r.top >= chip.bottom ? "below the chip" : "over the composer";
  const across = near(r.left, box.left) && near(r.right, box.right) ? "as wide as the composer"
    : near(r.left, chip.left) ? "at the chip's left edge" : r.right <= box.right + 1 ? "shifted inside the composer" : "out of line";
  const inside = r.top >= 0 && r.left >= 0 && r.bottom <= innerHeight && r.right <= innerWidth;
  return `${menu.dataset.chip}'s menu: ${where}, ${across}, ${inside ? "inside the window" : "cut off"}; focus in it: ${menu.contains(document.activeElement)}`;
});

const bodyHas = text => page.waitForFunction(t => document.body.innerText.includes(t), text);
const bodyLacks = text => page.waitForFunction(t => !document.body.innerText.includes(t), text);
const composer = () => page.locator(".composer textarea");
// Clicks the toasts away, one at a time: each click removes the one under it.
const dismissToasts = async () => {
  const toasts = page.locator(".toast");
  for (let i = 0; i < 20 && await toasts.count(); i++) await toasts.first().click({ timeout: 2000 }).catch(() => {});
};
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));

const send = async text => {
  await composer().fill(text);
  await page.locator(".btn.send").click();
};

const sessionId = () => new URL(page.url()).searchParams.get("session");

// The terminal panel's screen (xterm.js draws a div per row).
const terminalRows = () => page.evaluate(() =>
  [...document.querySelectorAll(".terminal-panel .xterm-rows > div")].map(row => row.textContent.trimEnd()));
const terminalHas = text => page.waitForFunction(
  t => document.querySelector(".terminal-panel .xterm-rows")?.textContent.includes(t), text);
const showTerminal = async label => {
  section(label);
  console.log(clean(await page.locator(".terminal-head").innerText()));
  console.log(clean((await terminalRows()).join("\n")));
  await screenshot(label);
};
// Runs [command] in the terminal and waits for the output line [expect].
const shell = async (command, expect) => {
  await page.keyboard.type(command);
  await page.keyboard.press("Enter");
  if (expect) await terminalHas(expect);
};

// A second client asks the backend what the session's messages are.
const backendMessages = session =>
  page.evaluate(
    ([token, session]) =>
      new Promise((resolve, reject) => {
        const ws = new WebSocket(`${location.origin.replace(/^http/, "ws")}/ws`);
        const replies = {};
        ws.onerror = () => reject(new Error("websocket failed"));
        ws.onmessage = event => {
          const msg = JSON.parse(event.data);
          if (msg.id === undefined) return;
          if (msg.error) reject(new Error(JSON.stringify(msg.error)));
          replies[msg.id] = msg.result;
          if (msg.id === 1) ws.send(JSON.stringify({ id: 2, method: "get_messages", params: {} }));
          if (msg.id === 2) { ws.close(); resolve(msg.result); }
        };
        ws.onopen = () =>
          ws.send(JSON.stringify({ id: 1, method: "hello", params: { name: "e2e", token, session } }));
      }),
    [token, session],
  );

// The bytes of a PNG as a File, delivered the way a browser does.
const deliver = (kind, bytes, name) =>
  page.evaluate(
    ([kind, bytes, name]) => {
      const data = new DataTransfer();
      data.items.add(new File([new Uint8Array(bytes)], name, { type: "image/png" }));
      const target = document.querySelector(".composer textarea");
      const init = { bubbles: true, cancelable: true };
      let event;
      if (kind === "paste") {
        // Firefox ignores [clipboardData] in a script's ClipboardEvent.
        event = new ClipboardEvent("paste", init);
        Object.defineProperty(event, "clipboardData", { value: data });
      } else {
        event = new DragEvent("drop", { ...init, dataTransfer: data });
      }
      target.dispatchEvent(event);
    },
    [kind, [...bytes], name],
  );

try {
  // 1. No token saved: hello is refused and the sign-in form appears;
  //    signing in remembers the password and reloads into the app.
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await show("sign-in page", "form");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  await page.waitForSelector(".sidebar");
  // The composer shows while the new session still loads.
  await page.locator(".chat .empty").waitFor();
  await show("signed in: sidebar", ".sidebar");
  await show("signed in: chat", ".chat");
  await showSession("signed in: the session's card");

  // A dialog owns the keyboard: Tab and Shift+Tab go round it, and Esc gives
  // the focus back to the editor. A refused command stays in the editor.
  await composer().fill("/help");
  await composer().press("Enter");
  await page.locator(".modal").waitFor();
  // The dialog takes the focus after it renders.
  await page.waitForFunction(() => document.activeElement?.classList.contains("modal"));
  const focusIn = () => page.evaluate(() =>
    { const e = document.activeElement; if (!e) return "none"; return e.closest(".modal") ? `dialog: ${e.title || e.innerText.split("\n")[0]}` : e.className; });
  const tabs = [];
  for (const key of ["Tab", "Tab", "Tab", "Shift+Tab", "Shift+Tab", "Shift+Tab", "Shift+Tab"]) {
    await page.keyboard.press(key);
    tabs.push(`${key}: ${await focusIn()}`);
  }
  await page.keyboard.press("Escape");
  await page.waitForFunction(() => !document.querySelector(".modal"));
  await page.waitForFunction(() => document.activeElement?.closest(".composer"), null, { timeout: 2000 }).catch(() => {});
  section("a dialog keeps the focus");
  console.log(tabs.join("\n"));
  console.log(`after Esc: ${await focusIn()}`);
  await composer().fill("/modle");
  await composer().press("Enter");
  await page.locator(".toast.error").waitFor();
  section("a mistyped command");
  console.log(clean(await page.locator(".toasts").innerText()));
  console.log(`editor: ${await composer().inputValue()}`);
  await screenshot("a mistyped command");
  await composer().fill("");
  await page.locator(".toast.error").click();

  // Snippets: the picker (Alt+N, /snippet) and #name completion insert at
  // the caret and leave it after the text; /snippet add and rm save them.
  const caret = () => page.evaluate(() => {
    const t = document.querySelector(".composer textarea");
    return `value ${JSON.stringify(t.value)}, caret ${t.selectionStart}-${t.selectionEnd}, focused ${document.activeElement === t}`;
  });
  const putCaret = n => composer().evaluate((t, n) => t.setSelectionRange(n, n), n);
  const popupShown = async () => (await page.locator(".popup").count()) ? clean(await page.locator(".popup").innerText()) : "no popup";
  const savedSnippets = () => JSON.parse(readFileSync(configFile, "utf8")).snippets?.map(s => s.name).join(", ");
  await composer().fill("Please  now");
  await putCaret(7);
  await page.keyboard.press("Alt+N");
  await page.locator(".modal .picker-item").first().waitFor();
  await page.waitForFunction(() => document.activeElement?.closest(".modal"));
  await page.keyboard.type("tst");
  await page.waitForFunction(() => document.querySelector(".modal .picker-item")?.innerText.startsWith("tests"));
  await page.waitForTimeout(200); // its rise
  await show("snippets: Alt+N, filtered", ".modal");
  await page.keyboard.press("Enter");
  await page.waitForSelector(".modal", { state: "detached" });
  await page.waitForFunction(() => document.activeElement?.closest(".composer"));
  section("snippets: inserted at the caret");
  console.log(await caret());
  await page.keyboard.type("!");
  console.log(`typing goes after it: ${JSON.stringify(await composer().inputValue())}`);
  await page.waitForTimeout(200); // the editor's new height
  await screenshot("snippets: inserted at the caret");
  await composer().fill("/snippet");
  await composer().press("Control+Enter");
  await page.locator(".modal .picker-item").first().waitFor();
  await page.waitForFunction(() => document.activeElement?.closest(".modal"));
  await page.keyboard.type("bugs");
  await page.waitForFunction(() => document.querySelectorAll(".modal .picker-item").length === 1);
  await page.waitForTimeout(200);
  await show("snippets: /snippet, filtered by the text", ".modal");
  await page.keyboard.press("Enter");
  await page.waitForSelector(".modal", { state: "detached" });
  await page.waitForFunction(() => document.activeElement?.closest(".composer"));
  section("snippets: /snippet inserted");
  console.log(await caret());
  await composer().fill("/snippet tests");
  await composer().press("Control+Enter");
  await page.waitForFunction(() => document.querySelector(".composer textarea").value.startsWith("Run"));
  section("snippets: /snippet tests");
  console.log(await caret());
  // #name at a word start completes; Tab puts the text in its place.
  await composer().fill("a  b");
  await putCaret(2);
  await page.keyboard.type("#rev");
  await page.locator(".popup").waitFor();
  await page.waitForTimeout(200); // its fade in
  await show("snippets: #rev completes", ".popup");
  await page.keyboard.press("Tab");
  await page.waitForSelector(".popup", { state: "detached" });
  section("snippets: #rev replaced");
  console.log(await caret());
  await page.keyboard.type("!");
  console.log(`typing goes after it: ${JSON.stringify(await composer().inputValue())}`);
  await page.waitForTimeout(200);
  await screenshot("snippets: #rev replaced");
  section("snippets: # that is not a snippet");
  await composer().fill("");
  await page.keyboard.type("# Title #123 x#rev");
  await page.waitForTimeout(100);
  console.log(`${JSON.stringify(await composer().inputValue())}: ${await popupShown()}`);
  // Saved and removed through the config.
  await composer().fill("/snippet add fix Fix the failing test.");
  await composer().press("Control+Enter");
  await page.locator(".toast", { hasText: "snippet fix saved" }).waitFor();
  section("snippets: /snippet add");
  console.log(clean(await page.locator(".toast", { hasText: "snippet fix saved" }).innerText()));
  console.log(`config: ${savedSnippets()}`);
  await composer().fill("");
  await page.keyboard.type("#fi");
  await page.locator(".popup").waitFor();
  console.log(await popupShown());
  await page.keyboard.press("Enter");
  await page.waitForSelector(".popup", { state: "detached" });
  console.log(await caret());
  await composer().fill("/snippet rm fix");
  await composer().press("Control+Enter");
  await page.locator(".toast", { hasText: "snippet fix removed" }).waitFor();
  section("snippets: /snippet rm");
  console.log(clean(await page.locator(".toast", { hasText: "snippet fix removed" }).innerText()));
  console.log(`config: ${savedSnippets()}`);
  await composer().fill("");
  await page.keyboard.type("#fi");
  await page.waitForTimeout(100);
  console.log(`#fi: ${await popupShown()}`);
  await composer().fill("");
  await dismissToasts();

  // Reminders (/remind): added, counted on the status line, listed in a
  // picker where Delete removes one, and removed by name; none is left to
  // fire in the scripted runs below.
  const remindToast = async text => {
    const toast = page.locator(".toast", { hasText: text });
    await toast.waitFor();
    console.log(clean(await toast.innerText()));
  };
  await composer().fill("/remind stop Keep going until the tests pass.");
  await composer().press("Control+Enter");
  section("reminders: added");
  await remindToast("Reminder r1 added");
  await composer().fill("/remind every 5 Check the disk space.");
  await composer().press("Control+Enter");
  await remindToast("Reminder r2 added");
  await page.locator(".status-extras .reminders").waitFor();
  console.log(`status: ${clean(await page.locator(".status-extras .reminders").innerText())}`);
  await composer().fill("/remind");
  await composer().press("Control+Enter");
  await page.locator(".modal .picker-item").first().waitFor();
  await page.waitForFunction(() => document.activeElement?.closest(".modal"));
  await page.waitForTimeout(200); // its rise
  await show("reminders: /remind", ".modal");
  await page.keyboard.press("Delete");
  await page.waitForSelector(".modal", { state: "detached" });
  section("reminders: Delete removes the highlighted one");
  await remindToast("Reminder r1 removed");
  await composer().fill("/remind rm r2");
  await composer().press("Control+Enter");
  await remindToast("Reminder r2 removed");
  await page.waitForSelector(".status-extras .reminders", { state: "detached" });
  console.log("status: no reminders");
  await composer().fill("");
  await dismissToasts();

  // 2. A scripted run with a bash tool call and its output.
  await send("count some lines");
  await bodyHas("three lines");
  await idle();
  await page.locator(".tool").first().waitFor();
  await showTurn("bash run");
  await showSession("the session's card after a run");

  // A budget shows in the status line as spend against it.
  await send("/budget 5");
  await page.locator(".status .budget").waitFor();
  await show("a budget in the status line", ".status");
  await send("/budget off");
  await page.locator(".status .budget").waitFor({ state: "detached" });
  // 2b. Long output folds to its first and last lines; its toggle shows it
  //     all, and folds it again.
  await send("count to thirty");
  await bodyHas("thirty lines");
  await idle();
  {
    const card = page.locator(".tool").last();
    section("long output, folded");
    console.log(clean(await card.innerText()));
    await card.locator("button.fold").click();
    await card.locator("button.fold[aria-expanded=true]").waitFor();
    section("long output, expanded");
    const lines = clean(await card.innerText()).split("\n");
    console.log(`${lines.length} lines, ending: ${lines.slice(-2).join(" / ")}`);
    await screenshot("long output expanded");
    await card.locator("button.fold").click();
    await card.locator("button.fold[aria-expanded=false]").waitFor();
    console.log(`folded again: ${await card.locator("button.fold").innerText()}`);
  }

  // 3. The model reads a real PNG: the tool result shows it.
  await send("what does flag.png look like?");
  await bodyHas("a red flag with a blue stripe");
  await idle();
  await showTurn("read an image");
  await showImages("the read tool's image", page.locator(".tool img.thumb"));
  // Opened, it covers the whole page, the composer's chips too; a chip's
  // menu keeps the chips over its backdrop.
  await page.locator(".tool details.image summary").first().click();
  await page.locator(".tool details.image[open] .lightbox").waitFor();
  await page.waitForTimeout(200); // its fade
  await showChips("an image open: the chips", ".lightbox");
  await page.locator(".tool details.image[open] .lightbox").click();
  await page.locator(".tool details.image[open]").waitFor({ state: "detached" });
  await page.locator(".thinking-chip").click();
  await page.locator("#chip-popover").waitFor();
  await page.waitForTimeout(200); // its rise
  await showChips("a chip's menu open: the chips", ".menu-backdrop");
  await page.keyboard.press("Escape");
  await page.waitForSelector("#chip-popover", { state: "detached" });

  // 4. An image pasted and one dropped into the composer go with the prompt.
  await deliver("paste", pasted, "pasted.png");
  await deliver("drop", dropped, "dropped.png");
  await page.waitForFunction(() => document.querySelectorAll(".composer img").length === 2);
  await showImages("attached in the composer", page.locator(".composer img"));
  await screenshot("attached");
  await send("what are these?");
  await bodyHas("you sent me two images");
  await idle();
  console.log(`composer cleared: ${(await page.locator(".composer img").count()) === 0}`);
  await showImages("the prompt's images", page.locator(".msg.user").last().locator("img.thumb"));
  {
    const messages = await backendMessages(sessionId());
    const user = messages.filter(m => m.role === "user").at(-1);
    section("the backend's copy of the prompt");
    console.log(user.text);
    for (const image of user.images ?? []) {
      console.log(`${image.mime_type} ${pngSize(Buffer.from(image.data, "base64"))}`);
    }
  }

  // 5. A reload rejoins the same session (its id is in the address bar).
  const first = sessionId();
  console.log(`url has the session: ${/^[0-9a-f]{16}$/.test(first ?? "")}`);
  await page.reload();
  await composer().waitFor();
  await bodyHas("you sent me two images");
  console.log(`same session after reload: ${sessionId() === first}`);
  await page.locator(".sidebar .session").first().waitFor();
  await show("sidebar after reload", ".sidebar");

  // 6. A new session, then back to the first from the sidebar.
  await page.locator(".sidebar").getByRole("button", { name: "New session", exact: true }).click();
  await bodyLacks("three lines");
  await page.locator(".chat .empty").waitFor();
  await show("new session", ".chat");
  // While the backend has not answered: the prompt shows, being sent.
  await holdBack(400);
  await send("hello again");
  await page.locator(".msg.user.sending").waitFor();
  section("sending");
  console.log(clean(await page.locator(".chat").innerText()));
  console.log(`send button: ${await page.locator(".btn.send").getAttribute("title")}`);
  await screenshot("sending");
  await holdBack(0);
  await bodyHas("second session reply");
  await idle();
  console.log(`new session in the url: ${sessionId() !== first}`);
  await page.waitForFunction(() => document.querySelectorAll(".sidebar .session").length === 2);
  await show("two sessions", ".sidebar");
  await holdBack(400);
  await page.locator(".sidebar .session", { hasText: "count some lines" }).click();
  await page.locator(".chat-loading").waitFor();
  section("switching");
  console.log(clean(await page.locator(".chat").innerText()));
  console.log(`marked in the sidebar: ${await page.locator(".sidebar .session.opening").innerText().then(clean)}`);
  await screenshot("switching");
  await holdBack(0);
  await bodyHas("three lines");
  await bodyLacks("second session reply");
  console.log(`switched back: ${sessionId() === first}`);
  await show("switched back", ".chat .msg.user");

  // 7. The backend restarts: the page says so, reconnects to the same
  //    session and carries on.
  await stopServer();
  await bodyHas("reconnecting");
  section("backend gone");
  // Without the attempt count (that depends on timing).
  console.log(clean(await page.locator("body").innerText()).split("\n").filter(l => /connect/i.test(l))
    .map(l => l.replace(/reconnecting \(attempt \d+, next in [\d.]+s\)…/, "reconnecting…")).join("\n"));
  await screenshot("backend gone");
  await startServer(`127.0.0.1:${port}`, join(dir, "script2.json"));
  await bodyLacks("Connection lost");
  await bodyHas("three lines");
  console.log(`same session after reconnecting: ${sessionId() === first}`);
  await send("still there?");
  await bodyHas("back again");
  await idle();
  await showTurn("after reconnecting");

  // 8. Subagents (one nested in a background one) and a background job in
  //    the agents panel: the list, one in full, its card in the chat, the
  //    job's output; a card in the chat opens its agent.
  await send("survey it");
  await bodyHas("Started a survey and a job.");
  await idle();
  await page.locator(".status-extras .background").click();
  await page.waitForFunction(() =>
    document.querySelectorAll(".agents-row").length === 3 && !document.querySelector(".agents-row.running"));
  await showPanel("agents panel");
  await page.keyboard.press("Alt+2");
  await page.waitForSelector(".agents-detail .agents-transcript .entries");
  await showPanel("a nested subagent");
  await page.locator(".agents-foot .btn", { hasText: "Show in chat" }).click();
  await page.waitForFunction(() => {
    const card = document.querySelector("#chat [data-call=s1] [data-call=n1]");
    const chat = document.getElementById("chat").getBoundingClientRect();
    const box = card?.getBoundingClientRect();
    return box && box.top >= chat.top && box.bottom <= chat.bottom;
  });
  console.log("the nested card is in view in the chat");
  await page.keyboard.press("Alt+3");
  await page.waitForSelector(".job-output");
  await showPanel("a background job");
  await page.keyboard.press("Alt+0");
  await page.waitForSelector(".agents-panel", { state: "detached" });
  await page.locator("#chat [data-agent=a1]").click();
  await page.waitForSelector(".agents-detail .agents-transcript .entries");
  await showPanel("a card opens its agent");
  // What it is stays pinned under its transcript, scrolled either way (in a
  // short window, so that it scrolls).
  await page.setViewportSize({ width: 1280, height: 420 });
  await page.waitForTimeout(200);
  const pinned = () => page.evaluate(() => {
    const panel = document.querySelector(".agents-panel").getBoundingClientRect();
    const body = document.getElementById("agents-body");
    const foot = document.querySelector(".agents-foot").getBoundingClientRect();
    return `scrolls ${body.scrollHeight > body.clientHeight}, `
      + `footer at the panel's foot ${Math.abs(foot.bottom - panel.bottom) < 2}, `
      + `under the transcript ${body.getBoundingClientRect().bottom <= foot.top + 1}`;
  });
  section("the agent's footer stays in sight");
  await page.evaluate(() => { document.getElementById("agents-body").scrollTop = 0; });
  console.log(`at the top: ${await pinned()}`);
  await page.evaluate(() => {
    const body = document.getElementById("agents-body");
    body.scrollTop = body.scrollHeight;
  });
  await page.waitForTimeout(100);
  console.log(`at the end: ${await pinned()}`);
  console.log(clean(await page.locator(".agents-foot").innerText()).replace(/\b\d+(m \d\d)?s\b/g, "<T>"));
  await screenshot("agent footer pinned");
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.locator(".agents-close").click();

  // 9. The terminal: a shell on the backend, in the session's directory.
  //    Its keys are the shell's (Esc too); closing and reopening it, or
  //    reloading the page, finds the same shell; another session has its own.
  const clearScreen = String.raw`printf '\033[H\033[2J\033[3J'`;
  const cleared = () => page.waitForFunction(() =>
    document.querySelector(".terminal-panel .xterm-rows").textContent.trim() === "$");
  await page.locator(".terminal-toggle").click();
  await page.waitForFunction(() =>
    document.activeElement?.closest(".terminal-panel")
    && document.querySelector(".terminal-panel .xterm-rows")?.textContent.trim());
  // A known prompt, and nothing from before it.
  await shell(`PS1='$ '; ${clearScreen}`);
  await cleared();
  await shell("echo hello-from-prigh-web", "hello-from-prigh-web");
  await shell("pwd", cwd);
  await shell("cat -v");
  await page.keyboard.press("Escape");
  await page.keyboard.press("Enter");
  await terminalHas("^[");
  await page.keyboard.press("Control+D");
  await page.waitForFunction(() =>
    [...document.querySelectorAll(".terminal-panel .xterm-rows > div")]
      .map(row => row.textContent.trim()).filter(row => row !== "").at(-1) === "$");
  await showTerminal("a shell in the terminal panel");
  // The shell's size is the panel's; dragging its edge makes both taller.
  const shellRows = async () => {
    const sizes = async () => (await terminalRows()).filter(row => /^\d+ \d+$/.test(row));
    const seen = (await sizes()).length;
    await shell("stty size");
    await page.waitForFunction(n =>
      [...document.querySelectorAll(".terminal-panel .xterm-rows > div")]
        .filter(row => /^\d+ \d+$/.test(row.textContent.trim())).length > n, seen);
    return Number((await sizes()).at(-1).split(" ")[0]);
  };
  const before = await shellRows();
  console.log(`the shell has the panel's rows: ${before === (await terminalRows()).length}`);
  const edge = await page.locator(".resizer-terminal").boundingBox();
  await page.mouse.move(edge.x + 300, edge.y + edge.height / 2);
  await page.mouse.down();
  await page.mouse.move(edge.x + 300, edge.y - 160, { steps: 8 });
  await page.mouse.up();
  await page.locator(".terminal-panel .xterm").click();
  await page.waitForFunction(n => document.querySelectorAll(".terminal-panel .xterm-rows > div").length > n, before);
  const after = await shellRows();
  console.log(`taller after dragging: ${after > before}; the panel's rows: ${after === (await terminalRows()).length}`);
  const draggedHeight = await page.evaluate(() => document.querySelector(".terminal-panel").offsetHeight);
  await shell(clearScreen);
  await cleared();
  await shell("echo still-here", "still-here");
  await page.keyboard.press("Control+Backquote");
  await page.waitForSelector(".terminal-panel", { state: "detached" });
  await page.waitForFunction(() => document.activeElement?.id === "editor");
  console.log("closed: the editor has the keyboard");
  await page.keyboard.press("Control+Backquote");
  await terminalHas("still-here");
  await showTerminal("reopened: the same shell");
  await page.reload();
  await composer().waitFor();
  await terminalHas("still-here");
  console.log(`reloaded: the same height: ${await page.evaluate(h =>
    Math.abs(document.querySelector(".terminal-panel").offsetHeight - h) <= 1, draggedHeight)}`);
  await showTerminal("reloaded: the panel and the shell are back");
  await page.locator(".sidebar .session", { hasText: "hello again" }).click();
  await bodyHas("second session reply");
  await page.waitForFunction(() => {
    const rows = document.querySelector(".terminal-panel .xterm-rows")?.textContent ?? "";
    return rows.trim() !== "" && !rows.includes("still-here");
  });
  console.log("another session: another shell");
  await page.locator(".sidebar .session", { hasText: "count some lines" }).click();
  await terminalHas("still-here");
  console.log("back: the first one again");
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.waitForTimeout(400); // the sheet's slide
  await showTerminal("the terminal on a phone");
  await page.locator(".terminal-close").click();
  await page.waitForSelector(".terminal-panel", { state: "detached" });
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => document.querySelector(".app:not(.narrow)"));

  // 10. /diff is a dialog of the changed files; after /cd, the next prompt
  //     has a margin note above it, unfolding to what the model saw.
  const showNotes = async label => {
    section(label);
    console.log(clean(await page.evaluate(() =>
      [...document.querySelectorAll(".chat details.note")]
        .map(d => `${d.dataset.kind} ${d.open ? "open" : "closed"}: ${d.querySelector("summary").innerText}`)
        .join("\n"))));
    await screenshot(label);
  };
  const git = (...args) => execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
  writeFileSync(join(cwd, "notes.txt"), "one\ntwo\nthree\n");
  git("init", "-q", "-b", "main");
  git("add", "notes.txt", "flag.png");
  git("commit", "-q", "-m", "start");
  writeFileSync(join(cwd, "notes.txt"), "one\n2\nthree\nfour\n");
  writeFileSync(join(cwd, "todo.md"), "# Todo\n\n- ship it\n");
  await send("/diff");
  await page.locator(".diff-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("/diff", ".diff-dialog");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".diff-dialog", { state: "detached" });
  // A missing directory: the dialog asks before creating it.
  await send(`/cd ${join(cwd, "sub")}`);
  await page.locator(".create-dir-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("a missing directory", ".create-dir-dialog");
  await page.getByRole("button", { name: "Create folder and switch" }).click();
  await page.waitForSelector(".create-dir-dialog", { state: "detached" });
  await bodyHas("switched to it");
  console.log(`created on the host: ${existsSync(join(cwd, "sub"))}`);
  await send("anything new?");
  // The backend's copy (with its note), not the page's echo while sending.
  await page.locator(".chat .msg.noted .msg.user", { hasText: "anything new?" }).waitFor();
  await bodyHas("Noted.");
  await idle();
  await page.evaluate(() => document.querySelectorAll(".chat details").forEach(d => { d.open = false; }));
  await page.locator(".chat .msg.user").last().scrollIntoViewIfNeeded();
  await showNotes("an environment note above the prompt");
  section("the prompt shows only what was typed");
  console.log(clean(await page.locator(".chat .msg.user").last().innerText()));
  await page.locator(".chat details.note summary").last().click();
  await page.waitForTimeout(300); // its fade in
  section("a margin note unfolded");
  console.log(clean(await page.locator(".chat details.note[open] .note-text").last().innerText()));
  await screenshot("a margin note unfolded");
  await page.locator(".chat details.note summary").last().click();
  {
    const messages = await backendMessages(sessionId());
    const user = messages.filter(m => m.role === "user").at(-1);
    section("the model saw the note");
    console.log(clean(user.text));
  }
  // 11. Approvals. In mode ask a bash call asks first: "always" allows its
  //     rule for the rest of the session. The model's question is answered
  //     with clicks; in plan mode its plan is approved with y.
  await stopServer();
  await bodyHas("reconnecting");
  await startServer(`127.0.0.1:${port}`, join(dir, "script3.json"));
  await bodyLacks("Connection lost");
  // The thinking level and the mode sit beside the model, each a picker.
  await show("the model, thinking and mode controls", ".composer-controls");
  // /host opens the host picker (the session's card names the host only
  // when there is another to choose).
  await composer().fill("/host");
  await composer().press("Control+Enter");
  await page.locator(".modal .picker-item").first().waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("the host picker", ".modal");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal", { state: "detached" });
  // Quiet until pointed at: then lit, with a chevron.
  await page.locator(".thinking-chip").hover();
  await page.waitForTimeout(300);
  await screenshot("a chip pointed at");
  // A chip opens a menu standing on it, not a dialog: above the composer's
  // box, from the chip's left edge, inside the window, the keyboard in it.
  await page.locator(".thinking-chip").click();
  await page.locator("#chip-popover .popover-item", { hasText: "high" }).waitFor();
  await page.waitForTimeout(300); // its rise
  await show("the thinking chip's menu", "#chip-popover");
  console.log(await popoverPlace());
  await page.locator("#chip-popover .popover-item", { hasText: "high" }).click();
  await page.locator(".thinking-chip:not(.awaiting) .chip-label", { hasText: "high" }).waitFor();
  console.log(`closed: ${await page.locator("#chip-popover").count() === 0}; focus: ${await page.evaluate(() => document.activeElement?.id)}`);
  // The model's: a filter, the models under their providers.
  await page.keyboard.press("Control+l");
  await page.locator("#chip-popover[data-chip=model]").waitFor();
  await page.waitForTimeout(300);
  console.log(await popoverPlace());
  await screenshot("the model chip's menu");
  await page.keyboard.type("v4.1 flash");
  await page.waitForFunction(() => document.querySelectorAll("#chip-popover .popover-item").length === 1);
  await show("the model chip's menu, filtered", "#chip-popover .popover-items");
  // Esc closes it unchanged; ← → go along the chips.
  await page.keyboard.press("Escape");
  await page.waitForSelector("#chip-popover", { state: "detached" });
  await page.locator(".model-chip").click();
  await page.locator("#chip-popover[data-chip=model]").waitFor();
  await page.waitForTimeout(100);
  await page.keyboard.press("ArrowRight");
  await page.locator("#chip-popover[data-chip=thinking]").waitFor();
  await page.waitForTimeout(100);
  await page.keyboard.press("ArrowRight");
  await page.locator("#chip-popover[data-chip=mode]").waitFor();
  await page.waitForTimeout(300); // its rise
  await show("the mode chip's menu, from the model's with → →", "#chip-popover");
  console.log(await popoverPlace());
  // A click beside it closes it.
  await page.mouse.click(700, 200);
  await page.waitForSelector("#chip-popover", { state: "detached" });
  await page.locator(".mode-chip").click();
  await page.locator("#chip-popover .popover-item").first().waitFor();
  await page.locator("#chip-popover .popover-item", { hasText: "Asks: edits & commands" }).click();
  await page.locator(".toast", { hasText: "Mode: ask" }).waitFor();
  await page.locator(".mode-chip:not(.awaiting)", { hasText: "Asks: edits & commands" }).waitFor();
  await show("mode ask", ".composer-controls");
  await send("build it");
  await page.locator("#confirm").waitFor();
  await show("a confirmation", "#confirm");
  await page.locator("#confirm .confirm-answer.always").click();
  await bodyHas("Built.");
  await idle();
  await showTurn("allowed always");
  await send("ask me");
  await page.locator("#question").waitFor();
  await show("a question", "#question");
  await page.locator("#question .question-choice", { hasText: "SQLite" }).click();
  await page.locator("#question .modal-buttons .primary").click();
  await bodyHas("SQLite it is.");
  await idle();
  await showTurn("answered");
  // Keys go where the focus is: back in the composer once the dialog closed.
  await page.waitForFunction(() => document.activeElement?.closest(".composer"));
  await page.keyboard.press("Alt+M");
  await page.keyboard.press("Alt+M");
  await page.keyboard.press("Alt+M");
  // Each step is shown at once; the backend's replies settle on the last.
  await page.locator(".toast", { hasText: "Mode: plan" }).waitFor();
  await page.waitForSelector(".mode-chip.plan-mode:not(.awaiting)");
  await show("plan mode (Alt+M)", ".composer-controls");
  await showStatus("plan mode: the status line");
  await send("plan it");
  await page.locator("#confirm.plan-dialog").waitFor();
  await show("a plan to approve", "#confirm");
  await page.keyboard.press("y");
  await bodyHas("Implementing.");
  await idle();
  await page.waitForFunction(() => !document.querySelector(".mode-chip.plan-mode"));
  await show("the plan approved", ".composer-controls");
  await showStatus("the plan approved: the status line");
  await showTurn("implementing");
  await send("/permissions");
  await page.locator(".modal").waitFor();
  await show("permissions", ".modal");
  await page.keyboard.press("Escape");
  await page.waitForFunction(() => !document.querySelector(".modal"));

  // 11b. Messages sent while the agent works wait above the composer, each
  //      saying when it reaches the agent; one is removed, the other is
  //      delivered (and said to be).
  await send("wait a bit");
  await page.locator(".btn.stop").waitFor();
  await send("also check the docs");
  await composer().fill("then write the changelog");
  await composer().press("Alt+Enter");
  await page.waitForFunction(() => document.querySelectorAll(".queue .queued").length === 2);
  await page.waitForTimeout(300); // their rise
  await show("two queued messages", ".queue");
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.waitForTimeout(400); // the drawer's slide
  await screenshot("phone: the controls and the queue");
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => document.querySelector(".app:not(.narrow)"));
  await page.locator(".queued", { hasText: "then write the changelog" }).locator(".queued-remove").click();
  await page.waitForFunction(() => document.querySelectorAll(".queue .queued").length === 1);
  await page.locator(".toast", { hasText: "Removed from the queue" }).waitFor();
  await show("one removed", ".queue");
  await page.locator(".toast", { hasText: "Steer delivered" }).waitFor();
  await page.waitForSelector(".queue", { state: "detached" });
  await bodyHas("Done waiting.");
  await idle();
  section("the steer delivered");
  console.log(clean(await page.locator(".toast", { hasText: "Steer delivered" }).innerText()));
  await showTurn("the steer in the transcript");

  // The layout at phone size.
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.waitForTimeout(400); // the drawer's slide
  await screenshot("phone");
  // Its drawer is over the composer, chips and all.
  await page.locator(".session-bar .toggle-sidebar").click();
  await page.waitForFunction(() => document.querySelector(".app.narrow.sidebar-open"));
  await page.waitForTimeout(400); // the drawer's slide
  await showChips("phone: the drawer open: the chips", ".sidebar, .scrim");
  await page.keyboard.press("Escape");
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.waitForTimeout(400); // the drawer's slide
  // A chip's menu is as wide as the composer's box there.
  await page.locator(".thinking-chip").click();
  await page.locator("#chip-popover").waitFor();
  await page.waitForTimeout(300);
  section("phone: a chip's menu");
  console.log(await popoverPlace());
  await screenshot("phone: a chip's menu");
  await page.keyboard.press("Escape");
  await page.waitForSelector("#chip-popover", { state: "detached" });
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => document.querySelector(".app:not(.narrow)"));

  // 12. Sessions: archiving one (from its ⋯ menu) moves it under a divider that folds the
  //     archived away (a click unfolds them, muted); selecting several (a
  //     shift-click takes the range, across the divider) and deleting them
  //     asks first with their count and size, then removes their files.
  const sizes = text => clean(text).replace(/\d+(\.\d+)? (B|KB|MB)\b/g, "<SIZE>");
  const sessionFiles = () =>
    readdirSync(join(home, ".prigh", "sessions")).filter(f => f.endsWith(".jsonl") && !f.includes(".trace.")).length;
  await page.locator(".sidebar").getByRole("button", { name: "New session", exact: true }).click();
  await page.locator(".chat .empty").waitFor();
  await send("one more");
  await bodyHas("Noted.");
  await idle();
  await page.waitForFunction(() => document.querySelectorAll(".sidebar .session").length === 3);
  const hello = page.locator(".sidebar .session", { hasText: "hello again" });
  await hello.hover();
  await hello.locator(".more").click();
  await page.locator(".context-menu.placed [data-item=archive]").click();
  await page.locator(".sessions-divider").waitFor();
  section("archived, folded");
  console.log(sizes(await page.locator(".sidebar .sessions").innerText()));
  await page.mouse.move(700, 400);
  await screenshot("archived folded");
  await page.locator(".sessions-divider").click();
  await page.locator(".session.archived").waitFor();
  section("archived");
  console.log(sizes(await page.locator(".sidebar .sessions").innerText()));
  console.log(`muted: ${await page.locator(".session.archived").count()}`);
  await screenshot("archived");
  await page.locator(".sessions-toolbar").getByRole("button", { name: "Select" }).click();
  await page.locator(".sidebar .session", { hasText: "count some lines" }).click();
  await hello.click({ modifiers: ["Shift"] });
  await page.waitForFunction(() => document.querySelectorAll(".session.marked").length === 2);
  section("selected");
  console.log(sizes(await page.locator(".sidebar .selection-bar").innerText()));
  await screenshot("selected");
  await page.locator(".sidebar .selection-bar .danger").click();
  await page.locator(".modal").waitFor();
  section("delete confirmation");
  console.log(sizes(await page.locator(".modal").innerText()));
  await screenshot("delete confirmation");
  console.log(`session files before: ${sessionFiles()}`);
  await page.locator(".modal .btn.danger").click();
  await page.waitForFunction(() => document.querySelectorAll(".sidebar .session").length === 1);
  section("deleted");
  await page.locator(".toast", { hasText: "Deleted" }).waitFor();
  console.log(sizes(await page.locator(".toast", { hasText: "Deleted" }).innerText()));
  console.log(sizes(await page.locator(".sidebar .sessions").innerText()));
  console.log(`session files after: ${sessionFiles()}`);
  await screenshot("deleted");

  // 13. The file browser (Ctrl+Shift+F) on the session's host: browse,
  //     preview, upload (asking before replacing), download a file and a
  //     directory, new folder, rename, delete; then files attached to a
  //     prompt from the browser, dropped on the composer and picked with
  //     the attach button.
  const showFiles = async label => {
    section(label);
    console.log(clean(await page.locator(".files-list").innerText()));
    await screenshot(label);
  };
  const toast = async text => {
    await page.locator(".toast", { hasText: text }).first().waitFor();
    console.log(`toast: ${clean(await page.locator(".toast", { hasText: text }).first().innerText())}`);
  };
  const row = name => page.locator(`.files-row[data-name="${name}"]`);
  const hostFile = async (path, content) => {
    for (let i = 0; i < 100 && !(existsSync(path) && readFileSync(path, "utf8") === content); i++) {
      await page.waitForTimeout(50);
    }
    return JSON.stringify(readFileSync(path, "utf8"));
  };
  writeFileSync(join(cwd, "sub", "inner.txt"), "inside\n");
  // Ctrl+Enter sends what was typed, whatever the popup highlights.
  await composer().fill(`/cd ${cwd}/`);
  await page.locator(".popup-item.selected", { hasText: "sub/" }).waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("/cd: the directory popup", ".popup");
  await composer().press("Control+Enter");
  // Its row names the directory unless it is its project's own (no subdirectory).
  await page.waitForFunction(c => {
    const where = document.querySelector(".session.selected .session-cwd");
    return where ? where.title === c : document.querySelector(".session.selected") !== null;
  }, cwd);
  await page.keyboard.press("Control+Shift+F");
  await page.locator(".files-panel").waitFor();
  await row("flag.png").waitFor();
  await page.waitForTimeout(200); // the drawer's slide
  // The Review button comes with the diff's reply, after the listing.
  await page.locator(".files-head .review-open").waitFor();
  section("files: the session's directory");
  console.log(clean(await page.locator(".files-head").innerText()));
  console.log(clean(await page.locator(".files-crumb.current").innerText()));
  await showFiles("files: listed");
  await row("notes.txt").click();
  // In a git repository a text file previews as its changes.
  await page.locator(".files-preview .review-code").waitFor();
  section("files: a text preview");
  console.log(clean(await page.locator(".files-preview").innerText()));
  await screenshot("files: a text preview");
  await row("flag.png").click();
  await page.locator(".files-image img").waitFor();
  await showImages("files: an image preview", page.locator(".files-image img"));
  await screenshot("files: an image preview");
  await page.locator("#files-upload-input").setInputFiles({ name: "hello.txt", mimeType: "text/plain", buffer: Buffer.from("hello\n") });
  await toast('Uploaded "hello.txt"');
  await row("hello.txt").waitFor();
  console.log(`on the host: ${await hostFile(join(cwd, "hello.txt"), "hello\n")}`);
  await page.locator("#files-upload-input").setInputFiles({ name: "hello.txt", mimeType: "text/plain", buffer: Buffer.from("hello again\n") });
  await page.locator(".files-ask").waitFor();
  await show("files: replace?", ".files-ask");
  await page.locator(".files-ask .btn.danger").click();
  console.log(`replaced: ${await hostFile(join(cwd, "hello.txt"), "hello again\n")}`);
  await page.waitForFunction(() => !document.querySelector(".files-transfers"));
  {
    await row("notes.txt").click();
    const [download] = await Promise.all([
      page.waitForEvent("download"),
      page.locator(".files-actions .btn", { hasText: "Download" }).click(),
    ]);
    section("files: a downloaded file");
    console.log(download.suggestedFilename());
    console.log(JSON.stringify(readFileSync(await download.path(), "utf8")));
  }
  {
    // A path typed (relative to the directory shown), then back up: sub/ is
    // selected, its actions show.
    await page.locator(".files-edit-path").click();
    await page.locator("#files-path").fill("sub");
    await page.locator("#files-path").press("Enter");
    await row("inner.txt").waitFor();
    section("files: a typed path");
    console.log(clean(await page.locator(".files-crumb.current").innerText()));
    console.log(clean(await page.locator(".files-list").innerText()));
    await page.locator("#files-list").focus();
    await page.keyboard.press("Backspace");
    await page.locator(".files-row.selected[data-name=sub]").waitFor();
    await screenshot("files: a directory selected");
    const [download] = await Promise.all([
      page.waitForEvent("download"),
      page.locator(".files-actions .btn", { hasText: "Download .tar.gz" }).click(),
    ]);
    const archive = join(dir, "sub.tar.gz");
    await download.saveAs(archive);
    section("files: a directory downloaded");
    console.log(download.suggestedFilename());
    console.log(execFileSync("tar", ["-tzf", archive], { encoding: "utf8" }).trim().split("\n").sort().join("\n"));
  }
  await page.locator(".files-tool[title='New folder']").click();
  await page.locator("#files-ask-input").fill("made");
  await page.keyboard.press("Enter");
  await toast('Created folder "made"');
  await page.locator(".files-row.selected[data-name=made]").waitFor();
  await page.locator("#files-list").focus();
  await page.keyboard.press("F2");
  await page.locator("#files-ask-input").fill("renamed");
  await page.keyboard.press("Enter");
  await toast('Renamed "made" to "renamed"');
  await page.locator(".files-row.selected[data-name=renamed]").waitFor();
  await page.locator("#files-list").focus();
  await page.keyboard.press("Delete");
  await page.locator(".files-ask").waitFor();
  await show("files: delete?", ".files-ask");
  await page.locator(".files-ask .btn.danger").click();
  await toast('Deleted "renamed"');
  console.log(`renamed on the host: ${existsSync(join(cwd, "renamed"))}`);
  // The toast can come before the listing is refreshed.
  await row("renamed").waitFor({ state: "detached", timeout: 10000 });
  await showFiles("files: after the changes");
  // Attached from the browser, dropped on the composer, picked.
  await row("hello.txt").click();
  await page.locator(".files-actions .btn", { hasText: "Attach" }).click();
  // Uploads held back a moment: the status line shows them.
  let release;
  const held = new Promise(resolve => (release = resolve));
  await page.route("**/files/upload**", async route => { await held; await route.continue(); });
  await page.evaluate(() => {
    const data = new DataTransfer();
    data.items.add(new File(["dropped text\n"], "dropped.txt", { type: "text/plain" }));
    document.querySelector(".composer textarea").dispatchEvent(
      new DragEvent("drop", { bubbles: true, cancelable: true, dataTransfer: data }));
  });
  await page.locator("#chat-attach-input").setInputFiles({ name: "data.bin", mimeType: "application/octet-stream", buffer: Buffer.from([0, 1, 2, 3, 255, 0, 7]) });
  await page.waitForFunction(() => /^Uploading 2 files/.test(document.querySelector(".status-extras .transfers")?.textContent ?? ""));
  section("files: uploads in the status line");
  console.log(clean(await page.locator(".status-extras .transfers").innerText()).replace(/ \d+%$| …$|…$/, ""));
  await screenshot("files: uploads in the status line");
  release();
  await page.waitForFunction(() => document.querySelectorAll(".attach-chip.ready").length === 3);
  section("files: attachments in the composer");
  console.log(clean(await page.locator(".attach-chips").innerText()));
  await screenshot("files: attachments in the composer");
  console.log(`uploads on the host: ${readdirSync(join(cwd, ".prigh", "uploads")).sort().join(" ")}`);
  await page.keyboard.press("Control+Shift+F");
  await page.waitForSelector(".files-panel", { state: "detached" });
  await send("read these");
  await bodyHas("Read the attachments.");
  await idle();
  await page.waitForFunction(() => !document.querySelector(".attach-chip"));
  await showTurn("files: sent with the prompt");
  {
    const messages = await backendMessages(sessionId());
    const user = messages.filter(m => m.role === "user").at(-1);
    section("files: what the model saw");
    console.log(clean(user.text).split("\n").filter(l => l.startsWith("<file") || l.startsWith("[binary") || l === "read these" || l === "hello again" || l === "dropped text").join("\n"));
  }
  // At phone size the browser is a sheet; a preview takes it over, with a
  // way back to the list.
  await page.waitForFunction(() => !document.querySelector(".toast"));
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.locator(".files-toggle").click();
  await row("todo.md").waitFor();
  await page.waitForTimeout(400); // the sheet's slide
  await showFiles("files on a phone");
  await row("todo.md").click();
  await page.locator(".files-preview .review-code").waitFor();
  await page.waitForTimeout(200);
  await screenshot("files: a preview on a phone");
  section("files: a preview on a phone");
  console.log(`the list: ${await page.locator(".files-list").isVisible() ? "shown" : "hidden"}; ${clean(await page.locator(".files-preview-head").innerText())}`);
  await page.locator(".files-back").click();
  await row("todo.md").waitFor();
  console.log(`back: the list ${await page.locator(".files-list").isVisible() ? "shown" : "hidden"}, the preview ${await page.locator(".files-preview").count() ? "open" : "closed"}`);
  await page.locator(".files-close").click();
  await page.waitForSelector(".files-panel", { state: "detached" });
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => document.querySelector(".app:not(.narrow)"));

  // 14. Every panel resizes from its inner edge: dragged (the size shows at
  //     once and is kept), double-clicked back to its default, moved with
  //     the arrow keys; sizes fit the window. Code is highlighted in the
  //     file preview and in replies.
  const width = selector => page.evaluate(s => Math.round(document.querySelector(s).getBoundingClientRect().width), selector);
  const height = selector => page.evaluate(s => Math.round(document.querySelector(s).getBoundingClientRect().height), selector);
  const handle = name => page.locator(`.resizer[data-resize="${name}"]`);
  const valueNow = async name => Number(await handle(name).getAttribute("aria-valuenow"));
  // Released: the model has the size once the handle says so.
  const drag = async (name, dx, dy) => {
    const before = await valueNow(name);
    // Where the handle is once a panel that just opened stops moving.
    await page.waitForFunction(async n => {
      const at = () => JSON.stringify(document.querySelector(`.resizer[data-resize="${n}"]`)?.getBoundingClientRect());
      const frame = () => new Promise(requestAnimationFrame);
      const first = at();
      await frame();
      await frame();
      return first !== undefined && first === at();
    }, name);
    const box = await handle(name).boundingBox();
    const x = box.x + box.width / 2, y = box.y + box.height / 2;
    await page.mouse.move(x, y);
    await page.mouse.down();
    await page.mouse.move(x + dx / 2, y + dy / 2, { steps: 4 });
    const during = await page.evaluate(() => document.body.className);
    await page.mouse.move(x + dx, y + dy, { steps: 4 });
    await page.mouse.up();
    await page.waitForFunction(([n, v]) =>
      document.querySelector(`.resizer[data-resize="${n}"]`).getAttribute("aria-valuenow") !== String(v), [name, before]);
    return during;
  };
  const tokens = selector => page.evaluate(s =>
    [...document.querySelectorAll(`${s} [class^="hl-"]`)].slice(0, 12).map(e => `${e.className}:${e.textContent}`).join(" "), selector);
  writeFileSync(join(cwd, "sample.py"),
    "import os\n\n\n@cache\ndef greet(name: str = \"world\") -> str:\n    # say hello\n    return f\"hello {name}\" * 2\n");
  await page.keyboard.press("Control+Shift+F");
  await page.locator(".files-panel").waitFor();
  await page.locator(".files-tool[title='Refresh']").click();
  await row("sample.py").click();
  await page.locator(".files-preview .review-code .hl-kw").first().waitFor();
  await page.waitForTimeout(200);
  section("resize: a highlighted preview");
  console.log(await tokens(".files-preview .review-code"));
  {
    const at = s => page.evaluate(s => Math.round(document.querySelector(s).getBoundingClientRect().left), s);
    console.log(`left to right: chat to ${await at(".files-panel")}, the preview from ${await at(".files-preview")}, the list from ${await at(".files-side")}`);
  }
  await screenshot("resize: a highlighted preview");
  // The list's edge with the preview closed: at 1280px the two of them
  // share what the sidebar and the chat leave.
  await page.locator(".files-close-preview").click();
  await page.locator(".files-preview").waitFor({ state: "detached" });
  section("resize: the file browser's edge");
  console.log(`handles: ${await page.evaluate(() => [...document.querySelectorAll(".resizer")].map(h => `${h.dataset.resize} ${h.getAttribute("aria-valuenow")}`).join(", "))}`);
  const filesBefore = await width(".files-side");
  const bodyWhileDragging = await drag("files", -160, 0);
  console.log(`while dragging: ${bodyWhileDragging}; no text selected: ${await page.evaluate(() => getSelection().toString() === "")}`);
  // Kept with the user's preferences on the backend.
  const keptWidth = async expected => {
    const read = () => {
      try {
        return JSON.parse(readFileSync(join(home, ".prigh/prefs.json"), "utf8"))["web.files-width"] ?? null;
      } catch {
        return null;
      }
    };
    for (let i = 0; i < 50 && read() !== expected; i++) await page.waitForTimeout(100);
    return read();
  };
  console.log(`dragged 160px left: ${filesBefore} -> ${await width(".files-side")} (aria ${await valueNow("files")}), kept ${await keptWidth(560)}`);
  await page.waitForFunction(() => !document.body.className.includes("resizing"));
  await screenshot("resize: the file browser wider");
  await drag("files", -600, 0);
  console.log(`as wide as it goes: ${await width(".files-side")}, the chat keeps ${await width(".main")}`);
  await handle("files").dblclick();
  await page.waitForFunction(() => document.querySelector(".resizer[data-resize=files]").getAttribute("aria-valuenow") === "400");
  console.log(`double-clicked: ${await width(".files-side")}, kept ${await keptWidth(null)}`);
  await handle("files").focus();
  await page.keyboard.press("ArrowLeft");
  await page.keyboard.press("Shift+ArrowLeft");
  await page.waitForFunction(() => document.querySelector(".resizer[data-resize=files]").getAttribute("aria-valuenow") === "480");
  console.log(`arrow keys: ${await width(".files-side")}`);
  await screenshot("resize: a focused handle");
  section("resize: the preview's edge");
  await page.setViewportSize({ width: 1800, height: 800 });
  await row("sample.py").click();
  await page.locator(".files-preview .review-code").waitFor();
  const previewBefore = await width(".files-preview");
  await drag("files-preview", -100, 0);
  console.log(`dragged 100px left: the preview ${previewBefore} -> ${await width(".files-preview")} (aria ${await valueNow("files-preview")}), the list ${await width(".files-side")}, the chat ${await width(".main")}`);
  await page.waitForFunction(() => !document.body.className.includes("resizing"));
  await screenshot("resize: a wider preview");
  // Too narrow for both beside the chat: the preview in the list's place.
  await page.setViewportSize({ width: 1150, height: 800 });
  await page.waitForFunction(() => document.querySelector(".files-panel.alone"));
  console.log(`at 1150px: the list ${await page.locator(".files-list").isVisible() ? "shown" : "hidden"}, the preview ${await width(".files-preview")} with ${await page.locator(".files-panel .resizer").evaluateAll(hs => hs.map(h => h.dataset.resize).join(", "))}`);
  await screenshot("resize: the preview alone");
  await handle("files-preview").count().then(n => n === 0 || console.log("a preview handle while alone"));
  await page.locator(".files-back").click();
  await page.locator(".files-preview").waitFor({ state: "detached" });
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => document.querySelector(".resizer[data-resize=files]").getAttribute("aria-valuenow") === "480");
  section("resize: the sidebar's edge");
  const sidebarBefore = await width(".sidebar");
  await drag("sidebar", 80, 0);
  console.log(`dragged 80px right: ${sidebarBefore} -> ${await width(".sidebar")}`);
  await screenshot("resize: a wider sidebar");
  section("resize: kept after a reload");
  await page.reload();
  await composer().waitFor();
  await page.keyboard.press("Control+Shift+F");
  await row("sample.py").waitFor();
  await page.waitForFunction(() => !document.querySelector(".files-panel .loading, .files-wait"));
  console.log(`sidebar ${await width(".sidebar")}, files ${await width(".files-side")}`);
  section("resize: a narrower window");
  await page.setViewportSize({ width: 1120, height: 800 });
  await page.waitForFunction(() => document.querySelector(".resizer[data-resize=files]").getAttribute("aria-valuenow") !== "480");
  console.log(`sidebar ${await width(".sidebar")}, files ${await width(".files-side")}, chat ${await width(".main")}`);
  await screenshot("resize: a narrower window");
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => document.querySelector(".resizer[data-resize=files]").getAttribute("aria-valuenow") === "480");
  console.log(`wide again: sidebar ${await width(".sidebar")}, files ${await width(".files-side")}`);
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow"));
  await page.waitForTimeout(400);
  console.log(`on a phone: ${await page.locator(".resizer").count()} handles`);
  await screenshot("resize: no handles on a phone");
  await page.locator(".files-close").click();
  await page.waitForSelector(".files-panel", { state: "detached" });
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => document.querySelector(".app:not(.narrow)"));
  await handle("sidebar").dblclick();
  await send("show me code");
  await bodyHas("Like this:");
  await idle();
  section("resize: highlighted code in a reply");
  for (const lang of ["ocaml", "json"]) console.log(await tokens(`.code code.language-${lang}`));
  await page.locator(".code").last().scrollIntoViewIfNeeded();
  await screenshot("resize: highlighted code in a reply");

  // 15. The session's card (or, without the sidebar, its bar) and the
  //     composer fit, however wide the panels around the chat make it: the
  //     composer's chips take shorter labels, the bar's status drops its
  //     counters, then its words; the title keeps some room. Nothing
  //     overlaps or is cut off.
  const chatWidth = () => width(".main");
  // Drags the file browser's edge until the chat column is [target] wide.
  const chatAt = async target => {
    for (let i = 0; i < 3 && (await chatWidth()) !== target; i++) await drag("files", target - (await chatWidth()), 0);
    await page.waitForTimeout(200);
  };
  const showTopBar = async label => {
    section(label);
    const where = await page.evaluate(() => {
      const at = document.querySelector(".session-bar") ? ".session-bar" : ".session-card";
      const r = s => document.querySelector(s).getBoundingClientRect();
      const status = [...document.querySelectorAll(`${at} :is(.status, .status-extras, .card-head) > .status-item`)]
        .filter(e => getComputedStyle(e).display !== "none")
        .map(e => e.innerText.trim().replace(/\s+/g, " ") || "icon");
      const title = document.querySelector(`${at} .session-name > span`);
      const titleFits = !title ? "none" : title.scrollWidth <= title.clientWidth || title.getBoundingClientRect().width >= 100;
      return { at, titleFits, status: status.join(", "), height: Math.round(r(at).height) };
    });
    console.log(`chat ${await chatWidth()}; chips: ${clean(await controlsShown())}; ${where.at}: ${clean(where.status)}; ${where.titleFits === "none" ? "" : `title whole or at least 100px: ${where.titleFits}; `}${where.height}px high`);
    for (const s of [".session-card, .session-bar", ".composer"]) {
      const problems = await layoutProblems(s);
      console.log(`${s}: ${problems.length ? problems.join("; ") : "fits"}`);
    }
    // With the card, the panels' toggles float in the chat's top right
    // corner, clear of the jump list's button.
    const tools = await page.evaluate(() => {
      const t = document.querySelector(".chat-tools");
      if (!t) return null;
      const r = t.getBoundingClientRect(), main = document.querySelector(".main").getBoundingClientRect();
      const list = document.querySelector(".jump-list")?.getBoundingClientRect();
      const clear = !list || list.top >= r.bottom || list.right <= r.left;
      const open = [...t.querySelectorAll(".btn.open")].map(b => b.getAttribute("aria-label"));
      return `${t.querySelectorAll(".btn").length} buttons ${Math.round(r.top - main.top)}px from the top, ${Math.round(main.right - r.right)}px from the right, clear of the jump list: ${clear}, open: ${open.join(", ") || "none"}`;
    });
    if (tools) console.log(`the panels' toggles: ${tools}`);
    await screenshot(label);
  };
  await page.keyboard.press("Control+Shift+F");
  await page.locator(".files-panel").waitFor();
  for (const target of [700, 520, 400]) {
    await chatAt(target);
    await showTopBar(`the session: a ${target}px chat column`);
  }
  // The settings page, from the gear: Esc closes it, a choice keeps it.
  await page.locator(".settings-toggle").click();
  await page.locator(".settings-dialog").waitFor();
  await page.waitForTimeout(200);
  await show("settings", ".settings-dialog");
  await showChips("a dialog open: the chips", ".modal-backdrop");
  await page.locator(".settings-dialog .segment", { hasText: "Verbose" }).click();
  await page.locator(".settings-dialog .segment.on", { hasText: "Verbose" }).waitFor();
  console.log(`a choice keeps it open: ${await page.locator(".settings-dialog").isVisible()}`);
  await page.locator(".settings-dialog .segment", { hasText: "Normal" }).click();
  // The sections down the side: a click shows one; on its tab, the arrows
  // move to the next and Home and End to the ends, the focus with them.
  const settingsAt = async label => {
    const at = await page.evaluate(() => ({
      tab: document.querySelector(".settings-tab[aria-selected=true]")?.innerText.replace(/\s+/g, " "),
      focus: document.activeElement?.closest(".settings-tab")?.innerText.replace(/\s+/g, " ") ?? document.activeElement?.tagName,
      panel: document.querySelector(".settings-panel-head h3")?.innerText,
      labelled: document.getElementById("settings-panel")?.getAttribute("aria-labelledby"),
    }));
    console.log(`${label}: tab ${at.tab}, focus ${at.focus}, showing ${at.panel} (labelled by ${at.labelled})`);
  };
  section("settings: the sections");
  await page.locator(".settings-tab", { hasText: "Behaviour" }).click();
  await page.locator(".settings-panel-behaviour").waitFor();
  await settingsAt("clicked Behaviour");
  await page.keyboard.press("ArrowDown");
  await page.locator(".settings-panel-models").waitFor();
  await settingsAt("ArrowDown");
  await page.keyboard.press("End");
  await page.locator(".settings-panel-account").waitFor();
  await settingsAt("End");
  await page.keyboard.press("ArrowDown");
  await page.locator(".settings-panel-appearance").waitFor();
  await settingsAt("ArrowDown wraps");
  await page.locator(".settings-tab", { hasText: "Git hosting" }).click();
  await page.locator(".settings-panel-git").waitFor();
  await screenshot("settings: git hosting");
  const settingsFits = () => page.evaluate(() => {
    const d = document.querySelector(".settings-dialog").getBoundingClientRect();
    const nav = document.querySelector(".settings-nav");
    return `dialog ${Math.round(d.width)}x${Math.round(d.height)}, in the window: ${d.left >= 0 && d.right <= innerWidth && d.top >= 0 && d.bottom <= innerHeight}; \
sections ${getComputedStyle(nav).flexDirection === "row" ? "across the top" : "down the side"}; panel fits: ${[...document.querySelectorAll(".settings-panel")].every(p => p.scrollWidth <= p.clientWidth)}`;
  });
  console.log(`desktop: ${await settingsFits()}`);
  await page.setViewportSize({ width: 390, height: 844 });
  await page.locator(".settings-tab", { hasText: "Behaviour" }).click();
  await page.locator(".settings-panel-behaviour").waitFor();
  console.log(`phone: ${await settingsFits()}`);
  await screenshot("settings: a phone");
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.locator(".settings-tab", { hasText: "Appearance" }).click();
  await page.keyboard.press("Escape");
  await page.waitForSelector(".settings-dialog", { state: "detached" });
  await page.waitForFunction(() => document.activeElement?.id === "editor", null, { timeout: 2000 }).catch(() => {});
  console.log(`Esc closes it, the editor has the focus: ${await page.evaluate(() => document.activeElement?.id === "editor")}`);
  // Without the sidebar, the session's bar over the chat, with its button.
  await page.keyboard.press("Control+b");
  await page.waitForFunction(() => !document.querySelector(".app.sidebar-open"));
  for (const target of [900, 600, 400]) {
    await chatAt(target);
    await showTopBar(`the session's bar: a ${target}px chat column without the sidebar`);
  }
  await page.keyboard.press("Control+b");
  await page.waitForFunction(() => document.querySelector(".app.sidebar-open"));
  await handle("files").dblclick();
  await page.keyboard.press("Control+Shift+F");
  await page.waitForSelector(".files-panel", { state: "detached" });

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
