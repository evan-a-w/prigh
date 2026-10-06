// Drives a real browser against `prigh serve -prigh-web ... -faux-script`
// (see prigh_web.sh) and prints normalised snapshots of what the page shows.
// It owns the backend so that it can restart it. The expected output lives
// in prigh_web.expected.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE (the built site),
// TEST_DIR (scratch: home/, cwd/, the faux scripts), TOKEN, and optionally
// SHOTS (a directory for screenshots of each step).
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { hostname } from "node:os";
import { join } from "node:path";
import { png, pngSize } from "./png.mjs";

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
writeFileSync(join(dir, "script.json"), JSON.stringify(script));
writeFileSync(join(dir, "script2.json"), JSON.stringify(script2));

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
      ["serve", "-prigh-web", listen, "-prigh-web-root", site, "-faux-script", scriptFile,
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
const errors = [];
page.on("console", message => {
  const text = message.text();
  // The backend going away (step 7) is logged by the browser.
  if (message.type() === "error" && !/favicon\.ico|WebSocket|ERR_CONNECTION_REFUSED/.test(text)) {
    errors.push(`console: ${text}`);
  }
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .replaceAll(cwd, "<CWD>")
  .replace(/[0-9a-f]{16}/g, "<ID>")
  .replace(/ws:\/\/127\.0\.0\.1:\d+/g, "ws://127.0.0.1:<PORT>")
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

// Tool calls and thinking are collapsible: open them so that their text shows.
const show = async (label, selector) => {
  await page.evaluate(() => document.querySelectorAll("details:not(.image)").forEach(d => { d.open = true; }));
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await screenshot(label);
};

// The top bar's text, with each select showing its chosen option.
const showHeader = async label => {
  section(label);
  console.log(clean(await page.evaluate(() =>
    [...document.querySelectorAll("header *")]
      .filter(e => e.tagName === "SELECT" || (e.children.length === 0 && e.tagName !== "OPTION"))
      .map(e => (e.tagName === "SELECT" ? `[${e.selectedOptions[0]?.text ?? ""}]` : e.textContent))
      .join("\n"))));
};

// From the last prompt to the end of the chat.
const showTurn = async label => {
  await page.evaluate(() => document.querySelectorAll("details:not(.image)").forEach(d => { d.open = true; }));
  section(label);
  console.log(clean(await page.evaluate(() => {
    const prompts = document.querySelectorAll(".chat .msg.user");
    const turn = [];
    for (let e = prompts[prompts.length - 1]; e; e = e.nextElementSibling) turn.push(e.innerText);
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

// The agents panel, with its elapsed times replaced.
const showPanel = async label => {
  section(label);
  console.log(clean(await page.locator(".agents-panel").innerText()).replace(/\b\d+(m \d\d)?s\b/g, "<T>"));
  await page.waitForTimeout(200); // its slide in
  await screenshot(label);
};

const bodyHas = text => page.waitForFunction(t => document.body.innerText.includes(t), text);
const bodyLacks = text => page.waitForFunction(t => !document.body.innerText.includes(t), text);
const composer = () => page.locator(".composer textarea");
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
  await show("signed in: sidebar", ".sidebar");
  await show("signed in: chat", ".chat");
  await showHeader("signed in: top bar");

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

  // 2. A scripted run with a bash tool call and its output.
  await send("count some lines");
  await bodyHas("three lines");
  await idle();
  await page.locator(".tool").first().waitFor();
  await showTurn("bash run");
  await showHeader("top bar after a run");

  // 3. The model reads a real PNG: the tool result shows it.
  await send("what does flag.png look like?");
  await bodyHas("a red flag with a blue stripe");
  await idle();
  await showTurn("read an image");
  await showImages("the read tool's image", page.locator(".tool img.thumb"));

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
  await page.locator(".sidebar").getByRole("button", { name: /new session/i }).click();
  await bodyLacks("three lines");
  await show("new session", ".chat");
  await send("hello again");
  await bodyHas("second session reply");
  await idle();
  console.log(`new session in the url: ${sessionId() !== first}`);
  await page.waitForFunction(() => document.querySelectorAll(".sidebar .session").length === 2);
  await show("two sessions", ".sidebar");
  await page.locator(".sidebar .session", { hasText: "count some lines" }).click();
  await bodyHas("three lines");
  await bodyLacks("second session reply");
  console.log(`switched back: ${sessionId() === first}`);
  await show("switched back", ".chat .msg.user");

  // 7. The backend restarts: the page says so, reconnects to the same
  //    session and carries on.
  await stopServer();
  await bodyHas("reconnecting");
  section("backend gone");
  console.log(clean(await page.locator("body").innerText()).split("\n").filter(l => /connect/i.test(l)).join("\n"));
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
  await page.locator(".status .background").click();
  await page.waitForFunction(() =>
    document.querySelectorAll(".agents-row").length === 3 && !document.querySelector(".agents-row.running"));
  await showPanel("agents panel");
  await page.keyboard.press("Alt+2");
  await page.waitForSelector(".agents-detail .agents-transcript .entries");
  await showPanel("a nested subagent");
  await page.locator(".detail-actions .btn", { hasText: "Show in chat" }).click();
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
  const edge = await page.locator(".terminal-resize").boundingBox();
  await page.mouse.move(edge.x + 300, edge.y + edge.height / 2);
  await page.mouse.down();
  await page.mouse.move(edge.x + 300, edge.y - 160, { steps: 8 });
  await page.mouse.up();
  await page.locator(".terminal-panel .xterm").click();
  await page.waitForFunction(n => document.querySelectorAll(".terminal-panel .xterm-rows > div").length > n, before);
  const after = await shellRows();
  console.log(`taller after dragging: ${after > before}; the panel's rows: ${after === (await terminalRows()).length}`);
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
  console.log(`reloaded: the same height: ${await page.evaluate(() =>
    getComputedStyle(document.querySelector(".terminal-panel")).getPropertyValue("--terminal-height") !== "")}`);
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

  // The layout at phone size.
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.waitForTimeout(400); // the drawer's slide
  await screenshot("phone");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  await stopServer();
}
