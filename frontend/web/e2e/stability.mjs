// Nothing flickers: what did not change keeps its DOM nodes and its place.
// While a reply streams, then ends, and while the page idles, the earlier
// messages, the session's card or bar and the composer are not redrawn, the transcript
// does not shift, a streamed reply keeps the nodes (and the selection) it
// was drawn with, and the editor grows with its text in the frame that shows
// the text, and the session's status keeps its place in every frame as turns
// start and end, in the sidebar on a desktop and in the bar on a phone. Run by prigh_web.sh; prints normalised results (the expected
// output is in ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: stability.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "stability");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });

const long = Array.from({ length: 10 }, (_, i) =>
  `Paragraph ${i + 1} with **bold** and \`code\`, long enough to wrap.\n\n`
  + (i % 3 === 0 ? `\`\`\`ocaml\nlet x = ${i}\n\`\`\`\n\n` : "")).join("");
writeFileSync(join(dir, "script.json"), JSON.stringify([
  { text: "let me look", tool_calls: [{ id: "c1", name: "bash", arguments: { command: "printf 'one\\ntwo\\n'" } }] },
  { text: "Done:\n\n- one\n- two" },
  { text: "Second answer." },
  // About 3s of streaming.
  { thinking: "thinking it over", text: long, chunks: 150 },
  { text: "Noted." },
  // Two turns whose activity changes (Sending, Running tools, Writing,
  // Ready), each with a steer queued while it runs.
  { text: "checking", tool_calls: [{ id: "c2", name: "bash", arguments: { command: "sleep 1.5" } }] },
  { text: "Checked." },
  { text: "checking", tool_calls: [{ id: "c3", name: "bash", arguments: { command: "sleep 1.5" } }] },
  { text: "Checked again." },
]));

let server;
const startServer = listen =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-web", listen, "-web-root", site, "-faux-script", join(dir, "script.json"),
        "-token", token, "-cwd", cwd],
      { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
    );
    let err = "";
    const timer = setTimeout(() => reject(new Error(`backend did not start:\n${err}`)), 30000);
    server.stderr.on("data", data => {
      err += data;
      const m = err.match(/^prigh: prigh-web on (\S+)$/m);
      if (m) { clearTimeout(timer); resolve(m[1]); }
    });
    server.on("exit", code => { clearTimeout(timer); reject(new Error(`backend exited (${code}):\n${err}`)); });
  });
const stopServer = () =>
  new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null || server.signalCode !== null) return resolve();
    const timer = setTimeout(() => { server.kill("SIGKILL"); }, 5000);
    server.on("exit", () => { clearTimeout(timer); resolve(); });
    server.kill();
  });

const url = await startServer("127.0.0.1:0");

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
context.setDefaultTimeout(20000);
const page = await context.newPage();
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico/.test(message.text()))
    errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

// From the start: layout shifts (Chromium reports them; elsewhere there are
// none to see), and the page's state at the end of every frame, after
// Bonsai's own animation frame callback (one requested from a timeout runs
// after the one Bonsai requested in its frame), which is what is painted.
await page.addInitScript(() => {
  window.__shifts = [];
  try {
    new PerformanceObserver(list => {
      for (const entry of list.getEntries()) {
        const where = (entry.sources || []).map(s => s.node).filter(n => n && n.nodeType === 1);
        if (where.some(n => n.closest("#chat, .session-card, .session-bar"))) window.__shifts.push(entry.value);
        if (where.some(n => n.closest(":is(.session-card, .session-bar) .status"))) window.__statusShifts?.push(entry.value);
      }
    }).observe({ type: "layout-shift" });
  } catch {}
  window.__frames = [];
  const sample = () => {
    const editor = document.getElementById("editor");
    if (editor)
      window.__frames.push({
        value: editor.value,
        overflow: editor.scrollHeight - editor.clientHeight,
        height: editor.offsetHeight,
      });
    // Where the status's items are (the card has them on lines of their
    // own, the bar directly).
    const status = document.querySelector(":is(.session-card, .session-bar) .status");
    if (status && window.__status)
      window.__status.push({
        activity: status.querySelector(".activity")?.textContent.trim(),
        at: [...status.querySelectorAll(":is(.status, .card-line) > :is(.context, .tokens, .cost, .activity)")]
          .filter(e => getComputedStyle(e).display !== "none")
          .map(e => {
            const r = e.getBoundingClientRect();
            const name = ["context", "tokens", "cost"].find(c => e.classList.contains(c)) ?? "activity";
            return `${name}@${Math.round(r.left)}`;
          })
          .concat([...status.querySelectorAll(".activity > .label")]
            .filter(e => e.offsetWidth > 0)
            .map(e => `label@${Math.round(e.getBoundingClientRect().left)}`))
          .join(" "),
      });
    setTimeout(() => requestAnimationFrame(sample), 0);
  };
  requestAnimationFrame(sample);
});

const section = label => console.log(`=== ${engineName}: stability: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-stability-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const composer = () => page.locator(".composer textarea");
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const bodyHas = text => page.waitForFunction(t => document.body.innerText.includes(t), text);
const send = async text => {
  await composer().fill(text);
  await page.locator(".btn.send").click();
};

// Remembers the nodes on show that must stay, and counts the nodes removed
// from inside them from now on.
const watch = () => page.evaluate(() => {
  const stable = [
    ...document.querySelectorAll("#chat .entries > *"),
    ...document.querySelectorAll("#chat .entries .md-part, #chat .entries p, #chat .tool"),
    document.querySelector(".session-card, .session-bar"),
    document.querySelector(".composer"),
    document.getElementById("editor"),
    document.querySelector(".sidebar"),
  ];
  window.__stable = stable;
  window.__removed = [];
  window.__observer?.disconnect();
  window.__observer = new MutationObserver(records => {
    for (const record of records)
      for (const node of record.removedNodes)
        if (node.nodeType === 1 && stable.some(s => s === node || node.contains(s)))
          window.__removed.push(node.tagName.toLowerCase() + "." + [...node.classList].join("."));
  });
  window.__observer.observe(document.body, { childList: true, subtree: true });
  window.__shifts = [];
});
const report = async label => {
  const r = await page.evaluate(() => ({
    nodes: window.__stable.length,
    kept: window.__stable.every(n => n.isConnected),
    removed: window.__removed,
    shift: window.__shifts.reduce((a, b) => a + b, 0),
  }));
  section(label);
  console.log(`nodes watched: ${r.nodes > 10}`);
  console.log(`all kept: ${r.kept}`);
  console.log(`removed: ${r.removed.length ? r.removed.join(", ") : "none"}`);
  console.log(`layout shift in the transcript and the session's status: ${r.shift < 0.005 ? "none" : r.shift.toFixed(4)}`);
  await screenshot(label);
};

try {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  await page.locator(".chat .empty").waitFor();

  // Two turns first: the jump rail beside the chat is on show from the
  // second.
  await send("look");
  await bodyHas("Done:");
  await idle();
  await send("again");
  await bodyHas("Second answer.");
  await idle();
  await page.waitForTimeout(300);

  await watch();
  await page.waitForTimeout(3000);
  await report("idle");

  await watch();
  await send("a long one");
  // Once some of the reply has streamed: its first paragraph, selected.
  await page.waitForFunction(() => document.querySelectorAll("#chat .streaming > .markdown > .md-part").length >= 4);
  const during = await page.evaluate(() => {
    const reply = document.querySelector("#chat .streaming");
    window.__parts = [...reply.querySelectorAll(":scope > .markdown > .md-part")].slice(0, 3);
    const p = reply.querySelector(":scope > .markdown > .md-part > p");
    const range = document.createRange();
    range.selectNodeContents(p);
    getSelection().removeAllRanges();
    getSelection().addRange(range);
    return getSelection().toString();
  });
  await idle();
  await bodyHas("Paragraph 10");
  await page.waitForTimeout(300);
  await report("a reply streamed");
  section("the streamed reply once ended");
  console.log(`its parts kept: ${await page.evaluate(() => window.__parts.every(p => p.isConnected))}`);
  const after = await page.evaluate(() => getSelection().toString());
  console.log(`selection kept: ${after === during && during.startsWith("Paragraph 1 with bold")}`);

  // Lines typed and sent: the editor shows them all in every frame, and is
  // back to one line in the frame that clears it.
  await page.evaluate(() => { window.__frames = []; });
  await composer().click();
  for (const line of ["one", "two", "three", "four"]) {
    await page.keyboard.type(line);
    await page.keyboard.press("Shift+Enter");
    await page.waitForTimeout(150);
  }
  await page.keyboard.type("five");
  await page.waitForTimeout(200);
  await page.keyboard.press("Enter");
  await bodyHas("Noted.");
  await idle();
  await page.waitForTimeout(200);
  const frames = await page.evaluate(() => window.__frames);
  const oneLine = Math.min(...frames.map(f => f.height));
  section("the editor");
  console.log(`frames sampled: ${frames.length > 20}`);
  console.log(`frames with lines out of view: ${frames.filter(f => f.overflow > 1).length}`);
  console.log(`frames empty but still tall: ${frames.filter(f => f.value === "" && f.height > oneLine).length}`);

  // A turn starts, runs a tool, has a steer queued, and ends: what the agent
  // is doing changes and what applies comes and goes, but the items of the
  // status start where they did in every frame.
  const turn = async (label, reply) => {
    await page.evaluate(() => { window.__status = []; window.__statusShifts = []; });
    await send("check it");
    await page.locator(":is(.session-card, .session-bar) .status > *", { hasText: "Running tools" }).waitFor();
    await send("and the rest");
    await page.locator(".status-extras .queued").waitFor();
    await bodyHas(reply);
    await idle();
    await page.waitForTimeout(300);
    const r = await page.evaluate(() => {
      const r = { frames: window.__status, shift: window.__statusShifts.reduce((a, b) => a + b, 0) };
      window.__status = undefined;
      return r;
    });
    const activities = new Set(r.frames.map(f => f.activity));
    const places = [...new Set(r.frames.map(f => f.at))];
    section(label);
    console.log(`activities shown: ${activities.size >= 3 ? "several" : [...activities].join(", ")}, ending ${r.frames.at(-1).activity}`);
    console.log(`places of the status's items: ${places.length === 1 ? "one" : places.join(" | ")}`);
    console.log(`layout shift in the status: ${r.shift < 0.0001 ? "none" : r.shift.toFixed(4)}`);
    await screenshot(label);
  };
  await turn("a turn starts and ends: the status in the sidebar", "Checked.");
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.waitForTimeout(400);
  await turn("a turn starts and ends on a phone: the status in the bar", "Checked again.");

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
