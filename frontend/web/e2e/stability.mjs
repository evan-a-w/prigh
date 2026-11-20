// Nothing flickers: what did not change keeps its DOM nodes and its place.
// While a reply streams, then ends, and while the page idles, the earlier
// messages, the session's card or bar and the composer are not redrawn, the transcript
// does not shift, a streamed reply keeps the nodes (and the selection) it
// was drawn with, a key typed draws no message again, and the editor grows
// with its text in the frame that shows the text, and the composer's host,
// model and context ring keep their places in every frame as turns start
// and end, on a desktop and on a phone. Run by prigh_web.sh; prints normalised results (the expected
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
  if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text()))
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
        if (where.some(n => n.closest("#chat, .chat-chrome"))) window.__shifts.push(entry.value);
        if (where.some(n => n.closest(".composer-controls, .context-ring-anchor"))) window.__statusShifts?.push(entry.value);
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
    // Where the status's items are (the context ring from the composer's
    // left edge; the host and the model from its right edge), and what the
    // agent is doing (the ring's status for screen readers).
    const ring = document.querySelector(".context-ring-anchor");
    const boxRect = document.querySelector(".composer-box")?.getBoundingClientRect();
    const left = boxRect?.left ?? 0, right = boxRect?.right ?? 0;
    if (ring && window.__status)
      window.__status.push({
        activity: ring.querySelector("[role=status]")?.textContent.trim(),
        at: [["ring", ".composer .context-ring", true], ["host", ".composer .host-chip"], ["model", ".composer .model-chip"]]
          .map(([name, sel, fromLeft]) => {
            const e = document.querySelector(sel)?.getBoundingClientRect();
            return e ? `${name}@${Math.round(fromLeft ? e.left - left : right - e.right)}` : `${name} missing`;
          })
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
    document.querySelector(".chat-chrome"),
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

// The editor is as tall as its lines with no scrollbar, a line of text
// typed, pasted or left after deleting the others (a line is 22.5px on a
// desktop: its scrollHeight, a whole number, can be a fraction short, which
// showed a scrollbar), and with its text wrapped anew when its width
// changes; it scrolls only once at its max-height.
const editorHeight = async label => {
  const state = () => page.evaluate(() => {
    const e = document.getElementById("editor");
    const s = getComputedStyle(e);
    const lines = Math.round((e.scrollHeight - parseFloat(s.paddingTop) - parseFloat(s.paddingBottom)) / parseFloat(s.lineHeight));
    const fits = Math.abs(e.offsetHeight - (lines * parseFloat(s.lineHeight) + parseFloat(s.paddingTop) + parseFloat(s.paddingBottom))) <= 1;
    const capped = Math.abs(e.offsetHeight - parseFloat(s.maxHeight)) <= 1;
    const overflow = e.scrollHeight > e.clientHeight + 1;
    const scrollbar = e.offsetWidth > e.clientWidth || (s.overflowY !== "hidden" && e.scrollHeight > e.clientHeight);
    return `${lines} line${lines === 1 ? "" : "s"}, ${capped ? "at its max-height" : fits ? "as tall" : `${e.offsetHeight}px tall`}`
      + `, ${overflow ? "overflows" : "no overflow"}, ${scrollbar ? "a scrollbar" : "no scrollbar"}`;
  });
  const settle = () => page.evaluate(() => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r))));
  const clear = async () => {
    await page.keyboard.press("ControlOrMeta+a");
    await page.keyboard.press("Backspace");
    await settle();
  };
  section(label);
  await composer().click();
  await clear();
  console.log(`empty: ${await state()}`);
  await page.keyboard.type("currently my attic is hosted with");
  await settle();
  console.log(`a line typed: ${await state()}`);
  await screenshot(`${label} a line typed`);
  await clear();
  await page.keyboard.insertText("currently my attic is hosted with");
  await settle();
  console.log(`a line pasted: ${await state()}`);
  await clear();
  await page.keyboard.type("one");
  for (const line of ["two", "three"]) {
    await page.keyboard.press("Shift+Enter");
    await page.keyboard.type(line);
  }
  await settle();
  console.log(`three lines typed: ${await state()}`);
  for (let i = 0; i < "\ntwo\nthree".length; i++) await page.keyboard.press("Backspace");
  await settle();
  console.log(`deleted back to one: ${await state()}`);
  await screenshot(`${label} deleted back to one`);
  await clear();
  await page.keyboard.insertText("words ".repeat(30).trim());
  await settle();
  const wide = await state();
  await page.evaluate(() => { document.querySelector(".composer-stack").style.width = "60%"; });
  await settle();
  console.log(`a paragraph, narrower: more lines: ${await state() !== wide}, ${(await state()).replace(/^\d+ lines?, /, "")}`);
  await page.evaluate(() => { document.querySelector(".composer-stack").style.width = ""; });
  await settle();
  console.log(`and wide again: as before: ${await state() === wide}`);
  await clear();
  await page.keyboard.insertText(Array.from({ length: 100 }, (_, i) => `line ${i + 1}`).join("\n"));
  await settle();
  console.log(`a hundred lines: ${(await state()).replace(/^\d+ lines, /, "")}`);
  const scrolled = await page.evaluate(() => {
    const e = document.getElementById("editor");
    e.scrollTop = 0;
    e.scrollTop = 1e6;
    return e.scrollTop > 0 && Math.abs(e.scrollTop + e.clientHeight - e.scrollHeight) <= 1;
  });
  console.log(`  scrolls to its last line: ${scrolled}`);
  await screenshot(`${label} a hundred lines`);
  await clear();
  console.log(`cleared: ${await state()}`);
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

  // Keys typed: the messages (View_cache's lazy nodes, app/vdom_thunk.js)
  // are neither drawn nor compared again.
  await page.evaluate(() => {
    window.__thunks = { reused: 0, drawn: 0 };
    const render = VdomThunk.prototype.render;
    VdomThunk.prototype.render = function (prev) {
      const vnode = render.call(this, prev);
      if (prev && vnode === prev.vnode) window.__thunks.reused++;
      else window.__thunks.drawn++;
      return vnode;
    };
  });
  // Nor is anything outside the composer: the sidebar, the transcript and
  // the status are not touched (View's parts).
  await page.evaluate(() => {
    window.__touched = new Set();
    for (const sel of ["#sidebar", ".entries", ".context-ring-anchor"]) {
      const els = document.querySelectorAll(sel);
      if (!els.length) window.__touched.add(`${sel} missing`);
      for (const el of els)
        new MutationObserver(() => window.__touched.add(sel))
          .observe(el, { subtree: true, childList: true, attributes: true, characterData: true });
    }
  });
  await composer().click();
  await page.keyboard.type("hello");
  await page.waitForFunction(() => window.__thunks.reused > 0);
  await page.waitForTimeout(200);
  section("keys typed");
  const thunks = await page.evaluate(() => window.__thunks);
  console.log(`messages reused: ${thunks.reused > 0}`);
  console.log(`messages drawn again: ${thunks.drawn}`);
  const touched = await page.evaluate(() => [...window.__touched]);
  console.log(`sidebar, transcript and status touched: ${touched.length ? touched.join(", ") : "none"}`);
  await composer().fill("");

  // Idle: the page renders at most once in a few seconds (its clock ticks
  // every 30 s), and that render changes nothing on the page.
  await page.evaluate(() => {
    window.__renders = { count: 0, last: performance.now() };
    let pending = false;
    const render = VdomThunk.prototype.render;
    VdomThunk.prototype.render = function (prev) {
      window.__renders.last = performance.now();
      if (!pending) {
        pending = true;
        window.__renders.count++;
        queueMicrotask(() => { pending = false; });
      }
      return render.call(this, prev);
    };
  });
  await page.waitForFunction(() => performance.now() - window.__renders.last > 500, null, { polling: 100 });
  await page.evaluate(() => {
    window.__renders.count = 0;
    window.__idleChanges = 0;
    new MutationObserver(r => { window.__idleChanges += r.length; })
      .observe(document.body, { subtree: true, childList: true, attributes: true, characterData: true });
  });
  // The window watched: a fixed time is the point here.
  await page.waitForTimeout(3000);
  section("idle for 3 s");
  const quiet = await page.evaluate(() => ({ renders: window.__renders.count, changes: window.__idleChanges }));
  console.log(`renders: ${quiet.renders <= 1 ? "at most one" : quiet.renders}`);
  console.log(`changes to the page: ${quiet.changes}`);

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
  // is doing changes (the ring turns), but the host, the model and the ring
  // stay where they were in every frame.
  const turn = async (label, reply) => {
    await page.evaluate(() => { window.__status = []; window.__statusShifts = []; });
    await send("check it");
    await page.locator(".context-ring-anchor [role=status]", { hasText: "Running tools" }).waitFor({ state: "attached" });
    await send("and the rest");
    await page.locator(".queued-tag").first().waitFor();
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
  await turn("a turn starts and ends: the status at the composer", "Checked.");
  await editorHeight("the editor's height");
  // As in the desktop app zoomed out to 90% (its page zoom; CSS zoom here).
  await page.evaluate(() => { document.documentElement.style.zoom = "0.9"; });
  await editorHeight("the editor's height at 90%");
  await page.evaluate(() => { document.documentElement.style.zoom = ""; });
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => document.querySelector(".app.narrow:not(.sidebar-open)"));
  await page.waitForTimeout(400);
  await turn("a turn starts and ends on a phone: the status at the composer", "Checked again.");
  await page.setViewportSize({ width: 320, height: 640 });
  await page.waitForFunction(() => innerWidth === 320);
  await editorHeight("the editor's height on a small phone");

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
