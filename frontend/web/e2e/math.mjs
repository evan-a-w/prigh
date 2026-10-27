// TeX math in replies (KaTeX, loaded the first time math shows): inline and
// display math, a matrix and an aligned environment, TeX KaTeX rejects
// (shown as written, tinted, with KaTeX's message as its title), untrusted
// commands, prose with prices and shell variables left alone, and code
// left as code. The math's colour follows the text's in the original, dark
// and light themes; on a phone, wide display math scrolls inside the
// column. Selecting rendered math and copying gives its TeX (copy-tex); the
// message menu's Copy, the reply as written. A reply streamed in later is
// drawn too.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
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
if (!engine) throw new Error("usage: math.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "math");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
// Nothing may wait without a deadline.
setTimeout(() => { console.error("math.mjs: timed out"); process.exit(2); }, 240000).unref();

const tick = "`";
const reply = String.raw`Euler's identity $e^{i\pi} + 1 = 0$, and Pythagoras \(a^2 + b^2 = c^2\).

It costs $5 and $10 (US$3 abroad); set $HOME and $PATH, or run ${tick}echo $x$${tick}.

$$
\int_0^\infty e^{-x^2}\,dx = \frac{\sqrt{\pi}}{2}
$$

A matrix:

\[
A = \begin{pmatrix} 1 & 2 & 3 \\ 4 & 5 & 6 \\ 7 & 8 & 9 \end{pmatrix}
\]

${tick.repeat(3)}math
\begin{aligned}
f(x) &= (x + 1)^2 \\
     &= x^2 + 2x + 1
\end{aligned}
${tick.repeat(3)}

Wide: $$\sum_{k=1}^{n} \frac{k^2 + 3k + 2}{(k + 1)(k + 2)(k + 3)(k + 4)} + \sum_{j=1}^{m} \binom{m}{j} x^j y^{m - j} = \prod_{i=1}^{n} (1 + x_i)$$

Broken: $\frac{1}{\undefinedmacro}$ and $\href{javascript:alert(1)}{click}$.`;

const id = "3a7e3a7e3a7e3a7e";
{
  let seq = 0;
  let parent = null;
  let at = Math.floor(Date.now() / 1000) - 3600;
  const created = new Date(at * 1000).toISOString().replace("T", " ").replace(/\.(\d+)Z$/, ".$1000Z");
  const lines = [JSON.stringify(["Header", { id, cwd, created_at: created }])];
  const entry = payload => {
    const eid = `${id.slice(0, 8)}${String(seq++).padStart(8, "0")}`;
    lines.push(JSON.stringify(["Entry", { id: eid, parent, at: (at += 7), payload }]));
    parent = eid;
  };
  entry(["Name", { name: "some math" }]);
  entry(["Message", ["User", { text: "Show me some math." }]]);
  entry(["Message", ["Assistant", {
    content: [["Text", reply]],
    stop_reason: ["End_turn"], usage: { input: 1200, output: 300, cache_read: 0 }, model: "faux",
  }]]);
  writeFileSync(join(sessions, `20261001-100000000_${id}.jsonl`), lines.join("\n") + "\n");
}
writeFileSync(join(dir, "script.json"), JSON.stringify([
  { text: String.raw`Streamed: $\alpha + \beta = \gamma$, then $$\sqrt{2} \approx 1.414$$ done.` },
]));

let server;
const url = await new Promise((resolve, reject) => {
  server = spawn(
    backend,
    ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
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

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const newPage = async options => {
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  return page;
};
let page = await newPage({ viewport: { width: 1280, height: 900 } });

const section = label => console.log(`=== ${engineName}: math: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-math-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
// A screenshot of the reply itself, scrolled to [selector].
const replyShot = async (label, selector) => {
  if (!process.env.SHOTS) return;
  await page.locator(selector).first().scrollIntoViewIfNeeded();
  await screenshot(label);
};

const composer = () => page.locator(".composer textarea");
const command = async text => {
  await page.waitForSelector(".modal-backdrop", { state: "detached" });
  await composer().fill(text);
  await composer().press("Enter");
  await page.waitForFunction(() => document.querySelector(".composer textarea").value === "");
};
const theReply = () => page.locator("#chat .msg.assistant", { hasText: "Euler's identity" });
// Every math element drawn: KaTeX's output (its TeX, from the MathML
// annotation) or the TeX shown as written, and what else the reply shows.
const drawn = () => page.waitForFunction(() =>
  document.querySelectorAll("#chat .math").length > 0
  && document.querySelectorAll("#chat .math.math-pending").length === 0
  && [...document.fonts].some(f => f.family.replace(/"/g, "") === "KaTeX_Main" && f.status === "loaded"));
const maths = () => page.evaluate(() =>
  [...document.querySelectorAll("#chat .msg.assistant .math")].map(el => {
    const kind = el.classList.contains("math-display") ? "display" : "inline";
    if (el.classList.contains("math-error"))
      return `${kind} error: ${el.textContent} (title: ${el.title.replace(/\s+/g, " ")})`;
    const tex = el.querySelector("annotation[encoding='application/x-tex']")?.textContent;
    const links = el.querySelectorAll("a").length;
    // What KaTeX does not allow (trust is off) it draws as its source, in red.
    const refused = [...el.querySelectorAll(".katex-html *")].some(e => e.title || e.style.color);
    return `${kind}${el.querySelector(".katex-display") ? " katex-display" : ""}: ${tex?.replace(/\s+/g, " ").trim()}`
      + `; links: ${links}${refused ? "; refused" : ""}`;
  }).join("\n"));
// The math's colour against the text's, and the error's tint.
const colours = () => page.evaluate(() => {
  const text = getComputedStyle(document.querySelector("#chat .msg.assistant .markdown p")).color;
  const katex = [...document.querySelectorAll("#chat .msg.assistant .katex")].map(k => getComputedStyle(k).color);
  const line = getComputedStyle(document.querySelector("#chat .msg.assistant .frac-line")).borderBottomColor;
  const error = getComputedStyle(document.querySelector("#chat .msg.assistant .math-error")).color;
  return `math in the text's colour: ${katex.every(c => c === text)}; fraction rule too: ${line === text}; `
    + `error tinted: ${error !== text}`;
});
// Display math wider than its column scrolls inside it; nothing else does.
const widths = () => page.evaluate(() => {
  const md = document.querySelector("#chat .msg.assistant .markdown");
  const wide = [...md.querySelectorAll(".math-display")].filter(m => m.scrollWidth > m.clientWidth + 1);
  return `markdown fits: ${md.scrollWidth <= md.clientWidth + 1}; page fits: ${document.documentElement.scrollWidth <= window.innerWidth}; `
    + `scrolling display math: ${wide.length}, ${wide.map(m => getComputedStyle(m).overflowX).join(" ")}`;
});

try {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  await page.locator(`[data-session="${id}"]`).click();
  await theReply().waitFor();
  await drawn();

  section("drawn");
  console.log(await maths());
  section("prose and code");
  console.log(await theReply().evaluate(el => {
    const p = [...el.querySelectorAll(".markdown p")].find(p => p.innerText.includes("costs"));
    return `${p.innerText}\nmath in it: ${p.querySelectorAll(".math").length}, code: ${p.querySelector("code").innerText}`;
  }));
  section("loaded once");
  console.log(await page.evaluate(() =>
    ["katex.min.js", "copy-tex.min.js", "katex.min.css"].map(f =>
      `${f}: ${document.querySelectorAll(`script[src="${f}"], link[href="${f}"]`).length}`).join(", ")));
  section("original");
  console.log(await colours());
  await replyShot("original inline", ".msg.assistant .math-inline");
  await replyShot("original display", ".msg.assistant .math-display >> nth=2");
  await replyShot("original errors", ".msg.assistant .math-error");

  for (const theme of ["dark", "light"]) {
    await command(`/theme ${theme}`);
    await page.waitForFunction(t => document.documentElement.dataset.theme === t, theme);
    section(theme);
    console.log(await colours());
    await replyShot(`${theme} inline`, ".msg.assistant .math-inline");
    await replyShot(`${theme} display`, ".msg.assistant .math-display >> nth=2");
  }
  await command("/theme original");

  // Selecting the inline math (and the words around it) and copying: the
  // TeX between dollars, as copy-tex writes it.
  section("copy a selection");
  await page.evaluate(() => {
    const p = document.querySelector("#chat .msg.assistant .markdown p");
    const range = document.createRange();
    range.selectNodeContents(p);
    const selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
    // After copy-tex (on the document), what it put on the clipboard.
    window.addEventListener("copy", ev => {
      window.copiedSelection = ev.clipboardData.getData("text/plain");
      ev.preventDefault();
    }, { once: true });
  });
  await page.keyboard.press("ControlOrMeta+c");
  await page.waitForFunction(() => window.copiedSelection !== undefined);
  console.log(await page.evaluate(() => {
    window.getSelection().removeAllRanges();
    return window.copiedSelection;
  }));

  // The message menu's Copy: the reply as written (the clipboard stubbed:
  // headless browsers have none).
  section("copy the message");
  await page.evaluate(() => {
    navigator.clipboard.writeText = async text => { window.copied = text; };
  });
  await theReply().click({ button: "right", position: { x: 40, y: 8 } });
  await page.locator(".context-menu.placed [data-item=copy_message]").click();
  await page.waitForFunction(() => window.copied !== undefined);
  console.log(`as written: ${await page.evaluate(r => window.copied === r, reply)}`);

  // A reply streamed in: drawn once it arrives.
  section("streamed");
  await composer().fill("And another?");
  await composer().press("Enter");
  const streamed = page.locator("#chat .msg.assistant", { hasText: "Streamed:" });
  await streamed.locator(".katex-display").waitFor();
  await page.waitForFunction(() => !document.querySelector("#chat .msg.assistant.streaming"));
  console.log(await streamed.evaluate(el =>
    [...el.querySelectorAll(".math")].map(m =>
      `${m.className}: ${m.querySelector("annotation")?.textContent}`).join("\n")));
  await replyShot("streamed", "#chat .msg.assistant >> nth=-1");

  // A phone: the column is narrow; wide display math scrolls in it.
  await page.context().close();
  page = await newPage({
    viewport: { width: 390, height: 844 },
    hasTouch: true,
    ...(engineName === "chromium" ? { isMobile: true } : {}),
  });
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  if (await page.evaluate(() => document.querySelector(".app.narrow:not(.sidebar-open)") !== null))
    await page.locator(".session-bar .toggle-sidebar").click();
  await page.locator(`[data-session="${id}"]`).click();
  await page.waitForFunction(() => document.querySelector(".sidebar").getBoundingClientRect().right <= 0
    || !document.querySelector(".app.narrow"));
  await theReply().waitFor();
  await drawn();
  section("phone");
  console.log(await widths());
  await replyShot("phone matrix", ".msg.assistant .math-display >> nth=1");
  await replyShot("phone wide", ".msg.assistant .math-display >> nth=3");
  await page.locator(".msg.assistant .math-display >> nth=3").evaluate(el => { el.scrollLeft = el.scrollWidth; });
  await screenshot("phone wide scrolled");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => server.kill(), () => browser.close());
}
