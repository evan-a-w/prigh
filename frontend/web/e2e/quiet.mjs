// Quiet verbosity: each run of steps between messages (thinking, tool calls)
// is one line saying what it did and how long it took; a click (a tap on a
// phone) lists the steps, a step's line opens its output; a failed step
// marks its run; an approval the agent waits on is asked outside the run,
// whose line says what it is doing meanwhile. On a desktop (1280x800) or a
// phone (390x844, 320x640; touch): the run's line is a thumb's height and
// nothing scrolls sideways. Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName, part] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
const viewports = {
  desktop: { width: 1280, height: 800 },
  phone: { width: 390, height: 844 },
  small: { width: 320, height: 640 },
};
const viewport = viewports[part];
if (!engine || !viewport) throw new Error("usage: quiet.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `quiet-${part}`);
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
writeFileSync(join(cwd, "notes.txt"), "the bug is in fix.txt\n");

writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    {
      thinking: "Let me look around first.",
      tool_calls: [
        { id: "b1", name: "bash", arguments: { command: "ls" } },
        { id: "r1", name: "read", arguments: { path: "notes.txt" } },
      ],
    },
    { tool_calls: [{ id: "b2", name: "bash", arguments: { command: "echo no such target >&2; exit 3" } }] },
    { text: "Now the fix.", tool_calls: [{ id: "w1", name: "write", arguments: { path: "fix.txt", content: "fixed\n" } }] },
    { text: "Done: wrote fix.txt." },
    { tool_calls: [{ id: "b3", name: "bash", arguments: { command: "echo cleaned" } }] },
    { text: "Cleaned up." },
  ]),
);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh" }, stdio: ["ignore", "ignore", "pipe"] },
);
const url = await new Promise((resolve, reject) => {
  let err = "";
  server.stderr.on("data", data => {
    err += data;
    const m = err.match(/^prigh: prigh-web on (\S+)$/m);
    if (m) resolve(m[1]);
  });
  server.on("exit", code => reject(new Error(`backend exited (${code}):\n${err}`)));
});

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const options = { viewport, reducedMotion: "reduce" };
if (touch) {
  options.hasTouch = true;
  if (engineName !== "firefox") options.isMobile = true;
}
const context = await browser.newContext(options);
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: quiet ${part}: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-quiet-${part}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const composer = page.locator(".composer textarea");
const press = async locator => (touch ? locator.tap() : locator.click());
const run = async command => {
  await composer.fill(command);
  await composer.press("Control+Enter");
};
// Times of day and durations masked.
const plain = s => s
  .replace(/\b\d\d:\d\d\b/g, "(time)")
  .replace(/(<1s|\b\d+(m|s)( \d+s)?)\b/g, "(took)")
  .replace(/\s+/g, " ")
  .trim();
const runs = page.locator("#chat .run");
const transcript = async () => {
  for (const line of await page.locator("#chat .entries > .msg").evaluateAll(es =>
    es.map(e => (e.matches(".user") ? "you: " : "prigh: ") + [...e.querySelectorAll(":scope > .run, :scope > :not(.run)")].map(c => (c.matches(".run") ? "[run] " : "") + c.innerText).join(" "))) ) {
    console.log(plain(line));
  }
};
const head = async locator => {
  const box = await locator.boundingBox();
  return `${await locator.getAttribute("aria-expanded")}${touch ? `, ${Math.round(box.height)}px tall` : ""}`;
};
const sideways = () => page.evaluate(() => {
  const out = [];
  if (document.scrollingElement.scrollWidth > innerWidth + 1) out.push("the page");
  for (const e of document.querySelectorAll("#chat, #chat .run, #chat .run-steps"))
    if (e.scrollWidth > e.clientWidth + 1) out.push(e.id ? `#${e.id}` : e.className);
  return out.length ? out.join(", ") : "none";
});

try {
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer.waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; }" });
  await run("/verbosity quiet");
  await page.locator("#chat.verbosity-quiet").waitFor();

  await composer.fill("fix the bug");
  await composer.press("Enter");
  await page.locator("#chat .msg.assistant", { hasText: "Done: wrote fix.txt." }).waitFor();
  section("a turn: two runs of steps, the first with a failed step");
  await transcript();
  console.log(`first run's line: ${await head(runs.first().locator(".run-head"))}`);
  console.log(`sideways: ${await sideways()}`);
  await screenshot("collapsed");

  await press(runs.first().locator(".run-head"));
  await runs.first().locator(".run-steps").waitFor();
  section("the first run opened: its steps");
  console.log(plain(await runs.first().innerText()));
  console.log(`sideways: ${await sideways()}`);
  await screenshot("opened");

  // A failed step shows its output as in normal; the others open theirs.
  const read = runs.first().locator(".run-steps > .tool", { hasText: "notes.txt" });
  await press(read.locator(".tool-head"));
  await read.locator(".tool-body").waitFor();
  section("a step opened: its output");
  console.log(plain(await runs.first().innerText()));
  console.log(`sideways: ${await sideways()}`);
  await screenshot("step-opened");

  await press(runs.first().locator(".run-head"));
  await runs.first().locator(".run-steps").waitFor({ state: "detached" });
  section("closed again");
  console.log(`first run's line: ${await head(runs.first().locator(".run-head"))}`);

  // An approval: asked outside the run, which says what is waiting.
  await run("/mode ask");
  await page.locator(".context-card .mode-line .status-detail-value", { hasText: "Asks: edits & commands" }).waitFor({ state: "attached" });
  await composer.fill("clean up");
  await composer.press("Enter");
  await page.locator("#confirm").waitFor();
  section("waiting for an approval");
  console.log(`last run: ${plain(await runs.last().innerText())}`);
  console.log(`last run running: ${await runs.last().evaluate(e => e.classList.contains("running"))}`);
  console.log(`approval inside a run: ${await page.locator("#chat .run #confirm").count()}`);
  await screenshot("approval");
  await page.keyboard.press("y");
  await page.locator("#chat .msg.assistant", { hasText: "Cleaned up." }).waitFor();
  // Its time shows once the message has ended.
  await page.waitForFunction(() => [...document.querySelectorAll("#chat .msg.assistant")]
    .some(m => m.innerText.includes("Cleaned up.") && /\b\d\d:\d\d\b/.test(m.innerText)));
  section("approved");
  await transcript();
  await screenshot("approved");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot("failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
