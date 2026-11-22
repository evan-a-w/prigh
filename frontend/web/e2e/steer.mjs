// Steering a background subagent from its chat: opening it (its line in
// the transcript) puts it in the session's place, and the composer writes
// to it; Esc goes back to the session's chat, where the composer is the
// main agent's, and Alt+1 opens it again. Enter steers it and Alt+Enter
// queues a follow-up, both listed above the composer until they reach it;
// its transcript then has them; once it has finished, a message carries it
// on. Run by prigh_web.sh; prints normalised snapshots (the expected output
// is in ENGINE.expected) and, with SHOTS, screenshots.
//
// The subagent's bash call, and the main agent's, wait for a file the
// script creates, so that what runs when is fixed (the two share the faux
// script's replies, in order).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: steer.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "steer");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
// Each waits at most as long as the script may run, and every gate is
// opened at teardown, so no shell is left polling (#67).
const waitFor = file => `timeout 300 sh -c 'until [ -f ${file} ]; do sleep 0.1; done'; echo ${file}`;
const gates = ["sub-go", "main-go"];

writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    // The main agent: a background subagent, then it waits.
    {
      text: "Delegating the survey.",
      tool_calls: [
        { id: "s1", name: "subagent", arguments: { task: "Survey the project" } },
        { id: "w1", name: "bash", arguments: { command: waitFor("main-go") } },
      ],
    },
    // The subagent: a step that waits; the steer reaches it after it, the
    // follow-up when it ends its turn.
    { text: "Looking around.", tool_calls: [{ id: "b1", name: "bash", arguments: { command: waitFor("sub-go") } }] },
    { text: "Using the tests, as asked." },
    { text: "Summary: **3** files, all tested." },
    // Carried on after it finished, by a message in its chat.
    { text: "Carrying on: the docs are fine too." },
    // The main agent again, the report delivered meanwhile.
    { text: "The survey is in." },
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
const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: steer: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  // Once what just appeared has faded in.
  await page.waitForTimeout(300);
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-steer-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const clean = text => text
  .replace(/\b\d+(m \d\d)?s\b/g, "<T>")
  .replace(/\b\d\d:\d\d\b/g, "<TIME>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .split("\n").map(l => l.trimEnd()).filter(l => l !== "").join("\n");
const show = async (label, selector) => {
  section(label);
  const texts = await page.locator(selector).allInnerTexts();
  console.log(clean(texts.join("\n")) || "(nothing)");
};
const composer = page.locator(".composer textarea");
const recipient = async () => {
  const shown = await page.locator(".subagent-chat").getAttribute("data-subagent").catch(() => null);
  const crumbs = clean(await page.locator(".session-crumbs").innerText()).replace(/\n/g, " ");
  return `${shown ? `${shown}'s chat` : "the session's chat"} (${crumbs}); ${await composer.getAttribute("placeholder")}`;
};
const subagentChat = page.locator(".subagent-chat[data-subagent=a1]");
const subagentHas = text => page.waitForFunction(t =>
  document.querySelector(".subagent-chat[data-subagent=a1]")?.innerText.includes(t), text);
const toasts = () => page.locator(".toast").allInnerTexts();
const waitToast = text => page.waitForFunction(t =>
  [...document.querySelectorAll(".toast")].some(e => e.innerText.includes(t)), text);

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer.waitFor();

  await composer.fill("survey the project");
  await composer.press("Enter");
  // The subagent waits in its bash call; its line in the transcript opens
  // its chat.
  await page.locator("#chat .tool-subagent .tool-head[data-agent=a1]").click();
  await subagentChat.waitFor();
  await subagentHas("sub-go");
  section("its line opens its chat: the composer writes to it");
  console.log(await recipient());
  console.log(`stop button: ${await page.locator(".composer .stop").count()}`);
  await screenshot("a1's chat");

  // Esc goes back to the session's chat; Alt+1 opens it again.
  await composer.click();
  await page.keyboard.press("Escape");
  await subagentChat.waitFor({ state: "detached" });
  section("Esc");
  console.log(await recipient());
  console.log(`still running: ${await page.locator(".composer .stop").count() === 1}`);
  await page.keyboard.press("Alt+1");
  await subagentChat.waitFor();
  console.log(`Alt+1: ${await recipient()}`);

  // A steer and a follow-up, queued while it waits.
  await composer.fill("use the tests");
  await composer.press("Enter");
  await composer.fill("then summarise");
  await composer.press("Alt+Enter");
  await page.waitForFunction(() => document.querySelectorAll(".queued.to-agent").length === 2);
  await show("queued for it", ".queue");
  await screenshot("queued for a1");

  // Edit takes the follow-up back; sent again.
  await page.locator(".queued.to-agent .queued-edit").nth(1).click();
  await page.waitForFunction(() => document.querySelector(".composer textarea").value === "then summarise");
  await page.waitForFunction(() => document.querySelectorAll(".queued.to-agent").length === 1);
  section("edited");
  console.log(`in the editor: ${await composer.inputValue()}`);
  await composer.press("Alt+Enter");
  await page.waitForFunction(() => document.querySelectorAll(".queued.to-agent").length === 2);

  // It carries on: the steer, then the follow-up, reach it.
  writeFileSync(join(cwd, "sub-go"), "");
  await waitToast("a1 has finished");
  await page.waitForFunction(() => !document.querySelector(".queued"));
  await page.waitForFunction(() => !document.querySelector(".composer .stop"));
  await subagentHas("all tested");
  section("delivered, it finished");
  console.log((await toasts()).filter(t => t.includes("has finished")).join("\n"));
  console.log(await recipient());
  const openNotes = () => page.evaluate(() => {
    document.querySelectorAll(".subagent-chat details.note").forEach(d => { d.open = true; });
  });
  await openNotes();
  await show("its transcript", ".subagent-chat .entries");
  await screenshot("a1 steered and finished");

  // Finished, a message carries it on: it runs again.
  await composer.fill("check the docs too");
  await composer.press("Enter");
  await subagentHas("Carrying on");
  await page.waitForFunction(() => !document.querySelector(".composer .stop"));
  section("carried on");
  console.log(`in the editor: ${JSON.stringify(await composer.inputValue())}`);
  await openNotes();
  await show("its transcript", ".subagent-chat .entries");
  await screenshot("a1 carried on");

  await page.keyboard.press("Escape");
  await subagentChat.waitFor({ state: "detached" });
  writeFileSync(join(cwd, "main-go"), "");
  await page.waitForFunction(() => document.getElementById("chat").innerText.includes("The survey is in."));
  await page.waitForFunction(() => !document.querySelector(".composer .btn.stop"));
  section("the main agent got the report");
  console.log("The survey is in.");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  for (const gate of gates) writeFileSync(join(cwd, gate), "");
  await end(() => server.kill(), () => browser.close());
}
