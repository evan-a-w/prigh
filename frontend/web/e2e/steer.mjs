// Steering a background subagent from the composer: showing it in the agents
// panel leaves the composer on the main agent; its Steer button makes the
// composer write to it (a chip over the editor says so), Enter
// steers it and Alt+Enter queues a follow-up, both listed above the
// composer until they reach it; its transcript then has them; once it
// finishes the composer is the main agent's again. Run by prigh_web.sh;
// prints normalised snapshots (the expected output is in ENGINE.expected)
// and, with SHOTS, screenshots.
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
const waitFor = file => `until [ -f ${file} ]; do sleep 0.1; done; echo ${file}`;
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
  if (message.type() === "error" && !/favicon\.ico|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
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
  const chip = page.locator(".steer-chip");
  const placeholder = await composer.getAttribute("placeholder");
  return `${(await chip.count()) ? clean(await chip.innerText()).replace(/\n/g, " ") : "(no chip)"}; ${placeholder}`;
};
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
  // The subagent waits in its bash call.
  await page.locator(".agents-toggle").waitFor();
  await page.locator(".agents-toggle").click();
  await page.locator(".agents-row.running").first().waitFor();
  await page.locator(".agents-row.running").first().click();
  await page.waitForSelector(".agents-detail .agents-transcript .entries");
  await page.waitForFunction(() => document.querySelector(".agents-transcript")?.innerText.includes("sub-go"));
  section("showing it leaves the composer on the main agent");
  console.log(await recipient());
  console.log(clean(await page.locator(".agents-foot").innerText()));
  await screenshot("showing a1");

  // Its Steer button writes to it; the editor has the focus.
  await page.locator(".agents-steer").click();
  await page.locator(".steer-chip").waitFor();
  section("Steer");
  console.log(await recipient());
  console.log(`editor focused: ${await composer.evaluate(e => e === document.activeElement)}`);
  console.log(clean(await page.locator(".agents-foot").innerText()));
  await screenshot("steering a1");

  // Esc goes back to the main agent; Steer again writes to it.
  await composer.click();
  await page.keyboard.press("Escape");
  await page.locator(".steer-chip").waitFor({ state: "detached" });
  section("Esc");
  console.log(await recipient());
  console.log(`still running: ${await page.locator(".composer .btn.stop").count() === 1}`);
  await page.locator(".agents-steer").click();
  await page.locator(".steer-chip").waitFor();
  console.log(`Steer: ${await recipient()}`);

  // A steer and a follow-up, queued while it waits.
  await composer.fill("use the tests");
  await composer.press("Enter");
  await composer.fill("then summarise");
  await composer.press("Alt+Enter");
  await page.waitForFunction(() => document.querySelectorAll(".queued.to-agent").length === 2);
  await show("queued for it", ".queue");
  await show("its row says so", ".agents-foot .detail-more");
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
  section("delivered, it finished");
  console.log((await toasts()).filter(t => t.includes("has finished")).join("\n"));
  section("the composer is the main agent's again");
  console.log(await recipient());
  await page.evaluate(() => {
    document.querySelectorAll(".agents-transcript details.note").forEach(d => { d.open = true; });
  });
  await show("its transcript", ".agents-transcript .entries");
  await screenshot("a1 steered and finished");

  // A finished subagent takes no messages: what to do instead.
  await composer.fill("/agents steer a1 one more thing");
  await composer.press("Enter");
  await waitToast("Couldn't send it to a1");
  section("too late");
  console.log((await toasts()).filter(t => t.includes("Couldn't")).join("\n"));
  console.log(`in the editor: ${await composer.inputValue()}`);
  await screenshot("too late");
  await composer.fill("");

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
  await end(() => server.kill(), () => browser.close());
}
