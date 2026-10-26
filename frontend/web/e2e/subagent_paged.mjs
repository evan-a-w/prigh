// A subagent whose call is in a page not loaded: the main agent delegates,
// then works on for more messages than a page holds; reloaded, the page has
// only the newest of them, and the subagent opened in the agents panel
// shows its transcript all the same (fetched on its own). Run by
// prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
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
if (!engine) throw new Error("usage: subagent_paged.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "subagent_paged");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("subagent_paged.mjs: timed out"); process.exit(2); }, 240000).unref();

// The main agent and the subagent share the faux script's replies, in
// order: the main agent waits in its bash call while the subagent works.
const waitFor = file => `until [ -f ${file} ]; do sleep 0.1; done; echo ${file}`;
const steps = 35;
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    {
      text: "Delegating the count.",
      tool_calls: [
        { id: "s1", name: "subagent", arguments: { task: "Count the files" } },
        { id: "w1", name: "bash", arguments: { command: waitFor("main-go") } },
      ],
    },
    { text: "There are **3** files." },
    // Two messages a step: more than a page after the call.
    ...Array.from({ length: steps }, (_, i) => ({
      text: `Step ${i + 1}.`,
      tool_calls: [{ id: `e${i + 1}`, name: "bash", arguments: { command: `echo ${i + 1}` } }],
    })),
    // Still running when the page reloads.
    { text: "Waiting.", tool_calls: [{ id: "w2", name: "bash", arguments: { command: waitFor("end") } }] },
    { text: "All done." },
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
page.setDefaultTimeout(20000);
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: subagent_paged: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(300);
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-subagent_paged-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const clean = text => text
  .replace(/\b\d\d:\d\d\b/g, "<TIME>")
  .split("\n").map(l => l.trimEnd()).filter(l => l !== "").join("\n");
const chatHas = text => page.evaluate(t => document.getElementById("chat").innerText.includes(t), text);

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  const composer = page.locator(".composer textarea");
  await composer.waitFor();

  await composer.fill("count the files");
  await composer.press("Enter");
  await page.waitForFunction(() => document.getElementById("chat").innerText.includes("There are 3 files."));
  writeFileSync(join(cwd, "main-go"), "");
  await page.waitForFunction(() => document.getElementById("chat").innerText.includes("Waiting."));

  await page.reload();
  await page.waitForFunction(() => document.getElementById("chat")?.innerText.includes("Waiting."));
  section("reloaded");
  console.log(`the call is loaded: ${await chatHas("Delegating the count.")}`);
  console.log(`older messages to load: ${await page.locator("#chat [data-older]").count() > 0}`);

  await page.locator(".agents-toggle").click();
  await page.locator(".agents-row", { hasText: "Count the files" }).click();
  await page.waitForFunction(() => document.querySelector(".agents-transcript")?.innerText.includes("3 files"));
  section("its transcript");
  console.log(clean(await page.locator(".agents-transcript").innerText()));
  await screenshot("its transcript");

  writeFileSync(join(cwd, "end"), "");
  await page.waitForFunction(() => document.getElementById("chat").innerText.includes("All done."));

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
