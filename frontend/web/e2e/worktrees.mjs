// The repository's worktrees against a real git repository: the sidebar's
// section (a worktree with uncommitted changes, the current one), a new
// worktree from its dialog that the session moves into, removing one (refused
// with changes, then forced), and /worktree's picker and open. Run by
// prigh_web.sh; prints normalised snapshots (the expected output is in
// prigh_web.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, realpathSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: worktrees.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "worktrees"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "worktrees"));
const home = join(dir, "home");
const repo = join(dir, "repo");
const docs = join(dir, "docs");
mkdirSync(home, { recursive: true });
mkdirSync(repo, { recursive: true });
writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: "noted" }]));

const git = (cwd, ...args) =>
  execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
writeFileSync(join(repo, "README.md"), "# Repo\n");
git(repo, "init", "-q", "-b", "main");
git(repo, "add", "README.md");
git(repo, "commit", "-q", "-m", "start");
git(repo, "worktree", "add", "-q", "-b", "docs", docs);
writeFileSync(join(docs, "README.md"), "# Repo\n\nDocs.\n");
writeFileSync(join(docs, "guide.md"), "# Guide\n");

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", repo],
  { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
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
const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
// The worktree calls and their answers, shown when a step fails.
const frames = [];
page.on("websocket", ws => {
  ws.on("framesent", f => frames.push(`sent ${f.payload}`));
  ws.on("framereceived", f => frames.push(`received ${f.payload}`));
});
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .split(dir).join("<DIR>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: worktrees: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-worktrees-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const show = async (label, selector) => {
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await page.waitForTimeout(200); // its fade in
  await screenshot(label);
};
const rows = async label => {
  section(label);
  for (const row of await page.locator(".worktree").all()) {
    const classes = (await row.getAttribute("class")).split(" ").filter(c => c !== "worktree").sort();
    const text = clean(await row.innerText()).split("\n").join(" | ");
    console.log(`${text}${classes.length ? ` [${classes.join(" ")}]` : ""}`);
  }
  await screenshot(label);
};
const toastHas = text => page.locator(".toast", { hasText: text }).first().waitFor();
const editor = () => page.locator(".composer textarea");
// Ctrl+Enter: as typed, whatever the completion popup offers.
const run = async command => {
  await editor().fill(command);
  await editor().press("Control+Enter");
};
// Listed again after a change: wait until the rows say it.
const listed = count => page.waitForFunction(
  n => document.querySelectorAll(".worktree").length === n
    && !document.querySelector(".worktrees-head .lamp"),
  count,
);

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await editor().waitFor();
  await listed(2);
  await rows("the sidebar");
  section("the section's buttons");
  console.log((await page.locator(".worktrees-head button").evaluateAll(bs =>
    bs.map(b => b.getAttribute("aria-label") || b.innerText.replace(/\s+/g, " ").trim()))).join(", "));

  // A new worktree from the sidebar: the session moves into it.
  await page.getByRole("button", { name: "New worktree" }).click();
  await page.locator(".worktree-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("the new worktree dialog", ".worktree-dialog");
  await page.locator("#worktree-branch").fill("feat/login");
  await page.keyboard.press("Enter");
  await toastHas("Working in the worktree feat/login");
  await page.waitForSelector(".worktree-dialog", { state: "detached" });
  await page.locator(".worktree.current", { hasText: "feat/login" }).waitFor();
  await listed(3);
  await rows("created and moved there");

  // Removing the one with changes: refused, then forced.
  await page.locator(".worktree", { hasText: "docs" }).hover();
  await page.getByRole("button", { name: "Remove docs" }).click();
  await page.locator(".remove-worktree-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("remove: asked first", ".remove-worktree-dialog");
  await page.locator(".remove-worktree-dialog").getByRole("button", { name: "Remove", exact: true }).click();
  await page.locator(".remove-worktree-dialog .dialog-error").waitFor();
  await show("remove: refused with changes", ".remove-worktree-dialog");
  await page.getByRole("button", { name: "Discard changes and remove" }).click();
  await toastHas("Removed the worktree docs");
  await listed(2);
  await rows("removed");

  // /worktree: a picker; open goes back to the main one.
  await run("/worktree");
  await page.locator(".picker-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("the /worktree picker", ".picker-dialog");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".picker-dialog", { state: "detached" });
  await run("/worktree open main");
  await toastHas("Working in the worktree main");
  await page.locator(".worktree.current", { hasText: "main" }).waitFor();
  await listed(2);
  await rows("back in main");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(frames.filter(f => /worktree/.test(f)).map(f => f.slice(0, 400)).join("\n"));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  server.kill();
}
