// The review page's git bar against `prigh serve -faux-script`, in a git
// repository whose `origin` is a local bare repository: the branch and what
// is staged, a file approved with Space, another staged from its context
// menu, files selected with x, the commit (Shift+C) of the approved and
// staged files with a message drafted by the (scripted) model and streamed
// into its box, the commit, and the push to origin. Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: review_git.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "review_git");
const home = join(dir, "home");
const cwd = join(dir, "repo");
const origin = join(dir, "origin.git");
mkdirSync(home, { recursive: true });
mkdirSync(join(cwd, "src"), { recursive: true });

const gitEnv = { ...process.env, GIT_CONFIG_GLOBAL: "/dev/null", GIT_CONFIG_NOSYSTEM: "1" };
const gitIn = (where, ...args) => execFileSync("git", args, { cwd: where, env: gitEnv }).toString();
const git = (...args) => gitIn(cwd, ...args);
const lines = n => Array.from({ length: n }, (_, i) => `let v${i + 1} = ${i + 1}`).join("\n") + "\n";
gitIn(dir, "init", "-q", "--bare", "-b", "main", origin);
git("init", "-q", "-b", "main", ".");
git("config", "user.name", "Tester");
git("config", "user.email", "t@example.com");
git("config", "commit.gpgsign", "false");
git("remote", "add", "origin", origin);
writeFileSync(join(cwd, "src", "main.ml"), lines(20));
writeFileSync(join(cwd, ".gitignore"), ".prigh\n");
git("add", "-A");
git("commit", "-q", "-m", "main: start");
git("push", "-q", "-u", "origin", "main");
// The changes: an edit, and a new file.
writeFileSync(join(cwd, "src", "main.ml"), lines(20).replace("let v12 = 12\n", "let v12 = 12 * 2\n"));
writeFileSync(join(cwd, "src", "util.ml"), "let id x = x\n\nlet twice f x = f (f x)\n");

// The model: the commit message it drafts, streamed in pieces.
writeFileSync(join(dir, "script.json"), JSON.stringify([
  { text: "main: double v12, add util\n\nutil has id and twice, for the next step.", chunks: 6 },
]));

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  { env: { ...gitEnv, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
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
const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });
const page = await context.newPage();
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .replaceAll(cwd, "<REPO>")
  .replace(/\b[0-9a-f]{7}\b/g, "<SHA>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: review git: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-review-git-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const show = async (label, selector) => {
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await page.waitForTimeout(150);
  await screenshot(label);
};
const rows = () => page.evaluate(() =>
  [...document.querySelectorAll(".review-files li")].map(li => {
    const file = li.querySelector(".review-file");
    const flags = [
      file.classList.contains("current") && "current",
      file.classList.contains("approved") && "approved",
      file.classList.contains("marked") && "selected",
      li.querySelector(".review-file-staged")?.textContent,
    ].filter(Boolean);
    return `${file.dataset.path}: ${flags.join(", ") || "-"}`;
  }).join("\n"));

try {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();

  // 1. The review page: the git bar says where the branch stands.
  await page.locator(".composer textarea").fill("/review");
  await page.locator(".composer textarea").press("Enter");
  await page.locator(".git-bar .git-branch").waitFor();
  await page.locator('.review-file[data-path="src/util.ml"]').waitFor();
  await show("the bar", ".git-bar");
  section("the list: no boxes");
  console.log(await rows());
  console.log(`checkboxes: ${await page.locator(".review-files [role=checkbox], .review-files input[type=checkbox]").count()}`);

  // 2. Space approves main.ml and moves on to util.ml; util.ml's menu
  //    (Shift+F10, as right-click) stages it.
  await page.locator(".review-page").focus();
  await page.keyboard.press(" ");
  await page.locator(".review-file.approved[data-path='src/main.ml']").waitFor();
  await page.waitForFunction(() => document.querySelector(".review-file.current")?.dataset.path === "src/util.ml");
  await page.keyboard.press("Shift+F10");
  await page.locator(".context-menu [data-item=stage_files]").waitFor();
  await show("util.ml's menu", ".context-menu");
  await page.locator(".context-menu [data-item=stage_files]").click();
  await page.locator(".review-file-row:has([data-path='src/util.ml']) .review-file-staged").waitFor();
  section("approved and staged");
  console.log(await rows());
  await show("one staged", ".git-bar .git-counts");

  // 2b. x selects files (Shift+X every one from the last), for the
  //     selection's bar; Esc lets go of them.
  await page.keyboard.press("x");
  await page.keyboard.press("ArrowLeft");
  await page.keyboard.press("Shift+X");
  await page.locator(".review-selection").waitFor();
  await show("two selected", ".review-selection");
  section("selected");
  console.log(await rows());
  await page.keyboard.press("Escape");
  await page.locator(".review-selection").waitFor({ state: "detached" });

  // 2c. Shift+C: the commit, with the approved file and the staged one.
  await page.keyboard.press("Shift+C");
  await page.locator(".git-commit").waitFor();
  await show("the commit's files", ".git-commit-files");

  // 3. ✦ Generate: the approved file is staged, then the model's draft
  //    streams into the commit box.
  await page.locator(".git-commit .generate-go").click();
  await page.waitForFunction(() =>
    document.querySelector("#git-message")?.value.startsWith("main: double v12, add util\n\nutil has id and twice")
    && document.querySelector(".git-commit .generate-undo"));
  section("drafted");
  console.log(await page.locator("#git-message").inputValue());
  await show("the commit box", ".git-commit");
  await show("both staged", ".git-bar .git-counts");

  // 4. Commit, then push to origin.
  await page.locator(".git-commit-button").click();
  await page.locator(".git-notice").waitFor();
  await show("committed", ".git-notice");
  await show("ahead of origin", ".git-bar");
  await page.locator(".git-actions .btn", { hasText: "Push" }).click();
  await page.locator(".git-notice", { hasText: "Pushed" }).waitFor();
  await show("pushed", ".git-notice");
  await show("level with origin", ".git-bar .git-upstream");
  section("origin has it");
  console.log(gitIn(origin, "log", "-1", "--format=%s%n%n%b", "main").trim());
  console.log(gitIn(origin, "show", "--stat", "--format=", "main").trim().split("\n")
    .map(line => line.trim().replace(/\s+\|/g, " |")).join("\n"));

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
