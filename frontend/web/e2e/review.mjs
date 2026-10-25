// The review against `prigh serve -faux-script` in a git repository with
// changes: the file browser marking them and showing a changed file's diff,
// comments on lines sent to the agent, its replies (one proposing a change,
// accepted), the review page beside the chat (approvals, the keys, the
// whole-file layout, edit mode saving a file), live updates as the agent
// edits, and a phone. Run by prigh_web.sh; prints normalised snapshots (the
// expected output is in prigh_web.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: review.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "review");
const home = join(dir, "home");
const cwd = join(dir, "repo");
mkdirSync(home, { recursive: true });
mkdirSync(join(cwd, "src"), { recursive: true });

const git = (...args) =>
  execFileSync("git", args, {
    cwd,
    env: {
      ...process.env, GIT_CONFIG_GLOBAL: "/dev/null", GIT_CONFIG_NOSYSTEM: "1",
      GIT_AUTHOR_NAME: "t", GIT_AUTHOR_EMAIL: "t@t", GIT_COMMITTER_NAME: "t", GIT_COMMITTER_EMAIL: "t@t",
    },
  });
const lines = n => Array.from({ length: n }, (_, i) => `let v${i + 1} = ${i + 1}`).join("\n") + "\n";
writeFileSync(join(cwd, "src", "main.ml"), lines(30));
writeFileSync(join(cwd, "README.md"), "# Demo\n\nA small project.\n");
writeFileSync(join(cwd, ".gitignore"), ".prigh\n");
git("init", "-q", "-b", "main", ".");
git("add", "-A");
git("commit", "-q", "-m", "init");
// The changes to review: an edit in the middle, a new file.
writeFileSync(
  join(cwd, "src", "main.ml"),
  lines(30).replace("let v12 = 12\n", "let v12 = 12 * 2\nlet doubled = true\n"),
);
writeFileSync(join(cwd, "src", "util.ml"), "let id x = x\n\nlet twice f x = f (f x)\n");

// The agent: replies to the comments, one with a proposed change, and edits
// another file meanwhile.
writeFileSync(join(dir, "script.json"), JSON.stringify([
  {
    text: "On it.",
    tool_calls: [
      { id: "r1", name: "reply_to_comment", arguments: { id: "c1", text: "Doubling is intended: `v12` feeds the **scale**.", resolve: false } },
      { id: "r2", name: "reply_to_comment", arguments: { id: "c2", text: "Agreed, a clearer name:", edits: [{ old_text: "let twice f x", new_text: "let apply_twice f x" }] } },
      { id: "e1", name: "edit", arguments: { path: "README.md", edits: [{ old_text: "A small project.", new_text: "A small project, reviewed." }] } },
    ],
  },
  { text: "Replied to both comments and updated the README." },
]));

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
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
const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });
const page = await context.newPage();
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .replaceAll(cwd, "<REPO>")
  .replace(/(Yesterday )?\b\d\d:\d\d\b/g, "<TIME>")
  .replace(/\b[0-9a-f]{8}\b/g, "<SHA>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "" && line !== "Today")
  .join("\n");
const section = label => console.log(`=== ${engineName}: review: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-review-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const show = async (label, selector) => {
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await page.waitForTimeout(150);
  await screenshot(label);
};
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const line = n => page.locator(`.review-page tr[data-line="${n}"] td.ln.pick`);

try {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();

  // 1. The file browser marks what changed: the directory with a count, the
  //    files with a dot; a changed file previews as its diff.
  await page.keyboard.press("Control+Shift+F");
  await page.locator(".files-row.changed").first().waitFor();
  section("files: changes marked");
  console.log(clean(await page.evaluate(() =>
    [...document.querySelectorAll(".files-row")].map(r =>
      `${r.dataset.name}${r.classList.contains("changed") ? " (changed" + (r.querySelector(".files-change-count") ? `: ${r.querySelector(".files-change-count").textContent}` : "") + ")" : ""}`).join("\n"))));
  await page.locator('.files-row[data-name="src"]').click();
  await page.locator('.files-row[data-name="main.ml"]').click();
  await page.locator(".files-preview .review-diff").waitFor();
  await show("files: a changed file's diff", ".files-preview");
  await page.locator(".files-preview .segment", { hasText: "Whole file" }).click();
  await page.locator('.files-preview tr[data-line="31"]').waitFor();
  section("files: the whole file");
  console.log(`rows: ${await page.locator(".files-preview .review-diff tr").count()}`);
  await screenshot("files: the whole file");
  await page.locator(".files-preview .segment", { hasText: "Changes" }).click();

  // 2. A comment on a range of lines, from the file browser.
  await page.locator('.files-preview tr[data-line="12"] td.ln.pick').click();
  await page.locator('.files-preview tr[data-line="13"] td.ln.pick').click({ modifiers: ["Shift"] });
  await page.locator(".review-select-bar .btn", { hasText: "Comment on lines 12–13" }).click();
  await page.locator("#review-draft").fill("Why double it here?");
  await screenshot("files: writing a comment");
  await page.locator("#review-draft").press("Control+Enter");
  await page.locator(".files-preview .review-thread:not(.draft)").waitFor();
  await show("files: the comment under its lines", ".files-preview .review-thread");

  // 3. The review page, from the browser's button: the chat goes beside it.
  await page.locator(".files-panel .files-head .review-open").click();
  await page.locator(".review-page").waitFor();
  await page.locator(".review-page .review-file.current").waitFor();
  await show("page: opened", ".review-head");
  section("page: the files");
  console.log(clean(await page.locator(".review-files").innerText()));
  console.log(`chat still shown: ${await page.locator(".main .composer").isVisible()}`);
  console.log(`file browser hidden: ${!(await page.locator(".files-panel").isVisible())}`);

  // 4. Keys: ] and [ go through the files; a comments the new file.
  await page.locator(".review-page").focus();
  await page.keyboard.press("]");
  await page.waitForFunction(() => document.querySelector(".review-file.current")?.dataset.path === "src/util.ml");
  await line(3).waitFor();
  await line(3).click();
  await page.keyboard.press("c");
  await page.locator("#review-draft").fill("Name it for what it does.");
  await page.locator(".review-draft-actions .btn.primary").click();
  await page.locator(".review-page .review-thread:not(.draft)").waitFor();
  await show("page: a second comment, not sent yet", ".review-current");
  await page.locator(".review-page").focus();
  await page.keyboard.press("[");
  await page.waitForFunction(() => document.querySelector(".review-file.current")?.dataset.path === "src/main.ml");

  // 5. Approving: a, and the button.
  await page.keyboard.press("a");
  await page.locator(".review-file.current.approved").waitFor();
  await show("page: main.ml approved", ".review-head");

  // 6. Sending the comments: the agent replies to each, proposes a change
  //    and edits the README meanwhile, which shows up live.
  await page.locator(".review-controls .btn", { hasText: "Send 2 comments" }).click();
  await idle();
  await page.locator(".review-file[data-path='README.md']").waitFor();
  await page.locator(".review-page .review-post.agent").waitFor();
  section("page: the files after the agent's edit");
  console.log(clean(await page.locator(".review-files").innerText()));
  await show("page: the agent's reply", ".review-current");
  section("chat: what the agent did");
  console.log(clean(await page.locator(".chat").innerText()).split("\n").slice(-14).join("\n"));
  await page.locator(".review-file[data-path='src/util.ml']").click();
  await page.locator(".review-proposal-actions").waitFor();
  await show("page: a proposed change", ".review-current");
  await page.locator(".review-proposal-actions .btn", { hasText: "Accept" }).click();
  await page.locator(".review-proposal-state.done").waitFor();
  // The file read again: the comment's line is not what it was, so it
  // shows above the file.
  await page.locator(".review-thread.outdated").waitFor();
  await show("page: accepted, the file changed", ".review-current");
  console.log(`util.ml on disk: ${JSON.stringify(readFileSync(join(cwd, "src", "util.ml"), "utf8"))}`);

  // 7. A file that changes after it was approved is no longer approved.
  writeFileSync(join(cwd, "src", "main.ml"), readFileSync(join(cwd, "src", "main.ml"), "utf8") + "let late = 0\n");
  await page.locator(".review-refresh").click();
  await page.locator(".review-file[data-path='src/main.ml']").click();
  await page.locator(".review-page").focus();
  await page.keyboard.press("v");
  await page.keyboard.press("v");
  await page.locator(".review-file.current.changed").waitFor();
  await show("page: changed since approved", ".review-file-head");

  // 8. Edit mode: the file's text, saved where the tools run.
  await page.locator(".review-controls .segment", { hasText: "Edit" }).click();
  await page.locator("#review-editor").waitFor();
  await page.locator("#review-editor").press("Control+End");
  await page.keyboard.type("let edited = 1\n");
  await screenshot("page: editing");
  await page.locator("#review-editor").press("Control+s");
  await page.locator(".review-editor-state", { hasText: /^Saved/ }).waitFor();
  console.log(`main.ml ends: ${JSON.stringify(readFileSync(join(cwd, "src", "main.ml"), "utf8").split("\n").slice(-3).join("\n"))}`);
  await page.locator(".review-controls .segment", { hasText: "Comment" }).click();
  await page.locator(".review-controls .segment", { hasText: "Whole file" }).click();
  await page.locator(".review-page tr[data-line='33']").waitFor();
  await screenshot("page: the whole file with its changes");

  // 9. Esc closes it; /review opens it again.
  await page.locator(".review-page").focus();
  await page.keyboard.press("Escape");
  await page.waitForSelector(".review-page", { state: "detached" });
  console.log(`Esc closed it: true`);
  await page.locator(".composer textarea").fill("/review");
  await page.locator(".composer textarea").press("Enter");
  await page.locator(".review-page").waitFor();
  console.log(`/review opened it: true`);

  // 10. A phone: a sheet over the chat.
  await page.setViewportSize({ width: 390, height: 800 });
  await page.waitForTimeout(300);
  await screenshot("phone");
  section("phone");
  console.log(`sheet: ${await page.evaluate(() => getComputedStyle(document.querySelector(".review-page")).position)}`);
  await page.keyboard.press("Alt+r");
  await page.waitForSelector(".review-page", { state: "detached" });
  console.log("Alt+R closed it");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  server.kill();
}
