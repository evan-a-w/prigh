// Projects and their worktrees against real git repositories: the sidebar's
// tree (a repository with a worktree that has uncommitted changes, one made
// for a subagent, one made from the project's row with a session in it,
// another repository, a plain folder), folding a project, searching, removing
// a worktree (refused with changes, then forced; a subagent's), /worktree's
// picker, and the tree on a phone. Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in prigh_web.expected).
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
const repo = join(dir, "prigh");
const docs = join(dir, "prigh-docs");
const web = join(dir, "site");
const notes = join(dir, "notes");
for (const d of [home, repo, web, notes]) mkdirSync(d, { recursive: true });
// The first prompt asks for a subagent in a worktree of its own; everything
// else is answered "noted" (the main agent and the subagent share the script,
// in whichever order they ask).
writeFileSync(join(dir, "script.json"), JSON.stringify([
  {
    text: "handing it over",
    tool_calls: [{ id: "s1", name: "subagent", arguments: { task: "Tidy the docs", worktree: "auto" } }],
  },
  ...Array(30).fill({ text: "noted" }),
]));

const git = (cwd, ...args) =>
  execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
for (const [d, branch] of [[repo, "main"], [web, "trunk"]]) {
  writeFileSync(join(d, "README.md"), "# Repo\n");
  git(d, "init", "-q", "-b", branch);
  git(d, "add", "README.md");
  git(d, "commit", "-q", "-m", "start");
}
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
const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 2 })).newPage();
// The calls and their answers, shown when a step fails.
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
  .replace(/\b(just now|\d+[smhd] ago)\b/g, "<age>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: worktrees: ${label} ===`);
let shot = 0;
const screenshot = async (label, target = page) => {
  if (!process.env.SHOTS) return;
  shot++;
  await target.screenshot({
    path: join(process.env.SHOTS, `${engineName}-worktrees-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const show = async (label, selector) => {
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await page.waitForTimeout(200); // its fade in
  await screenshot(label);
};
// The tree, a line per row: its text, indented by depth, and its state.
const tree = async (label, target = page) => {
  section(label);
  const rows = await target.locator(".sessions").evaluate(list =>
    [...list.querySelectorAll(".project-row, .worktree-row, .worktree-origin, .session")].map(row => {
      const owner = row.classList.contains("session") || row.classList.contains("worktree-origin")
        ? row : row.parentElement;
      const depth = (row.closest(".project-body") ? "│ " : "")
        + (row.closest(".worktree") && !row.classList.contains("worktree-row") ? "│ " : "");
      const text = row.innerText.split("\n").map(s => s.trim()).filter(s => s).join(" | ");
      const marks = [...owner.classList].filter(c =>
        ["here", "folded", "selected", "dirty", "agent", "running"].includes(c));
      return `${depth}${text}${marks.length ? ` [${marks.sort().join(" ")}]` : ""}`;
    }));
  console.log(clean(rows.join("\n")));
  await target.waitForTimeout(200);
  await screenshot(label, target);
};
const toastHas = text => page.locator(".toast", { hasText: text }).first().waitFor();
const editor = () => page.locator(".composer textarea");
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const send = async text => {
  await editor().fill(text);
  await editor().press("Enter");
  await page.locator(".chat", { hasText: text }).waitFor();
  await idle();
};
// Ctrl+Enter: as typed, whatever the completion popup offers.
const run = async command => {
  await editor().fill(command);
  await editor().press("Control+Enter");
};
const project = name =>
  page.locator(".project", { has: page.locator(".project-name", { hasText: new RegExp(`^${name}$`) }) });
const worktree = branch =>
  page.locator(".worktree", { has: page.locator(".worktree-branch", { hasText: new RegExp(`^${branch}$`) }) });
const newSession = async cwd => {
  await page.locator(".new-session").click();
  await page.waitForFunction(() => !document.querySelector(".new-session.opening"));
  await run(`/cd ${cwd}`);
  await page.waitForFunction(d => document.body.innerText.includes(d), cwd.split("/").pop());
};

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await editor().waitFor();

  // A subagent asked to work in a worktree of its own.
  await send("Tidy the docs in a worktree");
  await worktree("agent/tidy-the-docs").waitFor();
  await page.locator(".chat", { hasText: "agent/tidy-the-docs" }).first().waitFor();

  // Another repository, and a plain folder.
  await newSession(web);
  await send("A landing page");
  await newSession(notes);
  await send("Shopping list");

  // A new worktree from the project's row: a new session starts in it.
  await project("prigh").locator(".project-row").hover();
  await project("prigh").getByRole("button", { name: "New worktree of prigh" }).click();
  await page.locator(".worktree-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("the new worktree dialog", ".worktree-dialog");
  await page.locator("#worktree-branch").fill("feat/login");
  await page.keyboard.press("Enter");
  await toastHas("New session in");
  await page.waitForSelector(".worktree-dialog", { state: "detached" });
  await send("The login form");
  await worktree("feat/login").locator(".session").waitFor();

  // Back to the first session: its repository's other worktrees show, with
  // their changes.
  await page.locator(".session", { hasText: "Tidy the docs" }).first().locator(".session-open").click();
  await page.locator(".session.selected", { hasText: "Tidy the docs" }).waitFor();
  await worktree("docs").waitFor();
  await page.waitForFunction(() => document.querySelector(".worktree-changes"));
  await page.mouse.move(1000, 400);
  await tree("the project tree");
  // How it looks, with a project's and a worktree's actions at hand, in the
  // dark and light themes (screenshots only).
  const theme = () => page.evaluate(() => document.documentElement.dataset.theme);
  const started = await theme();
  for (const look of ["dark", "light"]) {
    await run(`/theme ${look}`);
    await page.waitForFunction(t => document.documentElement.dataset.theme === t, look);
    await project("prigh").locator(".project-row").first().hover();
    await page.waitForTimeout(200);
    await screenshot(`the project tree, ${look}, a project's actions`);
    await worktree("feat/login").locator(".worktree-row").hover();
    await page.waitForTimeout(200);
    await screenshot(`the project tree, ${look}, a worktree's actions`);
  }
  await run(`/theme ${started}`);
  await page.waitForFunction(t => document.documentElement.dataset.theme === t, started);
  await page.mouse.move(1000, 400);

  // Folding a project: its count stays on its row.
  await project("site").locator(".project-toggle").click();
  await project("site").locator(".tree-count").waitFor();
  await page.mouse.move(1000, 400);
  await tree("a folded project");
  await project("site").locator(".project-toggle").click();

  // Search finds sessions in every project (what else matches depends on
  // the temporary directory's name: only the match is printed).
  await page.locator("#session-search").fill("login form");
  const match = page.locator(".session", { hasText: "The login form" });
  await page.locator(".session", { hasText: "Shopping list" }).waitFor({ state: "detached" }).catch(() => {});
  await match.waitFor();
  section("searching");
  console.log(await match.evaluate(row => [
    row.closest(".project").querySelector(".project-name").textContent,
    row.closest(".worktree")?.querySelector(".worktree-branch").textContent,
    row.querySelector(".session-title").textContent,
  ].join(" › ")));
  await screenshot("searching");
  await page.locator("#session-search").fill("");

  // Removing the one with changes: refused, then forced.
  await worktree("docs").locator(".worktree-row").hover();
  await page.getByRole("button", { name: "Remove docs" }).click();
  await page.locator(".remove-worktree-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("remove: asked first", ".remove-worktree-dialog");
  await page.locator(".remove-worktree-dialog").getByRole("button", { name: "Remove", exact: true }).click();
  await page.locator(".remove-worktree-dialog .dialog-error").waitFor();
  await show("remove: refused with changes", ".remove-worktree-dialog");
  await page.getByRole("button", { name: "Discard changes and remove" }).click();
  await toastHas("Removed the worktree docs");
  await worktree("docs").waitFor({ state: "detached" });
  // The subagent's: its session forgets it.
  await worktree("agent/tidy-the-docs").locator(".worktree-row").hover();
  await page.getByRole("button", { name: "Remove agent/tidy-the-docs" }).click();
  await page.locator(".remove-worktree-dialog").waitFor();
  await page.keyboard.press("Enter");
  await toastHas("Removed the worktree agent/tidy-the-docs");
  await worktree("agent/tidy-the-docs").waitFor({ state: "detached" });
  await page.mouse.move(1000, 400);
  await tree("removed");

  // /worktree: a picker of the repository's worktrees.
  await run("/worktree");
  await page.locator(".picker-dialog").waitFor();
  await page.waitForTimeout(300); // its fade in
  await show("the /worktree picker", ".picker-dialog");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".picker-dialog", { state: "detached" });

  // On a phone: the sidebar over the chat, every action at hand.
  const phone = await (await browser.newContext({
    viewport: { width: 390, height: 844 },
    isMobile: engineName === "chromium",
    hasTouch: true,
  })).newPage();
  phone.on("pageerror", error => errors.push(`phone: ${error.message}`));
  await phone.goto(page.url());
  await phone.locator("form input[type=password]").fill(token);
  await phone.locator("form button[type=submit]").click();
  await phone.locator(".composer textarea").waitFor();
  await phone.locator(".session-bar .toggle-sidebar").click();
  await phone.locator(".sidebar .project").first().waitFor();
  await phone.waitForTimeout(400); // the sidebar slides in
  await tree("on a phone", phone);

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(frames.filter(f => /worktree|sessions/.test(f)).map(f => f.slice(0, 600)).join("\n"));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  server.kill();
}
