// The session card in the chat's top-right corner, against a real git
// repository and a subagent: what it lists, where it sits against the
// transcript's column as the window narrows (the column moves left only by
// what the margin lacks; below an 820px pane the card covers its corner),
// hiding it, a subagent's chat in the session's place and back (Esc), and
// on phones a sheet from the bottom, opened from the ⋯ sheet. Run by
// prigh_web.sh; prints normalised snapshots (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, realpathSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: session_card.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "session_card"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "session_card"));
const home = join(dir, "home");
normalVerbosity(home);
const repo = join(dir, "prigh");
const feat = join(dir, "prigh-feat");
for (const d of [home, repo]) mkdirSync(d, { recursive: true });
writeFileSync(join(dir, "script.json"), JSON.stringify([
  {
    text: "handing it over",
    tool_calls: [{ id: "s1", name: "subagent", arguments: { task: "Count the modules" } }],
  },
  ...Array(10).fill({ text: "There are 42 modules." }),
]));

const git = (cwd, ...args) =>
  execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
writeFileSync(join(repo, "README.md"), "# Repo\n");
git(repo, "init", "-q", "-b", "main");
git(repo, "add", "README.md");
git(repo, "commit", "-q", "-m", "start");
git(repo, "worktree", "add", "-q", "-b", "feat/ui", feat);
writeFileSync(join(feat, "README.md"), "# Repo\n\nChanged.\n");

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", repo],
  { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh" },
    stdio: ["ignore", "ignore", "pipe"] },
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
const watch = p => {
  p.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  p.on("pageerror", error => errors.push(`page: ${error.message}`));
};
const context = await browser.newContext({ viewport: { width: 1440, height: 900 } });
const page = await context.newPage();
watch(page);

const clean = text => text
  .split(dir).join("<DIR>")
  .replace(/\b(now|\d+[smhd])$/gm, "<age>")
  .replace(/^\d\d?:\d\d$/gm, "<time>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: session card: ${label} ===`);
let shot = 0;
const screenshot = async (label, target = page) => {
  if (!process.env.SHOTS) return;
  shot++;
  await target.screenshot({
    path: join(process.env.SHOTS, `${engineName}-session-card-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const editor = () => page.locator(".composer textarea");
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));

// Where the card is against the transcript's column and the composer, in
// CSS pixels of the main pane: how far the column moved, and whether the
// card stays clear of it.
const layout = async (label, width) => {
  await page.setViewportSize({ width, height: 900 });
  await page.waitForFunction(w => window.innerWidth === w, width);
  // Two frames: the container query applies after the resize's layout.
  await page.evaluate(() => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r))));
  section(`${label}, ${width}px wide`);
  console.log(await page.evaluate(() => {
    const box = e => e?.getBoundingClientRect();
    const main = box(document.querySelector(".main"));
    const card = box(document.querySelector(".session-card"));
    const chat = document.querySelector(".chat");
    const entries = box(document.querySelector(".chat .entries"));
    const composer = box(document.querySelector(".composer-box"));
    // Where the column would be, centred in the log, against where it is.
    const column = parseFloat(getComputedStyle(document.querySelector(".chat .entries")).maxWidth);
    const centred = Math.max(16, (chat.clientWidth - column) / 2);
    const moved = centred - (entries.left - box(chat).left) > 4;
    const lines = [`pane ${Math.round(main.width)}px, the column ${moved ? "moved left" : "centred"}`];
    if (!card) return [...lines, "no card"].join("\n");
    lines.push(`card ${Math.round(card.width)}px wide, ${Math.round(main.right - card.right)}px from the pane's edge`);
    const gap = Math.round(card.left - entries.right);
    lines.push(gap >= 12 ? `the column clears it (${gap >= 40 ? "40px or more" : "12px or more"})` : "the card covers the column's corner");
    lines.push(`the composer's left edge lines up with the column's: ${Math.abs(composer.left - entries.left) < 40}`);
    lines.push(`the column keeps ${entries.left - main.left >= 12 ? "its left margin" : "no left margin"}`);
    return lines.join("\n");
  }));
  await screenshot(`${label} ${width}`);
};

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await editor().waitFor();

  await editor().fill("Count the modules with a subagent");
  await editor().press("Enter");
  await page.locator(".chat", { hasText: "There are 42 modules." }).first().waitFor();
  await idle();
  await page.locator(".session-card-agent").waitFor();
  // Into the repository's other worktree: the session's own, so the card
  // lists it.
  await editor().fill("/worktree open feat/ui");
  await editor().press("Control+Enter");
  await page.locator("[aria-controls=session-card-worktrees-content]").waitFor();
  section("the card");
  console.log(clean(await page.locator(".session-card").innerText()));
  await page.locator("[aria-controls=session-card-worktrees-content]").click();
  await page.locator(".session-card-worktree").first().waitFor();
  section("the worktrees unfolded");
  console.log(clean(await page.locator(".session-card").innerText()));
  console.log(await page.locator(".session-card-worktree").evaluateAll(rows =>
    rows.map(r => `${r.innerText.replace(/\n/g, " ")}${r.getAttribute("aria-current") ? " (here)" : ""}`).join("\n")));

  for (const width of [1600, 1440, 1100, 900]) await layout("beside the chat", width);

  await page.setViewportSize({ width: 1440, height: 900 });
  await page.locator(".session-card-toggle").click();
  await page.waitForSelector(".session-card", { state: "detached" });
  await layout("hidden from the chrome", 1440);
  await page.locator(".session-card-toggle").click();
  await page.locator(".session-card").waitFor();

  // A subagent's chat in the session's place: the composer writes to it;
  // the breadcrumb and the card say where it is.
  const composerBox = () => page.evaluate(() => JSON.stringify(document.querySelector(".composer-box").getBoundingClientRect()));
  const sessionComposer = await composerBox();
  await page.locator(".session-card-agent").click();
  await page.locator(".subagent-chat").waitFor();
  await page.locator(".subagent-chat", { hasText: "There are 42 modules." }).first().waitFor();
  await page.locator(".session-card-parent").first().waitFor();
  section("a subagent's chat");
  console.log(clean(await page.locator("#chat").innerText()));
  console.log(`breadcrumb: ${clean(await page.locator(".session-crumbs").innerText()).replace(/\n/g, " ")}`);
  console.log(`card: ${clean(await page.locator(".session-card").innerText()).replace(/\n/g, " | ")}`);
  console.log(`composer: ${await page.locator(".composer textarea").count()}, ` +
    `focus in the editor: ${await page.evaluate(() => document.activeElement?.matches(".composer textarea"))}`);
  console.log(await page.evaluate(() => {
    const prompt = document.querySelector(".subagent-chat .msg.user");
    const bubble = prompt && [prompt, ...prompt.querySelectorAll("*")].find(e =>
      getComputedStyle(e).backgroundColor !== "rgba(0, 0, 0, 0)");
    return `its task as a filled prompt: ${!!bubble}`;
  }));
  console.log(`the composer where it is in the session's chat: ${await composerBox() === sessionComposer}`);
  await screenshot("a subagent's chat");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".subagent-chat", { state: "detached" });
  await editor().waitFor();
  section("Esc: back to the session");
  console.log(`the session's transcript: ${await page.locator(".chat", { hasText: "Count the modules with a subagent" }).count() > 0}, ` +
    `focus in the editor: ${await page.evaluate(() => document.activeElement?.matches(".composer textarea"))}`);

  // Phones: no card beside the chat; the ⋯ sheet opens it as a sheet.
  for (const [w, h] of [[390, 844], [320, 640]]) {
    const phone = await browser.newContext({
      viewport: { width: w, height: h },
      hasTouch: true,
      ...(engineName === "firefox" ? {} : { isMobile: true }),
    });
    const p = await phone.newPage();
    watch(p);
    await p.goto(page.url());
    await p.locator("form input[type=password]").fill(token).catch(() => {});
    if (await p.locator("form button[type=submit]").count()) await p.locator("form button[type=submit]").click();
    await p.locator(".composer textarea").waitFor();
    await p.locator(".chat", { hasText: "There are 42 modules." }).first().waitFor();
    section(`a phone, ${w}x${h}`);
    console.log(`card beside the chat: ${await p.locator(".session-card-anchor").count()}`);
    await p.locator(".more-toggle").tap();
    await p.locator(".session-card-row").tap();
    const sheet = p.locator(".session-card.sheet");
    await sheet.waitFor();
    // It slides in: measured once its animations (and its parent's) are done.
    await p.waitForFunction(() => {
      const s = document.querySelector(".session-card.sheet");
      return s && document.getAnimations().every(a => a.playState !== "running");
    });
    console.log(clean(await sheet.innerText()));
    console.log(await p.evaluate(() => {
      const s = document.querySelector(".session-card.sheet").getBoundingClientRect();
      const rows = [...document.querySelectorAll(".session-card.sheet button")]
        .filter(b => b.offsetParent)
        .map(b => b.getBoundingClientRect().height);
      return `inside the screen: ${s.left >= 0 && s.right <= innerWidth + 1 && s.bottom <= innerHeight + 1}; ` +
        `rows 44px or more: ${rows.every(h => h >= 44)} (${rows.length})`;
    }));
    await screenshot(`phone ${w}x${h} sheet`, p);
    await p.keyboard.press("Escape");
    await p.waitForSelector(".session-card.sheet", { state: "detached" });
    console.log("Esc closes it");
    await phone.close();
  }

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
