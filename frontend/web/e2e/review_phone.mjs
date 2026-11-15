// The review page on phones, by touch: at 390x844 and 320x640 (Chromium
// and WebKit as mobile browsers, Firefox, which has no isMobile, with the
// viewport and touch), and as an installed PWA on a notched phone
// (Chromium: the safe area's insets emulated). The change is a realistic
// one: long, deeply indented lines (wrapped with a hanging indent), a word
// changed in a line (highlighted on its own), a wide line with no spaces,
// a lock file and a renamed file (folded, approved together). On each
// screen it prints whether the page overflows sideways, any control off
// screen, covered or under 44px, and inputs iOS would zoom into; then what
// the reader sees: the indent of wrapped lines, the words changed, the
// bottom bar (next change, next file, Approve & next), the context's
// expanders, tapping lines to comment on them in the bottom sheet (above
// the keyboard), a quick-action chip, wrapping off and the text size.
// Run by prigh_web.sh; prints normalised results (the expected output is
// in ENGINE.expected) and, with SHOTS, screenshots.
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
if (!engine) throw new Error("usage: review_phone.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "review_phone");
const home = join(dir, "home");
const cwd = join(dir, "repo");
mkdirSync(home, { recursive: true });
mkdirSync(join(cwd, "src"), { recursive: true });
mkdirSync(join(cwd, "docs"), { recursive: true });
setTimeout(() => { console.error("review_phone.mjs: timed out"); process.exit(2); }, 280000).unref();

const git = (...args) => execFileSync("git", args, {
  cwd,
  env: {
    ...process.env, GIT_CONFIG_GLOBAL: "/dev/null", GIT_CONFIG_NOSYSTEM: "1",
    GIT_AUTHOR_NAME: "t", GIT_AUTHOR_EMAIL: "t@t", GIT_COMMITTER_NAME: "t", GIT_COMMITTER_EMAIL: "t@t",
  },
});

// A scheduler: forty lines of preamble, a deeply nested function with long
// lines, forty lines after it.
const filler = (from, n) => Array.from({ length: n }, (_, i) => `let helper_${from + i} x = x + ${from + i}`);
const nested = changed => [
  "let schedule_jobs ~config ~clock jobs =",
  "  List.iter jobs ~f:(fun job ->",
  "    match Job.next_run job ~now:(Clock.now clock) with",
  "    | Some at ->",
  "      if Time.(at > Clock.now clock) then (",
  "        let delay = Time.diff at (Clock.now clock) in",
  changed
    ? "            Scheduler.enqueue_after_delay ~priority:(Job.priority job) ~retries:config.max_retries ~delay ~label:(Job.name job) job"
    : "            Scheduler.enqueue_after_delay ~priority:(Job.priority job) ~retries:config.retries ~delay job",
  changed
    ? "            |> Result.iter_error ~f:(fun err -> Log.error \"could not schedule %s: %s\" (Job.name job) (Error.to_string_hum err)))"
    : "            |> Result.iter_error ~f:(fun err -> Log.error \"could not schedule: %s\" (Error.to_string_hum err)))",
  "      else Scheduler.run_now job",
  "    | None -> ())",
  ";;",
];
const scheduler = changed => [...filler(1, 40), "", ...nested(changed), "", ...filler(41, 40).map(l => changed && l.startsWith("let helper_70 ") ? l.replace("x + 70", "max x 70") : l)].join("\n") + "\n";
writeFileSync(join(cwd, "src", "scheduler.ml"), scheduler(false));
const wide = token => `let fixture = "${token}"\n`;
writeFileSync(join(cwd, "src", "fixture.ml"), wide("a".repeat(30)));
const lock = v => JSON.stringify({
  name: "demo", lockfileVersion: 3,
  packages: Object.fromEntries(Array.from({ length: 40 }, (_, i) => [`node_modules/dep-${i}`, { version: `1.${i}.${v}`, integrity: `sha512-${"x".repeat(40)}${v}` }])),
}, null, 2) + "\n";
writeFileSync(join(cwd, "package-lock.json"), lock(0));
writeFileSync(join(cwd, "docs", "old-guide.md"), "# Guide\n\nHow to run the scheduler.\n\nRun `make`.\n");
writeFileSync(join(cwd, ".gitignore"), ".prigh\n");
git("init", "-q", "-b", "main");
git("add", "-A");
git("commit", "-qm", "start");
writeFileSync(join(cwd, "src", "scheduler.ml"), scheduler(true));
writeFileSync(join(cwd, "src", "fixture.ml"), wide("0123456789abcdef".repeat(12)));
writeFileSync(join(cwd, "package-lock.json"), lock(1));
git("mv", "docs/old-guide.md", "docs/guide.md");

writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: "Ready for review." }]));
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
const section = label => console.log(`=== ${engineName}: review_phone: ${label} ===`);

// What is wrong on the screen: sideways overflow, controls off screen,
// covered or too small, inputs that zoom (as mobile.mjs checks).
const audit = page => page.evaluate(() => {
  const out = [];
  const W = innerWidth, H = innerHeight;
  const sw = document.scrollingElement.scrollWidth;
  if (sw > W + 1) out.push(`overflows sideways: ${sw} > ${W}`);
  for (const box of document.querySelectorAll(".review-page, .review-current, .review-code:not(.scrolls)"))
    if (box.scrollWidth > box.clientWidth + 1) out.push(`${box.className.split(" ")[0]} scrolls sideways`);
  const name = e => (e.getAttribute("aria-label") || e.innerText || e.getAttribute("title") || e.className || e.tagName)
    .toString().trim().replace(/\s+/g, " ").slice(0, 32);
  const visible = e => {
    const r = e.getBoundingClientRect();
    if (r.width < 1 || r.height < 1) return false;
    for (let a = e; a; a = a.parentElement) {
      const s = getComputedStyle(a);
      if (s.visibility === "hidden" || s.display === "none" || s.opacity === "0") return false;
    }
    return !e.closest("[inert], .sr-only");
  };
  const scope = document.querySelector(".review-page") ?? document;
  const controls = [...scope.querySelectorAll("button, a[href], input:not([type=hidden]), textarea, select, [role=tab]")].filter(visible);
  const off = [], covered = [], small = [], zoom = [];
  for (const e of controls) {
    const r = e.getBoundingClientRect();
    // What of the page its scrolling boxes show.
    const box = { left: 0, top: 0, right: W, bottom: H };
    for (let s = e.parentElement; s; s = s.parentElement)
      if (/(auto|scroll)/.test(getComputedStyle(s).overflowY + getComputedStyle(s).overflowX)) {
        const b = s.getBoundingClientRect();
        box.left = Math.max(box.left, b.left); box.top = Math.max(box.top, b.top);
        box.right = Math.min(box.right, b.right); box.bottom = Math.min(box.bottom, b.bottom);
      }
    const inBox = r.left >= Math.max(0, box.left) - 1 && r.right <= Math.min(W, box.right) + 1
      && r.top >= Math.max(0, box.top) - 1 && r.bottom <= Math.min(H, box.bottom) + 1;
    if (r.right > W + 1 || r.left < -1) { off.push(name(e)); continue; }
    if (!inBox) continue;
    const cx = Math.min(Math.max(r.left + r.width / 2, 0), W - 1), cy = Math.min(Math.max(r.top + r.height / 2, 0), H - 1);
    const hit = document.elementFromPoint(cx, cy);
    if (hit && !e.contains(hit) && !hit.contains(e)) covered.push(`${name(e)} under ${name(hit)}`);
    const isText = e.matches("textarea, input:not([type=checkbox]):not([type=radio]), select");
    if (!isText && !e.matches("a") && Math.min(r.width, r.height) < 43.5 && !e.closest(".review-post-text, .review-proposal"))
      small.push(`${name(e)} ${Math.round(r.width)}x${Math.round(r.height)}`);
    if (isText && parseFloat(getComputedStyle(e).fontSize) < 16) zoom.push(`${name(e)} ${getComputedStyle(e).fontSize}`);
  }
  if (off.length) out.push(`off screen: ${off.join(" | ")}`);
  if (covered.length) out.push(`covered: ${covered.join(" | ")}`);
  if (small.length) out.push(`small: ${small.join(" | ")}`);
  if (zoom.length) out.push(`zooms (under 16px): ${zoom.join(" | ")}`);
  return out.length ? out.join("\n") : "ok";
});

const shoot = async (page, tag) => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(200);
  await page.screenshot({ path: join(process.env.SHOTS, `review-phone-${engineName}-${tag}.png`) });
};

// The file on show and the pinned head.
const head = page => page.evaluate(() => {
  const t = document.querySelector("#review-files-toggle");
  return t ? t.innerText.replace(/\s+/g, " ").trim() : "no head";
});

const visit = async (w, h, { safe = false } = {}) => {
  const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
  const options = { viewport: { width: w, height: h }, hasTouch: true };
  if (engineName !== "firefox") options.isMobile = true;
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  const page = await context.newPage();
  page.on("console", m => { if (m.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(m.text())) errors.push(`console: ${m.text()}`); });
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
  if (safe) {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").tap();
  await page.locator(".composer textarea").waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; }" });
  const step = async (label, f) => {
    if (f) await f();
    await page.waitForTimeout(150);
    section(`${tag}: ${label}`);
    console.log(await audit(page));
    await shoot(page, `${tag}-${label.replace(/[^a-z0-9]+/gi, "-")}`);
  };

  await page.keyboard.press("Alt+r");
  await page.locator(".review-page").waitFor();
  await page.locator(".review-folded-file, .review-code tr[data-line]").first().waitFor();
  await step("the renamed file, folded");
  console.log(`head: ${await head(page)}`);
  console.log(`card: ${(await page.locator(".review-folded-file").innerText()).replace(/\s+/g, " ")}`);

  // To the next file by the dock until [path] shows.
  const goTo = async path => {
    for (let i = 0; i < 6 && !(await head(page)).includes(path); i++) {
      const before = await head(page);
      await page.locator(".review-dock-step[aria-label='Next file']").tap();
      await page.waitForFunction(b => {
        const t = document.querySelector("#review-files-toggle");
        return t && t.innerText.replace(/\s+/g, " ").trim() !== b;
      }, before);
    }
    console.log(`head: ${await head(page)}`);
  };
  await goTo("package-lock");
  await step("the lock file, folded");
  console.log(`card: ${(await page.locator(".review-folded-file").innerText()).replace(/\s+/g, " ")}`);

  await goTo("scheduler");
  await page.locator(".review-code tr[data-line] .wd").first().waitFor();
  await step("the long lines");
  // Each wrapped line: where its continuation starts against its first
  // character, in characters.
  console.log(await page.evaluate(() => {
    const out = [];
    for (const td of document.querySelectorAll(".review-code td.text")) {
      const text = td.innerText;
      if (!/enqueue_after_delay|iter_error/.test(text)) continue;
      const range = document.createRange();
      range.selectNodeContents(td);
      const lines = [...range.getClientRects()].filter(r => r.width > 0);
      const tops = [...new Set(lines.map(r => Math.round(r.top)))];
      const lefts = tops.map(t => Math.min(...lines.filter(r => Math.round(r.top) === t).map(r => r.left)));
      const ctx = document.createElement("canvas").getContext("2d");
      ctx.font = getComputedStyle(td).font;
      const ch = ctx.measureText("0".repeat(40)).width / 40;
      const first = lefts[0] + (text.length - text.trimStart().length) * ch;
      const cont = lefts.slice(1).map(l => Math.round((l - first) / ch));
      const kind = td.parentElement.className.match(/add|del/)[0];
      const under = cont.length > 0 && cont.every(c => c >= 1 && c <= 3);
      out.push(`${kind} line ${td.parentElement.dataset.line}: ${under ? "wrapped, continues under its indent" : `continuations at ${cont.join(" ")}ch from its indent`}`);
    }
    return out.join("\n");
  }));
  console.log(`changed words: ${(await page.locator(".review-code .wd").allInnerTexts()).map(t => t.trim()).filter(t => t).join(" | ")}`);
  console.log(`gaps: ${(await page.locator(".review-gap-text").allInnerTexts()).join(" | ")}`);

  const scrollTop = () => page.evaluate(() => Math.round(document.querySelector("#review-scroll").scrollTop));
  const before = await scrollTop();
  await page.locator(".review-dock-step.change[aria-label='Next change']").tap();
  await page.waitForFunction(b => Math.round(document.querySelector("#review-scroll").scrollTop) !== b, before);
  await step("next change");

  // A tap on a line's text selects it; a tap on another extends it.
  await page.locator(".review-code tr.add").first().locator("td.text").tap();
  await page.locator(".review-sheet.selected").waitFor();
  await page.locator(".review-code tr.add").nth(1).locator("td.text").tap();
  await page.waitForFunction(() => document.querySelectorAll(".review-code tr.selected").length >= 2);
  await step("lines selected");
  console.log(`sheet: ${(await page.locator(".review-sheet-head").innerText()).replace(/\s+/g, " ")}`);
  console.log(`chips: ${(await page.locator(".review-chip").allInnerTexts()).join(" | ")}`);

  await page.locator(".review-sheet-comment").tap();
  await page.locator(".review-sheet textarea").waitFor();
  await page.locator(".review-sheet textarea").fill("Should the label be the job's id?");
  await step("writing a comment");
  console.log(`dock while typing: ${await page.locator(".review-dock").count() ? "shown" : "gone"}`);
  await page.keyboard.press("Escape");
  await page.locator(".review-sheet").waitFor({ state: "detached" });

  await page.locator(".review-display-button").tap();
  await page.locator(".review-sheet.display").waitFor();
  await step("display options");
  await page.locator(".review-sheet.display .segment", { hasText: "Scroll" }).tap();
  await page.locator(".review-sheet.display .review-size-step").last().tap();
  await page.locator(".app.code-large").waitFor();
  await page.locator(".review-sheet.display .btn", { hasText: "Done" }).tap();
  await page.locator(".review-code.scrolls").waitFor();
  await step("scrolling, larger");
  console.log(`file scrolls sideways: ${await page.evaluate(() => { const c = document.querySelector(".review-code.scrolls"); return c.scrollWidth > c.clientWidth; })}`);
  await page.locator(".review-display-button").tap();
  await page.locator(".review-sheet.display .segment", { hasText: "Wrap" }).tap();
  await page.locator(".review-sheet.display .review-size-step").first().tap();
  await page.locator(".app:not(.code-large)").waitFor();
  await page.keyboard.press("Escape");
  await page.locator(".review-code:not(.scrolls)").waitFor();
  await page.keyboard.press("Alt+r");
  await page.locator(".review-page").waitFor({ state: "detached" });
  await context.close();
};

try {
  await visit(390, 844);
  await visit(320, 640);
  if (engineName === "chromium") await visit(390, 844, { safe: true });
  if (errors.length) {
    section("errors");
    console.log(errors.join("\n"));
  }
} catch (e) {
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
