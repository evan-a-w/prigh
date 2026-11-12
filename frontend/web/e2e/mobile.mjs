// prigh-web on phones, by touch: at 390x844 and 320x640 (Chromium as a
// mobile browser, Firefox, which has no isMobile, with the viewport and
// touch), and as an installed PWA on a notched phone (Chromium: the safe
// area's insets emulated). On each screen (the chat, the ⋯ sheet, the
// drawer, the composer's menus, the settings' list and a section of it, the
// files, a picker, the review page and its files) it prints whether the
// page overflows sideways, any control that is off screen, covered by
// another or smaller than a thumb (44px on its shorter side, 40px in the
// rows in [dense]), and inputs that iOS would zoom into (under 16px). Then:
// the one strip of chrome is under the notch and the composer over the home
// indicator, the settings go a level down and back by tap, and the keyboard
// (a shorter visual viewport) keeps the composer in view. Run by
// prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected) and, with SHOTS, screenshots.
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
if (!engine) throw new Error("usage: mobile.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "mobile");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(join(cwd, "src"), { recursive: true });
writeFileSync(join(cwd, "server.ml"), "let port = 8080\n");
writeFileSync(join(cwd, "README.md"), "# demo\n");
// A repository with a change, for the review page.
const git = (...args) => execFileSync("git", args, {
  cwd,
  env: {
    ...process.env, GIT_CONFIG_GLOBAL: "/dev/null", GIT_CONFIG_NOSYSTEM: "1",
    GIT_AUTHOR_NAME: "t", GIT_AUTHOR_EMAIL: "t@t", GIT_COMMITTER_NAME: "t", GIT_COMMITTER_EMAIL: "t@t",
  },
});
git("init", "-q", "-b", "main");
git("add", "-A");
git("commit", "-qm", "start");
writeFileSync(join(cwd, "README.md"), "# demo\n\nA change to review.\n");
setTimeout(() => { console.error("mobile.mjs: timed out"); process.exit(2); }, 280000).unref();

const long = "/home/someone/projects/a-very-long-directory-name/with/many/levels/of/nesting/and-a-file-name-without-breaks.ml";
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { thinking: "The port is probably a literal.", text: "Looking at the server first.",
      tool_calls: [{ id: "r1", name: "read", arguments: { path: "server.ml" } }] },
    { text: `It is a literal in \`${long}\`. Here is a long URL: https://example.com/${"x".repeat(90)}`,
      tool_calls: [{ id: "b1", name: "bash", arguments: { command: `cat server.ml && echo ${"y".repeat(120)}` } }] },
    { text: "Done.\n\n```\n" + "let configured_port = Sys.getenv_opt \"PORT\" |> Option.value ~default:\"8080\" |> int_of_string\n" + "```" },
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
const section = label => console.log(`=== ${engineName}: mobile: ${label} ===`);

// What is wrong on the screen now, as stable lines (empty: nothing).
// Every choice control fits its card, each choice whole (none clipped or
// on top of another), and no row has a gap between its words and control.
const choicesFit = page => page.evaluate(() => {
  const bad = [];
  for (const seg of document.querySelectorAll(".settings-panel .segmented")) {
    const card = seg.closest(".settings-card").getBoundingClientRect();
    const s = seg.getBoundingClientRect();
    if (s.left < card.left - 0.5 || s.right > card.right + 0.5) bad.push(`${seg.getAttribute("aria-label")} past its card`);
    for (const b of seg.querySelectorAll(".segment")) {
      const r = b.getBoundingClientRect();
      if (r.left < s.left - 0.5 || r.right > s.right + 0.5 || r.bottom > s.bottom + 0.5) bad.push(`${b.innerText.split("\n")[0]} clipped`);
      if (b.scrollWidth > b.clientWidth + 1) bad.push(`${b.innerText.split("\n")[0]} overflows`);
      if (r.height < 43.5) bad.push(`${b.innerText.split("\n")[0]} under 44px`);
    }
    const row = seg.closest(".setting");
    const text = row.querySelector(".setting-text").getBoundingClientRect();
    if (s.top - text.bottom > 24) bad.push(`${seg.getAttribute("aria-label")}: a ${Math.round(s.top - text.bottom)}px gap`);
  }
  const list = document.querySelector(".settings-panel .choice-list");
  const one = list && new Set([...list.querySelectorAll(".segment")].map(b => Math.round(b.getBoundingClientRect().left))).size === 1;
  return `choice controls fit: ${bad.length ? bad.join("; ") : "true"}` + (list ? `; the status styles a list: ${one}` : "");
});
// Rows a phone keeps on screen with more in them than 44px targets leave
// room for, whose targets are 40px: the composer's (+, the model, the host,
// the status, the microphone, send).
const dense = ".composer";
const audit = page => page.evaluate(dense => {
  const out = [];
  const W = innerWidth, H = innerHeight;
  const sw = document.scrollingElement.scrollWidth;
  if (sw > W + 1) out.push(`overflows sideways: ${sw} > ${W}`);
  // Nor do the transcript and the dialogs' bodies (a long line in them
  // scrolls in its own box, a code block's, or wraps).
  for (const box of document.querySelectorAll("#chat, .modal-body, .settings-panel"))
    if (box.scrollWidth > box.clientWidth + 1) out.push(`${box.id ? "#" + box.id : box.className.split(" ")[0]} scrolls sideways`);
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
  const top = [...document.querySelectorAll(".modal, .chrome-menu, .chip-popover, .popup, .sidebar, .files-panel, .review-page")]
    .filter(visible).pop();
  const scope = top ?? document;
  const controls = [...scope.querySelectorAll("button, a[href], input:not([type=hidden]), textarea, select, [role=tab]")].filter(visible);
  const off = [], covered = [], small = [], zoom = [];
  for (const e of controls) {
    const r = e.getBoundingClientRect();
    // Inside a scroller, what is past its edge is scrolled to, not lost.
    let scroller = e.parentElement;
    while (scroller && !/(auto|scroll)/.test(getComputedStyle(scroller).overflowY + getComputedStyle(scroller).overflowX)) scroller = scroller.parentElement;
    const box = scroller ? scroller.getBoundingClientRect() : { left: 0, top: 0, right: W, bottom: H };
    const inBox = r.left >= Math.max(0, box.left) - 1 && r.right <= Math.min(W, box.right) + 1
      && r.top >= Math.max(0, box.top) - 1 && r.bottom <= Math.min(H, box.bottom) + 1;
    if (r.right > W + 1 || r.left < -1) { off.push(name(e)); continue; }
    if (!inBox) continue;
    const cx = Math.min(Math.max(r.left + r.width / 2, 0), W - 1), cy = Math.min(Math.max(r.top + r.height / 2, 0), H - 1);
    const hit = document.elementFromPoint(cx, cy);
    if (hit && !e.contains(hit) && !hit.contains(e) && !e.closest("label")?.contains(hit)) covered.push(`${name(e)} under ${name(hit)}`);
    const isText = e.matches("textarea, input:not([type=checkbox]):not([type=radio]):not([type=color]):not([type=range]), select");
    const min = e.closest(dense) ? 40 : 44;
    if (!isText && !e.matches("a") && Math.min(r.width, r.height) < min - 0.5 && !e.closest(".message, .md, .tool-body, .status-details"))
      small.push(`${name(e)} ${Math.round(r.width)}x${Math.round(r.height)}`);
    if (isText && parseFloat(getComputedStyle(e).fontSize) < 16) zoom.push(`${name(e)} ${getComputedStyle(e).fontSize}`);
  }
  if (off.length) out.push(`off screen: ${off.join(" | ")}`);
  if (covered.length) out.push(`covered: ${covered.join(" | ")}`);
  if (small.length) out.push(`small: ${small.join(" | ")}`);
  if (zoom.length) out.push(`zooms (under 16px): ${zoom.join(" | ")}`);
  return out.length ? out.join("\n") : "ok";
}, dense);

// The chrome's pieces don't overlap each other or the transcript.
const chrome = page => page.evaluate(() => {
  const r = s => document.querySelector(s)?.getBoundingClientRect();
  const strip = r(".chat-chrome");
  const chat = r("#chat");
  const composer = r(".composer");
  const parts = [...document.querySelectorAll(".chat-chrome .chrome-left > *, .chat-chrome .chrome-center > *, .chat-chrome .chrome-right > *")]
    .map(e => e.getBoundingClientRect()).filter(b => b.width > 0);
  let overlap = false;
  for (let i = 0; i < parts.length; i++) for (let j = i + 1; j < parts.length; j++) {
    const a = parts[i], b = parts[j];
    if (a.left < b.right - 0.5 && b.left < a.right - 0.5) overlap = true;
  }
  return [
    `strip: ${strip ? `${Math.round(strip.top)} to ${Math.round(strip.bottom)}` : "none"}; its pieces overlap: ${overlap}`,
    `the transcript starts under it: ${chat && strip ? chat.top >= strip.bottom - 0.5 : "?"}`,
    `the composer ends at ${composer ? Math.round(innerHeight - composer.bottom) : "?"}px from the bottom`,
  ].join("\n");
});

const shoot = async (page, tag) => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(250);
  await page.screenshot({ path: join(process.env.SHOTS, `mobile-${engineName}-${tag}.png`) });
};

let first = true;
let sessionUrl = null;
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
  await page.goto(sessionUrl ?? url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").tap();
  const editor = page.locator(".composer textarea");
  await editor.waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; }" });
  if (first) {
    first = false;
    await editor.fill("make the port configurable");
    await editor.press("Enter");
    await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Done."), null, { timeout: 60000 });
    sessionUrl = page.url();
  } else {
    await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Done."));
  }
  const step = async (label, f) => {
    if (f) await f();
    await page.waitForTimeout(150);
    section(`${tag}: ${label}`);
    console.log(await audit(page));
    await shoot(page, `${tag}-${label.replace(/[^a-z0-9]+/gi, "-")}`);
  };
  const close = async () => {
    await page.keyboard.press("Escape");
    await page.waitForTimeout(150);
  };

  await step("the chat");
  section(`${tag}: the chrome`);
  console.log(await chrome(page));

  await step("the ⋯ sheet", () => page.getByRole("button", { name: "Panels and settings" }).tap());
  section(`${tag}: the sheet's rows`);
  console.log(await page.locator(".chrome-menu .sheet-row-name").evaluateAll(es => es.map(e => e.innerText).join(", ")));
  // Settings from the sheet, by tap: the list, a section, back.
  await page.locator(".chrome-menu .settings-row").tap();
  await step("the settings' list");
  section(`${tag}: the settings`);
  console.log(await page.locator(".settings-home-name").evaluateAll(es => es.map(e => e.innerText).join(", ")));
  await page.locator(".settings-home-row[data-settings-tab=appearance]").tap();
  await step("settings: appearance");
  console.log(`title: ${await page.locator(".modal-head h2").innerText()}; back: ${await page.locator(".modal-back").getAttribute("title")}`);
  console.log(await choicesFit(page));
  await page.locator(".modal-back").tap();
  await page.locator(".settings-home").waitFor();
  console.log(`back to the list: ${await page.locator(".settings-home-row").count() > 0}`);
  await page.locator(".settings-home-row[data-settings-tab=models]").tap();
  await step("settings: models");
  console.log(await choicesFit(page));
  await page.getByRole("button", { name: "Close" }).first().tap();
  await page.waitForSelector(".modal", { state: "detached" });

  await step("the drawer", () => page.getByRole("button", { name: "Sessions" }).first().tap());
  await page.locator(".scrim").tap({ position: { x: w - 10, y: h / 2 } });
  await page.waitForTimeout(250);

  await step("the model menu", () => page.locator(".composer .model-chip").tap());
  section(`${tag}: the model menu is a sheet`);
  console.log(await page.evaluate(() => {
    const r = document.querySelector("#chip-popover")?.getBoundingClientRect();
    return r ? `full width: ${Math.round(r.width) === innerWidth}; at the bottom: ${Math.abs(r.bottom - innerHeight) < 1}` : "no menu";
  }));
  await close();

  await step("/ completion", () => editor.fill("/"));
  await editor.fill("");

  await step("the files", async () => {
    await page.getByRole("button", { name: "Panels and settings" }).tap();
    await page.locator(".chrome-menu .files-sheet-row").tap();
    await page.locator(".files-panel").waitFor();
  });
  await step("the review page", async () => {
    await page.locator(".files-panel .files-head .review-open").tap();
    await page.locator(".review-page").waitFor();
  });
  await step("the review's files", async () => {
    await page.locator("#review-files-toggle").tap();
    await page.locator(".review-files").waitFor();
  });
  await page.keyboard.press("Alt+r");
  await page.locator(".review-page").waitFor({ state: "detached" });
  if (await page.locator(".files-panel").count())
    await page.getByRole("button", { name: "Close files" }).tap();

  // The keyboard: the visual viewport shrinks (as a phone's keyboard does);
  // the composer stays in it.
  if (engineName !== "firefox") {
    section(`${tag}: the keyboard`);
    await editor.focus();
    if (engineName === "chromium") {
      const cdp = await context.newCDPSession(page);
      await cdp.send("Emulation.setVisibleSize", { width: w, height: Math.round(h * 0.55) }).catch(() => {});
    }
    await page.setViewportSize({ width: w, height: Math.round(h * 0.55) });
    await page.waitForTimeout(250);
    console.log(await page.evaluate(() => {
      const c = document.querySelector(".composer").getBoundingClientRect();
      const vh = window.visualViewport?.height ?? innerHeight;
      return `the composer in view: ${c.bottom <= vh + 1 && c.top >= 0}`;
    }));
    await shoot(page, `${tag}-keyboard`);
  }
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
