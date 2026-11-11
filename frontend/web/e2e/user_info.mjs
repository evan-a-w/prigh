// The settings page's About you section: the text in the account's
// user_info.md, written, saved (Ctrl+Enter) and reverted; a change on another
// page followed, or offered when something is typed there; the open session
// told with the next message; and the section on phones (390x844, 320x640;
// touch, and Chromium as an installed PWA with the notch's insets). Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: user_info.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "user_info");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".prigh"), { recursive: true });
mkdirSync(cwd, { recursive: true });
const infoFile = join(home, ".prigh/user_info.md");
const file = () => {
  try {
    return JSON.stringify(readFileSync(infoFile, "utf8"));
  } catch {
    return "(no user_info.md)";
  }
};

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", cwd],
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
const open = async (viewport, { phone = false, safe = false } = {}) => {
  const options = { viewport, reducedMotion: "reduce" };
  if (phone) {
    options.hasTouch = true;
    if (engineName !== "firefox") options.isMobile = true;
  }
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  if (safe) {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }
  return page;
};

const section = label => console.log(`=== ${engineName}: user info: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-user-info-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const composer = p => p.locator(".composer textarea");
const signIn = async p => {
  await p.goto(url, { timeout: 60000 });
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await composer(p).waitFor();
};
const run = async (p, command) => {
  await composer(p).fill(command);
  await composer(p).press("Control+Enter");
};
const panel = p => p.locator(".settings-panel");
const text = p => p.locator("#about-you-text");
// /user-info: Settings, at About you, once the text is in.
const openInfo = async p => {
  await run(p, "/user-info");
  await p.locator(".settings-dialog").waitFor();
  await p.locator("#about-you-text:not([disabled])").waitFor();
};
const show = async p => {
  console.log((await panel(p).innerText()).replace(/\n+/g, "\n"));
  console.log(`text: ${JSON.stringify(await text(p).inputValue())}`);
};
// Whether Save is on the screen, clear of the edges (the home indicator).
const saveVisible = p =>
  p.evaluate(() => {
    const r = document.querySelector(".about-you-buttons .primary").getBoundingClientRect();
    const v = window.visualViewport;
    return r.top >= 0 && r.bottom <= v.height ? "yes" : `no (${Math.round(r.top)}-${Math.round(r.bottom)} of ${Math.round(v.height)})`;
  });
// The page's elements that overflow it sideways, or are under the edges.
const fits = p =>
  p.evaluate(() => {
    const bad = [];
    const w = document.documentElement.clientWidth;
    for (const el of document.querySelectorAll(".settings-panel button, .settings-panel input, .settings-panel textarea")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      if (r.left < 0 || r.right > w + 0.5) bad.push(`${el.className || el.tagName} ${Math.round(r.left)}-${Math.round(r.right)}`);
    }
    const s = document.querySelector(".settings-panel");
    if (s.scrollWidth > s.clientWidth) bad.push(`panel ${s.scrollWidth} > ${s.clientWidth}`);
    return bad.length ? bad.join(", ") : "yes";
  });
// Targets smaller than a thumb's 44px, and inputs smaller than 16px.
const targets = p =>
  p.evaluate(() => {
    const bad = [];
    for (const el of document.querySelectorAll(".settings-panel :is(button, input, textarea)")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      // A switch shows 28px; its ::after is the target.
      const after = el.matches(".switch") ? parseFloat(getComputedStyle(el, "::after").top) : 0;
      if (r.height - 2 * Math.min(after, 0) < 40) bad.push(`${el.getAttribute("aria-label") ?? el.innerText.trim() ?? el.id} ${Math.round(r.height)}px high`);
      if (el.matches("input, textarea") && parseFloat(getComputedStyle(el).fontSize) < 16) bad.push(`${el.id} font ${getComputedStyle(el).fontSize}`);
    }
    return bad.length ? bad.join(", ") : "all ok";
  });

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  // A session from before anything was written.
  await run(page, "hello");
  await page.getByText("faux reply").first().waitFor();
  await openInfo(page);
  section("empty");
  await show(page);
  await screenshot(page, "empty");

  // Written and saved with Ctrl+Enter: the file, and the acknowledgement.
  await text(page).fill("I'm Lloyd. I work on prigh, in OCaml.\nKeep answers short.");
  await page.locator(".about-you-buttons button", { hasText: "Revert" }).waitFor();
  await screenshot(page, "typed");
  await text(page).press("Control+Enter");
  await page.locator(".about-you-saved").waitFor();
  section("saved");
  await show(page);
  console.log(`file: ${file()}`);
  await screenshot(page, "saved");

  // Revert drops what is typed.
  await text(page).fill("something else");
  await page.locator(".about-you-buttons button", { hasText: "Revert" }).click();
  await page.locator(".about-you-buttons button", { hasText: "Revert" }).waitFor({ state: "detached" });
  console.log(`reverted: ${JSON.stringify(await text(page).inputValue())}`);

  // Another page follows a change; with something typed, it is offered.
  const other = await open({ width: 1280, height: 800 });
  await signIn(other);
  await openInfo(other);
  await text(page).fill("I'm Lloyd. Keep answers short.");
  await text(page).press("Control+Enter");
  await other.waitForFunction(() => document.querySelector("#about-you-text").value === "I'm Lloyd. Keep answers short.");
  section("followed on another page");
  console.log(`text: ${JSON.stringify(await text(other).inputValue())}`);
  await text(other).fill("typed on the other page");
  await text(page).fill("I'm Lloyd. Keep answers short, in British English.");
  await text(page).press("Control+Enter");
  await other.locator(".about-you-elsewhere").waitFor();
  section("changed under a draft");
  await show(other);
  await screenshot(other, "elsewhere");
  await other.locator(".about-you-elsewhere button").click();
  await other.locator(".about-you-elsewhere").waitFor({ state: "detached" });
  console.log(`loaded: ${JSON.stringify(await text(other).inputValue())}`);
  await other.close();

  // The open session, started before any of it, is told with the next message.
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal-backdrop", { state: "detached" });
  await run(page, "and again");
  await page.locator(".note[data-kind=user_info]").waitFor();
  section("the session is told");
  console.log((await page.locator(".note[data-kind=user_info] summary").innerText()).replace(/\s+/g, " ").trim());
  console.log((await page.locator(".note[data-kind=user_info] .note-text").textContent()).trim());
  await screenshot(page, "told");

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone);
    await phone.addStyleTag({ content: "*, *::before, *::after { animation: none !important; transition: none !important; }" });
    await openInfo(phone);
    section(`phone ${tag}`);
    console.log(`fits: ${await fits(phone)}; targets: ${await targets(phone)}; Save on screen: ${await saveVisible(phone)}`);
    await screenshot(phone, `phone-${tag}`);
    await text(phone).fill("I'm Lloyd. Keep answers short, in British English. On a phone.");
    await phone.locator(".about-you-buttons button", { hasText: "Revert" }).waitFor();
    console.log(`typing fits: ${await fits(phone)}; targets: ${await targets(phone)}; Save on screen: ${await saveVisible(phone)}`);
    await screenshot(phone, `phone-${tag}-typed`);
    await phone.locator(".about-you-buttons button", { hasText: "Revert" }).tap();
    await phone.locator(".about-you-buttons button", { hasText: "Revert" }).waitFor({ state: "detached" });
    // The way back: the list of sections.
    await phone.locator(".modal-back").tap();
    await phone.locator(".settings-home-row.settings-tab-user-info").waitFor();
    console.log(`back: ${(await phone.locator(".settings-tab-user-info").innerText()).replace(/\n+/g, " | ")}`);
    await phone.context().close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
