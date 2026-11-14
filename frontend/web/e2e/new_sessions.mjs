// The settings page's New sessions section: every default set from it (the
// model from the picker, thinking, mode, the tool host from the hosts
// connected, the starting directory, a fallback model, a budget), saved in
// config.json, followed by a second browser at once, and a new session
// started with them all while the one open keeps its own; then each reset to
// the built-in. On a desktop, on phones (390x844, 320x640) and as an
// installed PWA (Chromium: the notch and home indicator's insets). Run by
// prigh_web.sh; prints normalised snapshots (the expected output is in
// ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync, realpathSync } from "node:fs";
import { hostname } from "node:os";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: new_sessions.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
mkdirSync(join(process.env.TEST_DIR, "new_sessions"), { recursive: true });
const dir = realpathSync(join(process.env.TEST_DIR, "new_sessions"));
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const laptopHome = join(dir, "laptop-home");
for (const d of [home, cwd, laptopHome, join(laptopHome, "proj")]) mkdirSync(d, { recursive: true });

const waitForLine = (proc, pattern, what) => new Promise((resolve, reject) => {
  let err = "";
  proc.stderr.on("data", data => {
    err += data;
    const m = err.match(pattern);
    if (m) resolve(m[1]);
  });
  proc.on("exit", code => reject(new Error(`${what} exited (${code}):\n${err}`)));
});
const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", cwd],
  { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh" }, stdio: ["ignore", "ignore", "pipe"] },
);
const url = await waitForLine(server, /^prigh: prigh-web on (\S+)$/m, "the backend");
// A tool host, for the default host to be.
const laptopEnv = { ...process.env, HOME: laptopHome, TMUX_TMPDIR: dir, SHELL: "/bin/sh" };
for (const name of ["PRIGH_CONNECT", "PRIGH_TOKEN", "PRIGH_USER", "PRIGH_WEB_LISTEN"]) delete laptopEnv[name];
const laptop = spawn(
  backend,
  ["web", "-connect", new URL(url).host, "-token", token, "-listen", "127.0.0.1:0", "-web-root", site,
    "-cwd", laptopHome, "-name", "laptop", "-host-id", "host-laptop"],
  { env: laptopEnv, stdio: ["ignore", "ignore", "pipe"] },
);
await waitForLine(laptop, /^prigh web: prigh-web for \S+ on (\S+)$/m, "prigh web");

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

const section = label => console.log(`=== ${engineName}: new sessions: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-new-sessions-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
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
const openSection = async p => {
  await run(p, "/settings new-sessions");
  await p.locator(".setting.default-model").waitFor();
};
const normalise = text =>
  text.replace(/\n+/g, "\n").replaceAll(cwd, "<cwd>").replaceAll(dir, "<dir>")
    .replace(new RegExp(`\\b${hostname().replace(/\W/g, "\\$&")}\\b`, "g"), "<host>");
const show = async p => console.log(normalise(await panel(p).innerText()));
const configFile = () => {
  const c = JSON.parse(readFileSync(join(home, ".prigh/config.json"), "utf8"));
  const keys = ["default_model", "default_thinking", "approval_mode", "default_host", "default_cwd", "fallback_models", "max_budget_usd"];
  return keys.map(k => `${k}=${JSON.stringify(c[k] ?? null)}`).join(" ");
};
const toast = (p, text) => p.locator(".toast", { hasText: text }).last().waitFor();
const pick = async (p, opener, query, item) => {
  await p.locator(opener).click();
  await p.locator(".picker-dialog #picker-input").fill(query);
  await p.locator(".picker-dialog .picker-item", { hasText: item }).first().waitFor();
  await p.locator(".picker-dialog #picker-input").press("Enter");
  await p.locator(".setting.default-model").waitFor();
};
// The session's chips (model, host, thinking, mode) and where it is.
const chips = p =>
  p.evaluate(() => {
    const t = s => document.querySelector(s)?.getAttribute("title") ?? document.querySelector(s)?.innerText.trim() ?? "-";
    return [
      `model: ${t(".model-chip")}`,
      `thinking: ${t(".thinking-chip")}`,
      `mode: ${t(".mode-chip")}`,
      `host: ${t(".host-chip")}`,
    ].join("\n");
  });
const fits = p =>
  p.evaluate(() => {
    const bad = [];
    const w = document.documentElement.clientWidth;
    const h = window.visualViewport?.height ?? innerHeight;
    for (const el of document.querySelectorAll(".settings-panel button, .settings-panel input")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      if (r.left < 0 || r.right > w + 0.5) bad.push(`${el.getAttribute("aria-label") ?? el.innerText.trim()} ${Math.round(r.left)}-${Math.round(r.right)}`);
    }
    const s = document.querySelector(".settings-panel");
    if (s.scrollWidth > s.clientWidth) bad.push(`panel ${s.scrollWidth} > ${s.clientWidth}`);
    const head = document.querySelector(".settings-dialog .modal-header, .settings-dialog h2")?.getBoundingClientRect();
    if (head && (head.top < 0 || head.bottom > h)) bad.push("header off screen");
    return bad.length ? bad.join(", ") : "yes";
  });
const targets = p =>
  p.evaluate(() => {
    const bad = [];
    // A number field's target is its whole segment, the label around it.
    for (const el of document.querySelectorAll(".settings-panel :is(button, input:not(.number-field-input), .number-field)")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      // A switch's target reaches past it: its ::after.
      const after = getComputedStyle(el, "::after");
      const reach = el.matches(".switch") && after.position === "absolute" ? -parseFloat(after.top) - parseFloat(after.bottom) : 0;
      if (r.height + reach < 40) bad.push(`${el.getAttribute("aria-label") ?? el.innerText.trim()} ${Math.round(r.height)}px high`);
      const input = el.matches("input") ? el : el.querySelector("input");
      if (input && parseFloat(getComputedStyle(input).fontSize) < 16) bad.push(`${input.id} font ${getComputedStyle(input).fontSize}`);
    }
    return bad.length ? bad.join(", ") : "all ok";
  });

const page = await open({ width: 1280, height: 900 });
try {
  await signIn(page);
  await run(page, "first");
  await page.locator(".session-item .session-title", { hasText: "first" }).waitFor();
  await page.locator(".host-chip").waitFor();
  section("the session open, before");
  console.log(normalise(await chips(page)));
  await openSection(page);
  section("the built-ins");
  await show(page);
  await screenshot(page, "built-ins");

  // A second browser follows every change at once.
  const other = await open({ width: 1280, height: 900 });
  await signIn(other);
  await openSection(other);

  await pick(page, ".default-model-choice", "fable", "Fable");
  await toast(page, "New sessions start with");
  await page.locator(".setting.default-thinking").getByRole("button", { name: "High", exact: true }).click();
  await toast(page, "think at high");
  await page.locator(".setting.default-mode .segment", { hasText: "Ask" }).click();
  await toast(page, "Ask mode");
  await pick(page, ".default-host-choice", "lap", "laptop");
  await toast(page, "start on laptop");
  await page.locator("#default-dir").fill("~/proj");
  await page.locator("#default-dir").press("Enter");
  await toast(page, "~/proj");
  await pick(page, ".fallback-add", "flash", "Flash");
  await page.locator(".fallback-list li").first().waitFor();
  // The budget is off: its switch sets $5, typed over.
  await page.locator(".setting.default-budget .switch").click();
  await toast(page, "$5.00");
  await page.locator("#default-budget").fill("2.50");
  await page.locator("#default-budget").press("Enter");
  await toast(page, "$2.50");
  section("all set");
  await show(page);
  console.log(configFile());
  await screenshot(page, "all-set");
  await other.waitForFunction(() => document.querySelector("#default-budget")?.value === "2.50");
  await other.locator(".setting.default-host", { hasText: "laptop" }).waitFor();
  section("the other browser");
  await show(other);
  await other.close();

  // Refused: a budget that is not one.
  await page.locator("#default-budget").fill("lots");
  await page.locator("#default-budget").press("Enter");
  await page.locator(".setting.default-budget .setting-note, .setting.default-budget [role=alert]").first().waitFor();
  section("refused");
  console.log(normalise(await page.locator(".setting.default-budget").innerText()));
  await page.locator("#default-budget").fill("2.50");
  await page.locator("#default-budget").press("Enter");

  // The open session kept its own; a new one starts with them all.
  await page.keyboard.press("Escape");
  await page.locator(".settings-dialog").waitFor({ state: "detached" });
  section("the session open keeps its own");
  console.log(normalise(await chips(page)));
  await page.locator(".new-session").click();
  await page.waitForFunction(() => !document.querySelector(".new-session.opening"));
  await run(page, "second");
  await page.locator(".session-item .session-title", { hasText: "second" }).waitFor();
  await page.locator(".host-chip", { hasText: "laptop" }).waitFor();
  await page.locator(".model-chip", { hasText: "Fable" }).waitFor();
  section("a new session");
  console.log(normalise(await chips(page)));
  await run(page, "/budget");
  await toast(page, "spent");
  console.log(normalise(await page.locator(".toast", { hasText: "spent" }).last().innerText()));
  await run(page, "/fallback");
  await toast(page, "Fallback:");
  console.log(normalise(await page.locator(".toast", { hasText: "Fallback:" }).last().innerText()));
  await screenshot(page, "new-session");

  // Phones, and the PWA's insets: the section fits, every control is a
  // target; the host picker opens over it.
  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone);
    await phone.addStyleTag({ content: "*, *::before, *::after { animation: none !important; transition: none !important; } .toasts { display: none !important; }" });
    await openSection(phone);
    section(`phone ${tag}`);
    console.log(`fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}`);
    await phone.locator(".setting.default-budget").scrollIntoViewIfNeeded();
    await screenshot(phone, `phone-${tag}-limits`);
    await phone.locator(".default-host-choice").tap();
    await phone.locator(".picker-dialog .picker-item", { hasText: "laptop" }).waitFor();
    await screenshot(phone, `phone-${tag}-host-picker`);
    await phone.keyboard.press("Escape");
    await phone.locator(".setting.default-host").waitFor();
    console.log(`after the picker: ${await phone.locator(".settings-dialog").count() ? "back on the settings" : "closed"}`);
    await phone.locator(".modal-back").tap();
    await phone.locator(".settings-home-row.settings-tab-new-sessions").waitFor();
    console.log(`back: ${(await phone.locator(".settings-tab-new-sessions").innerText()).replace(/\n+/g, " | ")}`);
    await phone.context().close();
  }

  // Back to the built-ins, each by its own reset.
  await openSection(page);
  for (const row of ["default-model", "default-thinking", "default-mode", "default-host", "default-dir", "default-fallback"]) {
    await page.locator(`.setting.${row} .default-reset`).click();
    await page.locator(`.setting.${row} .default-reset`).waitFor({ state: "detached" });
  }
  await page.locator(".setting.default-budget .switch").click();
  await page.locator("#default-budget").waitFor({ state: "detached" });
  section("reset");
  await show(page);
  console.log(configFile());

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => laptop.kill(), () => server.kill());
}
