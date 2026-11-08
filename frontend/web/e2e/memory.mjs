// The settings page's Memory section: the notes in the account's
// memory.json, filtered, edited and deleted (asked first); the switch, saved
// in prefs.json, and /memory off; a change on another page followed; and the
// section on phones (390x844, 320x640; touch, and Chromium as an installed
// PWA with the notch's insets). Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: memory.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "memory");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".prigh"), { recursive: true });
mkdirSync(cwd, { recursive: true });
const memoryFile = join(home, ".prigh/memory.json");
writeFileSync(
  memoryFile,
  JSON.stringify({
    next: 4,
    notes: [
      { id: "m1", title: "editor", text: "helix, with the default keymap", saved: "2025-10-01 09:30:00.000000Z" },
      { id: "m2", text: "Deploys go through the staging cluster first, then production on Tuesdays", saved: "2025-10-02 14:05:00.000000Z" },
      { id: "m3", title: "shell", text: "fish", saved: "2025-10-03 08:00:00.000000Z" },
    ],
  }),
);

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

const section = label => console.log(`=== ${engineName}: memory: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-memory-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
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
// /memory: Settings, at Memory, once the notes are in.
const openMemory = async p => {
  await run(p, "/memory");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".memory-enabled .switch:not([disabled])").waitFor();
};
const show = async p => {
  console.log((await panel(p).innerText()).replace(/\n+/g, "\n").replaceAll(today, "(today)"));
  console.log(`switch: ${await p.locator(".memory-enabled .switch").getAttribute("aria-checked")}`);
};
const today = new Date().toISOString().slice(0, 10);
const notesFile = () =>
  JSON.parse(readFileSync(memoryFile, "utf8"))
    .notes.map(n => `${n.id} ${n.title ?? "-"}: ${n.text}`)
    .join("; ");
const prefsFile = () => {
  try {
    return JSON.stringify(JSON.parse(readFileSync(join(home, ".prigh/prefs.json"), "utf8"))["memory.enabled"] ?? null);
  } catch {
    return "(no prefs.json)";
  }
};
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
  await openMemory(page);
  section("notes");
  await show(page);
  await screenshot(page, "notes");

  await page.locator("#memory-filter").fill("deploy");
  await page.locator(".memory-note").nth(1).waitFor({ state: "detached" });
  section("filtered");
  console.log((await page.locator(".memory-note").innerText()).replace(/\n+/g, " | "));
  await page.locator("#memory-filter").fill("");

  // Edit: the note's text in place; Enter in the title saves.
  await page.locator("[data-note=m1] button[aria-label='Edit editor']").click();
  await page.locator("#memory-text").fill("neovim, with lazy.nvim");
  await page.locator("#memory-title").press("Enter");
  await page.locator("[data-note=m1]", { hasText: "neovim" }).waitFor();
  // A new note; Esc leaves the one half written, and the page stays.
  await page.locator(".memory-add").click();
  await page.locator("#memory-text").fill("half written");
  await page.keyboard.press("Escape");
  await page.locator(".memory-draft").waitFor({ state: "detached" });
  console.log(`settings still open: ${await page.locator(".settings-dialog").count()}`);
  await page.locator(".memory-add").click();
  await page.locator("#memory-title").fill("pager");
  await page.locator("#memory-text").fill("less -R");
  await screenshot(page, "adding");
  await page.locator("#memory-text").press("Control+Enter");
  await page.locator(".memory-note", { hasText: "less -R" }).waitFor();
  section("edited and added");
  console.log(notesFile());

  // Delete asks first.
  await page.locator("[data-note=m3] .memory-delete").click();
  await page.locator("[data-note=m3].forgetting").waitFor();
  section("deleting");
  console.log((await page.locator("[data-note=m3]").innerText()).replace(/\n+/g, " | "));
  await screenshot(page, "deleting");
  await page.locator("#memory-forget-yes").click();
  await page.locator("[data-note=m3]").waitFor({ state: "detached" });
  console.log(notesFile());

  // The switch, saved for the account; another page follows.
  const other = await open({ width: 1280, height: 800 });
  await signIn(other);
  await openMemory(other);
  await page.locator(".memory-enabled .switch").click();
  await page.locator(".memory-enabled .switch[aria-checked=false]").waitFor();
  await other.locator(".memory-enabled .switch[aria-checked=false]").waitFor();
  section("off");
  await show(other);
  console.log(`prefs: memory.enabled=${prefsFile()}`);
  await screenshot(page, "off");
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal-backdrop", { state: "detached" });
  await run(page, "/memory on");
  await other.locator(".memory-enabled .switch[aria-checked=true]").waitFor();
  console.log(`after /memory on: prefs memory.enabled=${prefsFile()}`);
  await other.close();

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone);
    await phone.addStyleTag({ content: "*, *::before, *::after { animation: none !important; transition: none !important; }" });
    await openMemory(phone);
    section(`phone ${tag}`);
    console.log(`fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}`);
    await phone.locator("[data-note=m2] .memory-delete").tap();
    await phone.locator("#memory-forget-yes").waitFor();
    console.log(`deleting fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-deleting`);
    await phone.locator("[data-note=m2] button", { hasText: "Keep" }).tap();
    await phone.locator("[data-note=m2] button[aria-label^='Edit']").tap();
    await phone.locator("#memory-text").waitFor();
    console.log(`editing fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-editing`);
    await phone.locator(".memory-draft button", { hasText: "Cancel" }).tap();
    // The way back: the list of sections.
    await phone.locator(".modal-back").tap();
    await phone.locator(".settings-home-row.settings-tab-memory").waitFor();
    console.log(`back: ${(await phone.locator(".settings-tab-memory").innerText()).replace(/\n+/g, " | ")}`);
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
