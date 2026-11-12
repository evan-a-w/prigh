// The settings page's Custom instructions section: the instructions in the
// account's custom_instructions.json, added, edited and deleted (asked
// first); the switch letting the model change them, saved in prefs.json;
// the size, and the warning past the threshold; a change on another page
// followed; the open session told with its next message; and the section on
// phones (390x844, 320x640; touch, and Chromium as an installed PWA with the
// notch's insets), in the warning state too. Run by prigh_web.sh; prints
// normalised snapshots (the expected output is in ENGINE.expected) and,
// with SHOTS, screenshots.
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
if (!engine) throw new Error("usage: custom_instructions.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "custom_instructions");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".prigh"), { recursive: true });
mkdirSync(cwd, { recursive: true });
const file = join(home, ".prigh/custom_instructions.json");
const seed = notes => writeFileSync(file, JSON.stringify({ next: notes.length + 1, notes }));
seed([
  { id: "ci1", text: "Use Australian spelling.", saved: "2026-11-01 09:30:00.000000Z" },
  { id: "ci2", title: "Maths", text: "Keep a single memory note on my maths ability up to date.", saved: "2026-11-02 14:05:00.000000Z" },
]);

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

const section = label => console.log(`=== ${engineName}: custom instructions: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-instructions-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
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
// /instructions: Settings, at Custom instructions, once they are in.
const openInstructions = async p => {
  await run(p, "/instructions");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".instructions-enabled .switch:not([disabled])").waitFor();
};
const today = new Date().toISOString().slice(0, 10);
const show = async p => {
  console.log((await panel(p).innerText()).replace(/\n+/g, "\n").replaceAll(today, "(today)"));
  console.log(`switch: ${await p.locator(".instructions-enabled .switch").getAttribute("aria-checked")}`);
};
const notesFile = () =>
  JSON.parse(readFileSync(file, "utf8"))
    .notes.map(n => `${n.id} ${n.title ?? "-"}: ${n.text.length > 60 ? `${n.text.slice(0, 20)}… (${n.text.length} bytes)` : n.text}`)
    .join("; ");
const prefsFile = () => {
  try {
    return JSON.stringify(JSON.parse(readFileSync(join(home, ".prigh/prefs.json"), "utf8"))["custom_instructions.enabled"] ?? null);
  } catch {
    return "(no prefs.json)";
  }
};
// The page's elements that overflow it sideways, or are under the edges.
const fits = p =>
  p.evaluate(() => {
    const bad = [];
    const w = document.documentElement.clientWidth;
    for (const el of document.querySelectorAll(".settings-panel button, .settings-panel input, .settings-panel textarea, .instructions-warning")) {
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
      const after = el.matches(".switch") ? parseFloat(getComputedStyle(el, "::after").top) : 0;
      if (r.height - 2 * Math.min(after, 0) < 40) bad.push(`${el.getAttribute("aria-label") ?? el.innerText.trim() ?? el.id} ${Math.round(r.height)}px high`);
      if (el.matches("input, textarea") && parseFloat(getComputedStyle(el).fontSize) < 16) bad.push(`${el.id} font ${getComputedStyle(el).fontSize}`);
    }
    return bad.length ? bad.join(", ") : "all ok";
  });

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  // A session started before the changes below, so it is told them.
  await run(page, "hello");
  await page.getByText("faux reply").first().waitFor();
  await openInstructions(page);
  section("list");
  await show(page);
  await screenshot(page, "list");

  // Edit in place; Enter in the title saves.
  await page.locator("[data-note=ci1] button[aria-label='Edit ci1']").click();
  await page.locator("#instruction-text").fill("Use Australian spelling and metric units.");
  await page.locator("#instruction-title").press("Enter");
  await page.locator("[data-note=ci1]", { hasText: "metric" }).waitFor();
  // A new one; Esc leaves the one half written, and the page stays.
  await page.locator(".instruction-add").click();
  await page.locator("#instruction-text").fill("half written");
  await page.keyboard.press("Escape");
  await page.locator(".instruction-draft").waitFor({ state: "detached" });
  console.log(`settings still open: ${await page.locator(".settings-dialog").count()}`);
  await page.locator(".instruction-add").click();
  await page.locator("#instruction-title").fill("Code");
  await page.locator("#instruction-text").fill("Prefer OCaml examples.");
  await screenshot(page, "adding");
  await page.locator("#instruction-text").press("Control+Enter");
  await page.locator(".memory-note", { hasText: "Prefer OCaml" }).waitFor();
  section("edited and added");
  console.log(notesFile());

  // Delete asks first.
  await page.locator("[data-note=ci2] .memory-delete").click();
  await page.locator("[data-note=ci2].forgetting").waitFor();
  section("deleting");
  console.log((await page.locator("[data-note=ci2]").innerText()).replace(/\n+/g, " | "));
  await screenshot(page, "deleting");
  await page.locator("#instruction-remove-yes").click();
  await page.locator("[data-note=ci2]").waitFor({ state: "detached" });
  console.log(notesFile());

  // The switch, saved for the account; another page follows.
  const other = await open({ width: 1280, height: 800 });
  await signIn(other);
  await openInstructions(other);
  await page.locator(".instructions-enabled .switch").click();
  await page.locator(".instructions-enabled .switch[aria-checked=false]").waitFor();
  await other.locator(".instructions-enabled .switch[aria-checked=false]").waitFor();
  section("off, followed on another page");
  await show(other);
  console.log(`prefs: custom_instructions.enabled=${prefsFile()}`);
  await other.close();
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal-backdrop", { state: "detached" });
  await run(page, "/instructions on");

  // The open session is told what changed with its next message.
  await run(page, "and again");
  await page.locator(".note[data-kind=custom_instructions]").waitFor();
  section("the session is told");
  console.log((await page.locator(".note[data-kind=custom_instructions] summary").innerText()).replace(/\s+/g, " ").trim());
  console.log((await page.locator(".note[data-kind=custom_instructions] .note-text").textContent()).trim());
  await screenshot(page, "told");
  console.log(`after /instructions on: prefs custom_instructions.enabled=${prefsFile()}`);

  // Past the threshold: a warning says what they cost (written by hand: the
  // backend reads the file each time).
  seed(
    Array.from({ length: 8 }, (_, i) => ({
      id: `ci${i + 1}`,
      title: `Rule ${i + 1}`,
      text: `${"Explain every step of the reasoning in full. ".repeat(19)}`.trim(),
      saved: "2026-11-03 08:00:00.000000Z",
    })),
  );
  await openInstructions(page);
  await page.locator(".instructions-warning").waitFor();
  section("warning");
  console.log((await page.locator(".instructions-warning").innerText()).replace(/\n+/g, " | "));
  await screenshot(page, "warning");

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone);
    await phone.addStyleTag({ content: "*, *::before, *::after { animation: none !important; transition: none !important; }" });
    await openInstructions(phone);
    section(`phone ${tag}`);
    console.log(`warning fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-warning`);
    await phone.locator("[data-note=ci2] .memory-delete").tap();
    await phone.locator("#instruction-remove-yes").waitFor();
    console.log(`deleting fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-deleting`);
    await phone.locator("[data-note=ci2] button", { hasText: "Keep" }).tap();
    await phone.locator("[data-note=ci2] button[aria-label^='Edit']").tap();
    await phone.locator("#instruction-text").waitFor();
    console.log(`editing fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-editing`);
    await phone.locator(".instruction-draft button", { hasText: "Cancel" }).tap();
    await phone.locator(".modal-back").tap();
    await phone.locator(".settings-home-row.settings-tab-instructions").waitFor();
    console.log(`back: ${(await phone.locator(".settings-tab-instructions").innerText()).replace(/\n+/g, " | ")}`);
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
