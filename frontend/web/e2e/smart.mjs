// Smart approval mode: the mode chosen from the model menu's access (on a
// phone, /mode, then the model sheet shows it), a call the approval model lets run
// (its "allowed by" note on the card) and one it leaves to the user (its
// reason on the approval card, answered), both listed by /permissions and
// forgotten there (one, then all); then Settings → Models → Approvals. On a desktop and two phones (390x844, 320x640, touch, the
// larger as an installed PWA with the safe area's insets). Run by
// prigh_web.sh; prints normalised snapshots (the expected output is in
// ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { animationsOver } from "./animations.mjs";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName, part = "desktop"] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
const viewports = {
  desktop: { width: 1280, height: 800 },
  phone: { width: 390, height: 844 },
  small: { width: 320, height: 640 },
};
const viewport = viewports[part];
if (!engine || !viewport) throw new Error("usage: smart.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `smart-${part}`);
const home = join(dir, "home");
normalVerbosity(home);
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });

// The session's model answers both the agent and, as the approval model,
// the judge: one call per turn, so each verdict follows its call.
const verdict = (id, decision, reason) => ({
  tool_calls: [{ id, name: "verdict", arguments: { decision, reason } }],
});
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { text: "Running the tests.", tool_calls: [{ id: "b1", name: "bash", arguments: { command: "echo tests passed" } }] },
    verdict("v1", "allow", "runs the project's tests, which the user asked for"),
    { text: "Clearing the cache.", tool_calls: [{ id: "b2", name: "bash", arguments: { command: "rm -rf ~/.cache/demo" } }] },
    verdict("v2", "ask", "deletes files outside the project"),
    { text: "Done." },
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
const context = await browser.newContext({
  viewport,
  reducedMotion: "reduce",
  hasTouch: touch,
  ...(touch && engineName !== "firefox" ? { isMobile: true } : {}),
});
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: smart (${part}): ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-smart-${part}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const tap = locator => (touch ? locator.tap() : locator.click());
const composer = page.locator(".composer textarea");
const flat = s => s.replace(/\s+/g, " ").trim();
const overflow = () => page.evaluate(() => document.documentElement.scrollWidth > window.innerWidth
  ? `overflows: ${document.documentElement.scrollWidth}px` : "fits the width");
// Whether [selector]'s box is whole in the window, nothing covers its
// middle, and it is tall enough to tap.
const inView = selector => page.evaluate(([sel, min]) => {
  const el = document.querySelector(sel);
  if (!el) return "missing";
  const r = el.getBoundingClientRect();
  if (r.left < 0 || r.top < 0 || r.right > window.innerWidth + 0.5 || r.bottom > window.innerHeight + 0.5)
    return `off screen: ${Math.round(r.left)},${Math.round(r.top)} ${Math.round(r.width)}x${Math.round(r.height)}`;
  const hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
  if (!el.contains(hit)) return `covered by ${hit?.className}`;
  return r.height >= min ? "in view, tall enough" : `in view, ${Math.round(r.height)}px tall`;
}, [selector, touch ? 44 : 24]);
const send = async text => {
  await composer.fill(text);
  await tap(page.locator(".btn.send"));
};

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer.waitFor();
  if (part === "phone" && engineName === "chromium") {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }

  // 1. Smart mode.
  if (touch) {
    await send("/mode smart");
    await page.locator(".toast", { hasText: "Mode: smart" }).waitFor();
    await tap(page.locator(".composer .model-chip"));
    await page.locator("#chip-popover.model-menu").waitFor();
    section("the model sheet");
    console.log(`shows smart: ${flat(await page.locator("#chip-popover .model-access-button").innerText()) === "Smart"}`);
    console.log(await overflow());
    await screenshot("sheet");
    await page.keyboard.press("Escape");
    await page.locator("#chip-popover").waitFor({ state: "detached" });
  } else {
    await page.locator(".composer .model-chip").click();
    await page.locator("#chip-popover .model-access-button").click();
    await page.locator("#chip-popover .model-access-menu").waitFor();
    section("the model menu's access");
    console.log((await page.locator("#chip-popover .model-access-item").evaluateAll(es => es.map(e => `${e.innerText} ${e.title}`))).map(flat).join("\n"));
    await screenshot("mode menu");
    await page.locator("#chip-popover .model-access-item", { has: page.locator(".menu-label", { hasText: /^Smart$/ }) }).click();
    const access = page.locator("#chip-popover .model-access-button");
    await access.locator(".chip-label", { hasText: /^Smart$/ }).waitFor();
    section("the access mode");
    console.log(`${flat(await access.innerText())}: ${await access.getAttribute("title")}`);
    await page.keyboard.press("Escape");
    await page.waitForSelector("#chip-popover", { state: "detached" });
  }

  // 2. One call allowed, one left to the user.
  await send("run the tests, then clear the cache");
  const confirm = page.locator("#confirm");
  await confirm.waitFor();
  // The sheet's entrance done.
  await animationsOver(page, "#confirm");
  section("left to the user");
  console.log(flat(await confirm.innerText()));
  for (const cls of ["allow", "deny"]) console.log(`${cls}: ${await inView(`#confirm .confirm-answer.${cls}`)}`);
  console.log(await overflow());
  await screenshot("judged");
  await tap(page.locator("#confirm .confirm-answer.allow"));
  await confirm.waitFor({ state: "detached" });
  await page.waitForFunction(() => document.getElementById("chat")?.innerText.includes("Done."));

  section("allowed by the approval model");
  const allowed = page.locator(".tool", { has: page.locator(".chip.judged") });
  console.log(`judged cards: ${await allowed.count()}`);
  console.log(`head: ${flat(await allowed.locator(".tool-head").innerText())}`);
  console.log(`chip's title: ${await allowed.locator(".chip.judged").getAttribute("title")}`);
  await tap(allowed.locator(".tool-head"));
  await allowed.locator(".tool-allowed").waitFor();
  console.log(`note: ${flat(await allowed.locator(".tool-allowed").innerText())}`);
  console.log(await overflow());
  await allowed.scrollIntoViewIfNeeded();
  await screenshot("allowed");

  // 2b. /permissions lists both calls, which run again without being judged;
  //     one is forgotten, then the rest.
  await send("/permissions");
  const perms = page.locator(".permissions-dialog");
  const calls = perms.locator(".allowed-call");
  await calls.nth(1).waitFor();
  const listed = async () =>
    (await calls.locator(".allowed-call-text").allInnerTexts())
      .map(t => flat(t).replaceAll(cwd, "$CWD").replace(/ on \S+ in /, " on <host> in "))
      .join("\n");
  section("/permissions: the remembered calls");
  console.log(await listed());
  console.log(`forget: ${await inView(".allowed-call .forget")}`);
  console.log(`forget all: ${await inView(".permissions-dialog .forget-all")}`);
  console.log(await overflow());
  await screenshot("permissions");
  await tap(calls.first().locator(".forget"));
  await page.waitForFunction(() => document.querySelectorAll(".allowed-call").length === 1);
  section("/permissions: one forgotten");
  console.log(await listed());
  const forgot = page.locator(".toast", { hasText: "Forgot" }).first();
  await forgot.waitFor();
  console.log(flat(await forgot.innerText()));
  await tap(perms.locator(".forget-all"));
  await calls.first().waitFor({ state: "detached" });
  section("/permissions: all forgotten");
  console.log(`calls: ${await calls.count()}, forget all: ${await perms.locator(".forget-all").count()}`);
  await screenshot("permissions forgotten");
  await page.keyboard.press("Escape");
  await perms.waitFor({ state: "detached" });

  // 3. Settings → Models → Approvals.
  if (touch) {
    await tap(page.getByRole("button", { name: "Panels and settings" }));
    await tap(page.locator(".chrome-menu .sheet-row", { hasText: "Settings" }).first());
  } else {
    await page.keyboard.press("Control+,");
  }
  await page.locator(".settings-dialog").waitFor();
  await tap(page.locator(".settings-tab-models"));
  const row = page.locator(".setting.approval-model");
  await row.locator(".approval-model-choice:not([disabled])").waitFor();
  await row.scrollIntoViewIfNeeded();
  section("Settings → Models → Approvals");
  console.log(flat(await row.innerText()));
  console.log(`choice: ${await inView(".approval-model-choice")}`);
  console.log(await overflow());
  await screenshot("approvals row");
  await tap(row.locator(".approval-model-choice"));
  await page.locator(".modal #picker-input").waitFor();
  console.log(`${await page.locator(".modal-head h2").innerText()}; first: ${flat(await page.locator(".picker-item").first().innerText())}`);
  await page.locator(".modal #picker-input").fill("haiku");
  await page.locator(".modal #picker-input").press("Enter");
  await page.locator(".settings-dialog").waitFor();
  await row.locator(".approval-model-choice", { hasText: "Haiku" }).waitFor();
  console.log(`chosen: ${flat(await row.locator(".approval-model-choice").innerText()).replace(/Haiku.*/, "Haiku")}`);
  await screenshot("approvals chosen");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  console.log(`FAILED: ${error.stack ?? error}`);
  await screenshot("failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
