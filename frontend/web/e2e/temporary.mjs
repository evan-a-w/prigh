// Temporary sessions, on a desktop (1280x800) and two phones by touch
// (390x844, 320x640): the new-session screen (its heading, the tray and its
// Temporary chip), a temporary session started from it, its pill, a reload
// that comes back to it, leaving it (a toast, and nothing left on disk or in
// the sidebar), keeping one (in the sidebar, saved), and leaving one that
// works (the confirmation, then the work stopped). Run by prigh_web.sh;
// prints normalised results (the expected output is in ENGINE.expected)
// and, with SHOTS, screenshots (also in the three themes, landscape and as
// an installed PWA).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: temporary.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "temporary");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("temporary.mjs: timed out"); process.exit(2); }, 280000).unref();

// Per screen: the answer in the session left, the one in the session kept,
// and a command that keeps the third one working until it is left.
const marker = join(dir, "pid");
const perScreen = [
  { text: "It moves a branch onto another base." },
  { text: "Kept: worth reading again." },
  { text: "Running it.",
    tool_calls: [{ id: "s1", name: "bash", arguments: { command: `echo $$ > ${marker}; sleep 60` } }] },
];
writeFileSync(join(dir, "script.json"), JSON.stringify([...perScreen, ...perScreen, ...perScreen, { text: "Sure." }]));

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  { env: { ...process.env, HOME: home, SHELL: "/bin/sh" }, stdio: ["ignore", "ignore", "pipe"] },
);
const url = await new Promise((resolve, reject) => {
  let err = "";
  const timer = setTimeout(() => reject(new Error(`backend did not start:\n${err}`)), 30000);
  server.stderr.on("data", data => {
    err += data;
    const m = err.match(/^prigh: prigh-web on (\S+)$/m);
    if (m) { clearTimeout(timer); resolve(m[1]); }
  });
  server.on("exit", code => { clearTimeout(timer); reject(new Error(`backend exited (${code}):\n${err}`)); });
});
const stopServer = () =>
  new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null || server.signalCode !== null) return resolve();
    const timer = setTimeout(() => { server.kill("SIGKILL"); }, 5000);
    server.on("exit", () => { clearTimeout(timer); resolve(); });
    server.kill();
  });

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const section = label => console.log(`=== ${engineName}: temporary: ${label} ===`);
let base = 0;
// Saved sessions, since the screen began (they share a backend).
const files = () => readdirSync(sessions).filter(f => f.endsWith(".jsonl")).length - base;

let page;
let tag;
const shoot = async label => {
  if (!process.env.SHOTS) return;
  await page.waitForFunction(() => document.getAnimations().every(a => a.playState !== "running" || a.effect?.getComputedTiming().iterations === Infinity));
  await page.screenshot({ path: join(process.env.SHOTS, `temporary-${engineName}-${tag}-${label.replace(/\W+/g, "-")}.png`) });
};
const say = (...parts) => console.log(parts.join(" "));
const text = sel => page.evaluate(sel => document.querySelector(sel)?.innerText.trim().replace(/\s*\n+\s*/g, " | ") ?? "none", sel);
// Every control of the screen is in it, whole, and not under another.
const reachable = sel => page.evaluate(async sel => {
  await Promise.all(document.getAnimations().filter(a => a.effect?.getComputedTiming().iterations !== Infinity).map(a => a.finished.catch(() => {})));
  const bad = [];
  for (const el of document.querySelectorAll(sel)) {
    const r = el.getBoundingClientRect();
    const name = el.getAttribute("aria-label") || el.innerText.trim();
    if (r.left < -0.5 || r.right > innerWidth + 0.5 || r.top < -0.5 || r.bottom > innerHeight + 0.5) bad.push(`${name} off screen`);
    const top = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
    if (!top || !(el === top || el.contains(top))) bad.push(`${name} covered`);
  }
  if (document.documentElement.scrollWidth > innerWidth + 1) bad.push("the page overflows sideways");
  return bad.length ? bad.join("; ") : "all in view";
}, sel);
const tap = async sel => (tag === "desktop" ? page.locator(sel).first().click() : page.locator(sel).first().tap());
const send = async message => {
  const editor = page.locator(".composer textarea");
  await editor.fill(message);
  await editor.press("Enter");
};
// The sidebar's sessions (a phone's drawer has them too, closed), since the
// screen began.
let listedBase = 0;
const listed = async () => (await page.evaluate(() => document.querySelectorAll("[data-session]").length)) - listedBase;
const sidebarSessions = listed;
const temporaryOn = async () => {
  await tap(".temporary-chip");
  await page.waitForFunction(() => document.querySelector(".hero")?.innerText.includes("Temporary session")
    && document.querySelector(".temporary-chip")?.getAttribute("aria-checked") === "true"
    && document.querySelector(".main.temporary.fresh"));
  // The page has the session once its key is in the tab's storage.
  await page.waitForFunction(() => sessionStorage.getItem("prigh-web.temporary"));
};
const newSession = async () => {
  await send("/new");
  await page.locator(".main.fresh .hero").waitFor();
};

const screen = async (w, h, label) => {
  tag = label;
  const phone = label !== "desktop";
  base = 0;
  base = files();
  const options = { viewport: { width: w, height: h } };
  if (phone) {
    options.hasTouch = true;
    if (engineName !== "firefox") options.isMobile = true;
  }
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  page = await context.newPage();
  page.on("console", m => { if (m.type() === "error" && !/favicon\.ico|WebSocket/.test(m.text())) errors.push(`console: ${m.text()}`); });
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".main.fresh .hero").waitFor();
  await page.locator(".sessions [data-session], .sessions-empty").first().waitFor({ state: "attached" });
  listedBase = 0;
  listedBase = await listed();

  section(`${label}: the new-session screen`);
  say("heading:", await text(".hero"));
  say("tray:", (await text(".tray")).replaceAll(cwd, "<cwd>"));
  say("chip:", await page.locator(".temporary-chip").getAttribute("aria-checked"));
  say("composer under the heading:", await page.evaluate(() =>
    document.querySelector(".hero").getBoundingClientRect().bottom <= document.querySelector(".composer-stack").getBoundingClientRect().top));
  say("tray:", await page.evaluate(() => {
    const t = document.querySelector(".tray").getBoundingClientRect();
    const s = document.querySelector(".composer-stack").getBoundingClientRect();
    return t.top >= s.bottom - 20 ? "under the composer" : t.bottom <= s.top + 1 ? "over the composer" : "elsewhere";
  }));
  if (!phone) say("centred:", await page.evaluate(() => {
    const s = document.querySelector(".composer-stack").getBoundingClientRect();
    return s.top > innerHeight * 0.3 && s.bottom < innerHeight * 0.75;
  }));
  else say("composer at the foot:", await page.evaluate(() =>
    innerHeight - document.querySelector(".composer").getBoundingClientRect().bottom < 2));
  say("controls:", await reachable(".tray button, .composer .send"));
  await shoot("1-new");
  if (process.env.SHOTS)
    for (const theme of ["light", "original", "dark"]) {
      await page.evaluate(theme => document.documentElement.setAttribute("data-theme", theme), theme);
      await shoot(`1-new-${theme}`);
    }

  section(`${label}: Temporary on`);
  await temporaryOn();
  say("heading:", await text(".hero"));
  say("listed:", await listed());
  say("controls:", await reachable(".tray button, .composer .send"));
  await shoot("2-temporary-on");
  if (process.env.SHOTS)
    for (const theme of ["light", "original"]) {
      await page.evaluate(theme => document.documentElement.setAttribute("data-theme", theme), theme);
      await shoot(`2-temporary-on-${theme}`);
    }
  await page.evaluate(() => document.documentElement.setAttribute("data-theme", "dark"));

  section(`${label}: the first message`);
  await send("What does git rebase --onto do?");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("onto another base"));
  await page.locator(".temporary-pill").waitFor();
  say("pill:", await text(".temporary-pill"));
  say("layout:", await page.evaluate(() => document.querySelector(".main").className));
  say("prompt's edge:", await page.evaluate(() => {
    const edge = getComputedStyle(document.querySelector(".msg.user")).borderLeftColor;
    const probe = document.createElement("i");
    probe.style.color = "var(--sodium)";
    document.body.append(probe);
    const sodium = getComputedStyle(probe).color;
    probe.remove();
    return edge === sodium ? "sodium" : "grey";
  }));
  say("tray gone:", (await page.locator(".tray").count()) === 0);
  say("saved:", files(), "listed:", await sidebarSessions());
  say("pill:", await reachable(".temporary-pill"));
  await shoot("3-inside");

  section(`${label}: reloaded`);
  await page.reload();
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("onto another base"));
  await page.locator(".temporary-pill").waitFor();
  say("still there:", await page.evaluate(() => document.querySelector("#chat").innerText.includes("rebase --onto")));
  say("saved:", files());

  section(`${label}: its menu`);
  await tap(".temporary-pill");
  await page.locator(".temporary-menu").waitFor();
  say("menu:", await text(".temporary-menu .modal-body"));
  say("menu:", await page.evaluate(async phone => {
    await Promise.all(document.getAnimations().filter(a => a.effect?.getComputedTiming().iterations !== Infinity).map(a => a.finished.catch(() => {})));
    const m = document.querySelector(".temporary-menu").getBoundingClientRect();
    if (phone) return `a sheet across the bottom: ${m.left <= 1 && m.right >= innerWidth - 1 && Math.abs(m.bottom - innerHeight) <= 1}`;
    const p = document.querySelector(".temporary-pill").getBoundingClientRect();
    return `under the pill: ${m.top >= p.bottom - 1 && Math.abs((m.left + m.right) / 2 - (p.left + p.right) / 2) < 2}`;
  }, phone));
  say("controls:", await reachable(".temporary-actions .btn"));
  await shoot("4-menu");
  await page.keyboard.press("Escape");
  await page.locator(".temporary-menu").waitFor({ state: "detached" });

  section(`${label}: left`);
  await newSession();
  await page.locator(".toast", { hasText: "Temporary session deleted" }).waitFor();
  say("toast:", await text(".toasts"));
  say("saved:", files(), "listed:", await sidebarSessions());
  await shoot("5-left");

  section(`${label}: kept`);
  const before = files();
  const listedBefore = listedBase + await listed();
  await temporaryOn();
  await send("Explain git worktrees");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("worth reading again"));
  await tap(".temporary-pill");
  await tap(".temporary-keep");
  await page.waitForFunction(() => !document.querySelector(".temporary-pill") && !document.querySelector(".main.temporary"));
  await page.waitForFunction(n => document.querySelectorAll("[data-session]").length > n, listedBefore);
  say("saved:", files() - before, "listed:", await sidebarSessions());
  say("prompt's edge:", await page.evaluate(() => getComputedStyle(document.querySelector(".msg.user")).borderLeftStyle));
  await shoot("6-kept");

  section(`${label}: leaving while it works`);
  await newSession();
  await temporaryOn();
  await send("Run the slow thing");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Running it."));
  await new Promise(resolve => {
    const check = () => (existsSync(marker) && readFileSync(marker, "utf8").trim() ? resolve() : setTimeout(check, 50));
    check();
  });
  await send("/new");
  await page.locator(".leave-temporary").waitFor();
  say("dialog:", await text(".leave-temporary"));
  say("controls:", await reachable(".leave-temporary .btn"));
  await shoot("7-leave-while-running");
  // Stay: nothing happens.
  await tap(".leave-temporary .modal-buttons .btn:nth-child(2)");
  await page.locator(".leave-temporary").waitFor({ state: "detached" });
  say("stayed:", (await page.locator(".temporary-pill").count()) === 1);
  await send("/new");
  await tap(".leave-temporary .btn.danger");
  await page.locator(".main.fresh .hero").waitFor();
  await page.locator(".toast", { hasText: "Temporary session deleted" }).waitFor();
  const pid = Number(readFileSync(marker, "utf8"));
  const alive = () => { try { process.kill(pid, 0); return true; } catch { return false; } };
  for (let i = 0; i < 200 && alive(); i++) await new Promise(r => setTimeout(r, 50));
  say("its command stopped:", !alive());
  rmSync(marker);
  say("saved:", files() - before - 1);

  if (process.env.SHOTS && phone && label === "390x844") {
    // Landscape, and as an installed PWA (the notch and the home indicator).
    await page.setViewportSize({ width: 844, height: 390 });
    await shoot("8-landscape");
  }
  await context.close();
};

// Screenshots only: as an installed PWA on a notched phone (Chromium: the
// safe area's insets emulated).
const pwa = async () => {
  tag = "pwa";
  const context = await browser.newContext({ viewport: { width: 390, height: 844 }, hasTouch: true, isMobile: true });
  page = await context.newPage();
  const cdp = await context.newCDPSession(page);
  await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".main.fresh .hero").waitFor();
  await shoot("9-new");
  await temporaryOn();
  await shoot("9-temporary-on");
  await send("one more");
  await page.locator(".temporary-pill").waitFor();
  await shoot("9-inside");
  await tap(".temporary-pill");
  await page.locator(".temporary-menu").waitFor();
  await shoot("9-menu");
  await context.close();
};

try {
  await screen(1280, 800, "desktop");
  await screen(390, 844, "390x844");
  await screen(320, 640, "320x640");
  if (process.env.SHOTS && engineName === "chromium") await pwa();
  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await shoot("failure").catch(() => {});
  console.error(await page?.evaluate(() => document.querySelector(".app")?.outerHTML.slice(0, 3000)).catch(() => ""));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
