// Chat mode on a backend with no tool host (`prigh serve -no-backend-host`,
// none connected): a new session starts in chat mode (the mode chip says
// chat, the host chip "no host", the composer no working directory) and a
// message gets the model's reply; the mode chip's menu; another mode can't
// send (the strip over the composer says why, the message stays) until
// "Chat without a host" switches back and sends it. On a desktop, then on
// phones (390x844, 320x640; touch, and Chromium as an installed PWA with
// the notch's insets): the ⋯ sheet's mode row, the strip, nothing sideways
// or under 40px in the composer. Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: chat.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "chat");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".prigh"), { recursive: true });
mkdirSync(cwd, { recursive: true });

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", cwd, "-no-backend-host"],
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

const section = label => console.log(`=== ${engineName}: chat: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-chat-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const composer = p => p.locator(".composer textarea");
const signIn = async (p, at = url) => {
  await p.goto(at, { timeout: 60000 });
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await composer(p).waitFor();
};
const run = async (p, command) => {
  await composer(p).fill(command);
  await composer(p).press("Control+Enter");
};
// Times of day masked.
const plain = s => s.replace(/\b\d\d:\d\d\b/g, "(time)").replace(/\s+/g, " ").trim();
const text = async (p, selector) =>
  (await p.locator(selector).count()) ? plain(await p.locator(selector).first().innerText()) : "(none)";
// What says where the session is: the mode, the host, the composer, the strip.
const where = async p => {
  const chip = p.locator(".mode-chip");
  console.log(`mode chip: ${(await chip.count()) && (await chip.first().isVisible()) ? await text(p, ".mode-chip") : "(in the ⋯ sheet)"}`);
  const host = p.locator(".host-chip");
  console.log(`host chip: ${(await host.count()) && (await host.first().isVisible()) ? await text(p, ".host-chip") : "(hidden)"}`);
  console.log(`placeholder: ${(await composer(p).getAttribute("placeholder")).replace(cwd, "$CWD")}`);
  console.log(`strip: ${await text(p, ".no-host-banner")}`);
};
const replies = p => p.locator("#chat .msg.assistant");
// From mobile.mjs: sideways overflow, controls off screen, covered or
// under 44px (40px in the composer), inputs under 16px.
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

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  await page.locator(".mode-chip.chat-mode").waitFor();
  section("a new session with no tool host");
  await where(page);
  await screenshot(page, "new");

  await composer(page).fill("what is a monad?");
  await composer(page).press("Enter");
  await replies(page).first().waitFor();
  const sessionUrl = page.url();
  section("a reply, with no tool host");
  console.log(`you: ${await text(page, "#chat .msg.user")}`);
  console.log(`prigh: ${await text(page, "#chat .msg.assistant")}`);
  await screenshot(page, "replied");

  await page.locator(".mode-chip").click();
  await page.locator("#chip-popover").waitFor();
  section("the mode menu");
  console.log((await page.locator("#chip-popover").innerText()).replace(/\n+/g, "\n"));
  await screenshot(page, "menu");
  await page.keyboard.press("Escape");
  await page.locator("#chip-popover").waitFor({ state: "detached" });

  // Out of chat mode, nothing can run the message: said before sending,
  // and sending keeps it.
  await run(page, "/mode ask");
  await page.locator(".no-host-banner").waitFor();
  section("mode ask, no tool host");
  await where(page);
  await composer(page).fill("and a functor?");
  await composer(page).press("Enter");
  await page.locator(".toast.error").waitFor();
  console.log(`toast: ${await text(page, ".toast.error")}`);
  console.log(`draft: ${await composer(page).inputValue()}`);
  console.log(`messages: ${await page.locator("#chat .msg.user").count()}`);
  await screenshot(page, "no-host");

  await page.locator(".no-host-banner-chat").click();
  await replies(page).nth(1).waitFor();
  await page.locator(".no-host-banner").waitFor({ state: "detached" });
  section("chat without a host: sent");
  await where(page);
  console.log(`draft: "${await composer(page).inputValue()}"`);
  console.log(`you: ${(await page.locator("#chat .msg.user").evaluateAll(es => es.map(e => e.innerText))).map(plain).join(" | ")}`);
  console.log(`replies: ${await replies(page).count()}`);
  await screenshot(page, "chatted");

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone, sessionUrl);
    await phone.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; }" });
    await replies(phone).first().waitFor();
    section(`phone ${tag}: chat`);
    await where(phone);
    console.log(`audit: ${await audit(phone)}`);
    await screenshot(phone, `phone-${tag}`);
    await phone.getByRole("button", { name: "Panels and settings" }).tap();
    await phone.locator(".chrome-menu").waitFor();
    section(`phone ${tag}: the ⋯ sheet's mode`);
    console.log(await phone.locator(".chrome-menu .session-section").filter({ has: phone.locator(".session-section-title", { hasText: /^Mode$/ }) }).evaluate(e => [...e.querySelectorAll(".session-section-title, .session-choice")].map(c => c.classList.contains("on") ? `[${c.innerText}]` : c.innerText).join(" ")));
    console.log(`audit: ${await audit(phone)}`);
    await screenshot(phone, `phone-${tag}-sheet`);
    await phone.locator(".chrome-menu").getByRole("button", { name: "ask", exact: true }).tap();
    await phone.locator(".no-host-banner").waitFor({ state: "attached" });
    if (await phone.locator(".chrome-menu").count()) {
      await phone.keyboard.press("Escape");
      await phone.locator(".chrome-menu").waitFor({ state: "detached" });
    }
    await phone.locator(".no-host-banner").waitFor();
    section(`phone ${tag}: mode ask, no tool host`);
    await where(phone);
    console.log(`audit: ${await audit(phone)}`);
    await screenshot(phone, `phone-${tag}-no-host`);
    await phone.locator(".no-host-banner-chat").tap();
    await phone.locator(".host-chip.chatting").waitFor();
    await phone.locator(".no-host-banner").waitFor({ state: "detached" });
    console.log(`after Chat without a host: strip ${await text(phone, ".no-host-banner")}`);
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
