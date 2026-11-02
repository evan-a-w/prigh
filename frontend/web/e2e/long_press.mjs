// A long press on a phone opens the menu a right-click opens on a computer:
// on a message (yours, a reply, a tool line), a session and a worktree in the
// drawer, and a file of the review. Chromium holds a finger down through the
// DevTools protocol (Input.dispatchTouchEvent, as a phone's touch screen
// does: the finger never quite still); Firefox, which has no such protocol
// here, gets pointer events typed touch. Then: Chrome on Android's own long
// press (a contextmenu while the finger is down, after it selected a word)
// opens ours too, once, the word unselected; a short tap, a scroll and a
// dragged selection open nothing; Delete deletes; Select text selects; a
// mouse's right-click is as before. Run by prigh_web.sh; prints normalised
// results (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots.
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
if (!engine) throw new Error("usage: long_press.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "long_press");
const home = join(dir, "home");
const cwd = join(dir, "repo");
const docs = join(dir, "docs");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("long_press.mjs: timed out"); process.exit(2); }, 280000).unref();

const git = (...args) =>
  execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
writeFileSync(join(cwd, "server.ml"), "let port = 8080\n");
git("init", "-q", "-b", "main");
git("add", "-A");
git("commit", "-q", "-m", "start");
git("worktree", "add", "-q", "-b", "docs", docs);
// The session works in the worktree, so the drawer has it under the session.
writeFileSync(join(docs, "server.ml"), "let port = Sys.getenv \"PORT\"\n");

writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { text: "Looking at the server first.",
      tool_calls: [{ id: "r1", name: "read", arguments: { path: "server.ml" } }] },
    { text: "Done. The port is a literal; it should come from the environment." },
    { text: "Noted: the second answer." },
  ]),
);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", docs],
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
const section = label => console.log(`=== ${engineName}: long press: ${label} ===`);
let shot = 0;
const shoot = async (page, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-long-press-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const options = { viewport: { width: 390, height: 844 }, hasTouch: true };
if (engineName !== "firefox") options.isMobile = true;
const context = await browser.newContext(options);
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", m => { if (m.type() === "error" && !/favicon\.ico|WebSocket/.test(m.text())) errors.push(`console: ${m.text()}`); });
page.on("pageerror", e => errors.push(`page: ${e.message}`));
const cdp = engineName === "chromium" ? await context.newCDPSession(page) : null;

const sleep = ms => new Promise(r => setTimeout(r, ms));

// A finger on the element's point [at] (from its top left) for [ms],
// moving by [path] (dx, dy for each 50ms) meanwhile.
const touch = async (locator, { ms = 650, at = { x: 30, y: 10 }, path = i => [(i % 3) * 2, (i % 2) * 3] } = {}) => {
  await locator.scrollIntoViewIfNeeded();
  const box = await locator.boundingBox();
  const x = box.x + Math.min(at.x, box.width / 2), y = box.y + Math.min(at.y, box.height / 2);
  const steps = Math.max(1, Math.round(ms / 50));
  if (cdp) {
    await cdp.send("Input.dispatchTouchEvent", { type: "touchStart", touchPoints: [{ x, y }] });
    for (let i = 0; i < steps; i++) {
      await sleep(50);
      const [dx, dy] = path(i);
      await cdp.send("Input.dispatchTouchEvent", { type: "touchMove", touchPoints: [{ x: x + dx, y: y + dy }] });
    }
    await cdp.send("Input.dispatchTouchEvent", { type: "touchEnd", touchPoints: [] });
  } else {
    const send = (type, px, py) => page.evaluate(([type, px, py]) => {
      const target = document.elementFromPoint(px, py);
      target.dispatchEvent(new PointerEvent(type, {
        bubbles: true, cancelable: true, composed: true, pointerId: 7, pointerType: "touch",
        isPrimary: true, clientX: px, clientY: py,
      }));
      if (type === "pointerup")
        target.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, clientX: px, clientY: py }));
    }, [type, px, py]);
    await send("pointerdown", x, y);
    for (let i = 0; i < steps; i++) {
      await sleep(50);
      const [dx, dy] = path(i);
      await send("pointermove", x + dx, y + dy);
    }
    await send("pointerup", x, y);
  }
  await page.waitForTimeout(250);
};

const menu = () => page.evaluate(() => {
  const m = document.querySelector(".context-menu");
  if (!m) return "no menu";
  const items = [...m.querySelectorAll("[data-item]")].map(e => e.dataset.item).join(", ");
  const r = m.getBoundingClientRect();
  return `menu: ${items}; a sheet at the bottom: ${Math.abs(r.bottom - innerHeight) < 2 && Math.round(r.width) === innerWidth}`;
});
const close = async () => {
  if (await page.locator(".context-menu").count()) {
    await page.keyboard.press("Escape");
    await page.locator(".context-menu").waitFor({ state: "detached" });
  }
  await page.waitForTimeout(150);
};
const selected = () => page.evaluate(() => JSON.stringify(String(getSelection())));

const user = n => page.locator("#chat .msg.user[tabindex]").nth(n);
const reply = text => page.locator("#chat .msg[tabindex]").filter({ hasText: text }).last();

try {
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").tap();
  const editor = page.locator(".composer textarea");
  await editor.waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; }" });
  await editor.fill("make the port configurable");
  await editor.press("Enter");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Done."), null, { timeout: 60000 });
  await editor.fill("and a second question");
  await editor.press("Enter");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("Noted"), null, { timeout: 60000 });
  await page.waitForTimeout(300);

  section("held on your message");
  await touch(user(0).locator(".bubble"));
  console.log(await menu());
  console.log(`the release clicked nothing: ${(await page.locator(".context-menu").count()) === 1}`);
  console.log(`selected: ${await selected()}`);
  await shoot(page, "your message");
  await close();

  section("held on a reply");
  await touch(reply("Done.").locator("p").first());
  console.log(await menu());
  await close();

  section("held on a tool line");
  await touch(page.locator("#chat .tool-read").first());
  console.log(await menu());
  console.log(`the release did not open it: ${await page.locator("#chat .tool-read .tool-head").getAttribute("aria-expanded")}`);
  await shoot(page, "a tool line");
  await close();

  section("a short tap");
  await touch(user(0).locator(".bubble"), { ms: 100 });
  console.log(await menu());
  await close();

  section("a scroll");
  await touch(reply("Done.").locator("p").first(), { ms: 700, path: i => [0, -Math.min(60, i * 12)] });
  console.log(await menu());
  await close();

  // Chrome on Android: its long press selects the word under the finger,
  // then fires contextmenu (its copy bar's cue) while the finger is still
  // down, before our timer.
  section("the browser's own long press, with a word selected");
  {
    const p = reply("Done.").locator("p").first();
    const box = await p.boundingBox();
    const x = box.x + 20, y = box.y + 6;
    const down = (type) => cdp
      ? cdp.send("Input.dispatchTouchEvent", { type, touchPoints: type === "touchEnd" ? [] : [{ x, y }] })
      : page.evaluate(([type, x, y]) => document.elementFromPoint(x, y).dispatchEvent(new PointerEvent(
          type === "touchStart" ? "pointerdown" : "pointercancel",
          { bubbles: true, pointerId: 7, pointerType: "touch", isPrimary: true, clientX: x, clientY: y })), [type, x, y]);
    await down("touchStart");
    await sleep(300);
    console.log(await page.evaluate(([x, y]) => {
      const target = document.elementFromPoint(x, y);
      const range = document.createRange();
      range.selectNodeContents(target.firstChild ?? target);
      getSelection().removeAllRanges();
      getSelection().addRange(range);
      const ev = new MouseEvent("contextmenu", { bubbles: true, cancelable: true, clientX: x, clientY: y });
      target.dispatchEvent(ev);
      return `cancelled: ${ev.defaultPrevented}; the word unselected: ${getSelection().isCollapsed}`;
    }, [x, y]));
    await sleep(400); // past our own timer: the menu opens once
    await down(cdp ? "touchEnd" : "pointercancel");
    await page.waitForTimeout(250);
    console.log(await menu());
    console.log(`menus: ${await page.locator(".context-menu").count()}`);
    await close();
  }

  section("Select text");
  await touch(reply("Done.").locator("p").first());
  await page.locator(".context-menu [data-item=select_text]").tap();
  await page.waitForTimeout(250);
  console.log(`selected: ${await selected()}`);
  console.log(`selectable: ${await reply("Done.").evaluate(e => e.hasAttribute("data-selectable") && getComputedStyle(e.querySelector("p")).userSelect)}`);
  await shoot(page, "select text");
  section("then a dragged selection's end (no press of its own)");
  console.log(await page.evaluate(() => {
    const p = [...document.querySelectorAll("#chat .msg[tabindex] p")].find(p => p.innerText.includes("Done."));
    const r = p.getBoundingClientRect();
    const ev = new MouseEvent("contextmenu", { bubbles: true, cancelable: true, clientX: r.left + 20, clientY: r.top + 5 });
    p.dispatchEvent(ev);
    return `left to the browser: ${!ev.defaultPrevented}`;
  }));
  await page.waitForTimeout(250);
  console.log(await menu());
  await close();
  section("then a long press there is the browser's");
  await touch(reply("Done.").locator("p").first());
  console.log(await menu());
  await page.evaluate(() => getSelection().removeAllRanges());
  await close();


  section("Delete");
  await touch(user(1).locator(".bubble"));
  await page.locator(".context-menu [data-item=delete]").tap();
  await page.waitForFunction(() => !document.querySelector("#chat")?.innerText.includes("and a second question"));
  await page.locator(".toast", { hasText: "Message deleted" }).waitFor({ state: "attached" });
  await page.waitForFunction(() => document.querySelectorAll("#chat .msg.user[tabindex] .bubble").length === 1);
  console.log(`your messages: ${await page.locator("#chat .msg.user[tabindex] .bubble").evaluateAll(es => es.map(e => e.innerText.trim()).join(" | "))}`);

  section("a session in the drawer");
  await page.getByRole("button", { name: "Sessions" }).first().tap();
  await page.locator(".sidebar [data-session]").first().waitFor();
  await page.waitForTimeout(300);
  await touch(page.locator(".sidebar [data-session]").first());
  console.log(await menu());
  await shoot(page, "a session");
  await close();

  section("a worktree in the drawer");
  const worktree = page.locator(".sidebar [data-worktree]").first();
  if (await worktree.count()) {
    await touch(worktree);
    console.log(await menu());
    await shoot(page, "a worktree");
    await close();
  } else console.log("no worktree row");
  await page.keyboard.press("Escape");
  await page.waitForTimeout(300);

  section("a file of the review");
  await editor.fill("/review");
  await editor.press("Enter");
  const file = page.locator(".review-file-row").first();
  await file.waitFor();
  await page.waitForTimeout(300);
  await touch(file);
  console.log(await menu());
  await shoot(page, "a review file");
  await close();
  await page.keyboard.press("Escape");

  if (errors.length) {
    section("errors");
    console.log(errors.join("\n"));
  }
} catch (e) {
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  await shoot(page, "failed").catch(() => {});
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
