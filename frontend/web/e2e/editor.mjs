// The editor mode (/edit, Alt+E): a file of a git repository opened, typed
// into and saved; saving over a file that changed on the host refused, then
// done on purpose; lines selected and asked about (the chip in the composer,
// the prompt the agent gets); the agent's edit of the open file shown as a
// change to keep or undo, and undone. On a desktop (1440x900, the files
// docked, the chat beside, the keyboard's shortcuts) and on phones by touch
// (390x844 as an installed PWA under a notch in Chromium, 320x640): the
// drawer, the ⋯ sheet, the keys over the keyboard. Each screen prints
// whether anything overflows sideways or a control is off screen, covered,
// too small or zooms on focus.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName, part = "desktop"] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
const viewports = {
  desktop: { width: 1440, height: 900 },
  phone: { width: 390, height: 844 },
  small: { width: 320, height: 640 },
};
const viewport = viewports[part];
if (!engine || !viewport) throw new Error("usage: editor.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const pwa = part === "phone" && engineName === "chromium";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `editor-${part}`);
const home = join(dir, "home");
normalVerbosity(home);
const cwd = join(dir, "repo");
mkdirSync(home, { recursive: true });
mkdirSync(join(cwd, "src"), { recursive: true });
setTimeout(() => { console.error("editor.mjs: timed out"); process.exit(2); }, 280000).unref();

const file = join(cwd, "src", "compact.ml");
const source = `open! Import

(* Drop the oldest turns until what is left fits the window, keeping the most recent [keep_recent] tokens whatever they cost. *)
let fit (budget : Budget.t) (turns : Turn.t list) =
  let limit = budget.window - budget.reserve in
  let rec go acc used = function
    | [] -> acc
    | turn :: rest ->
      let cost = Turn.tokens turn in
      if used + cost > limit && used >= budget.keep_recent
      then acc
      else go (turn :: acc) (used + cost) rest
  in
  go [] 0 (List.rev turns)
;;
`;
writeFileSync(file, source);
writeFileSync(join(cwd, "src", "turn.mli"), "type t\n\nval tokens : t -> int\n");
writeFileSync(join(cwd, "README.md"), "# Compaction\n");
writeFileSync(join(cwd, ".gitignore"), ".prigh\n");
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

writeFileSync(join(dir, "script.json"), JSON.stringify([
  {
    text: "Pinning the system turn.",
    tool_calls: [{
      id: "e1", name: "edit",
      arguments: {
        path: "src/compact.ml",
        edits: [{
          old_text: "  let limit = budget.window - budget.reserve in\n",
          new_text: "  let limit = Budget.usable budget in\n",
        }, {
          old_text: "  go [] 0 (List.rev turns)\n",
          new_text: "  match turns with\n  | system :: rest -> system :: go [] (Turn.tokens system) (List.rev rest)\n  | [] -> []\n",
        }],
      },
    }],
  },
  { text: "The system turn is now pinned: go starts after it and never drops it." },
]));
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
const section = label => console.log(`=== ${engineName}: editor ${part}: ${label} ===`);

// What is wrong on the screen (as mobile.mjs checks): sideways overflow,
// controls off screen, covered or small (44px; 40px for the keys over the
// keyboard, the drawer's rows and the gutter's badges, dense rows), inputs
// that zoom.
const audit = page => page.evaluate(touch => {
  const out = [];
  const W = innerWidth, H = innerHeight;
  const sw = document.scrollingElement.scrollWidth;
  if (sw > W + 1) out.push(`overflows sideways: ${sw} > ${W}`);
  const page = document.querySelector(".code-page");
  if (page && page.scrollWidth > page.clientWidth + 1) out.push("the editor scrolls sideways");
  const scroller = document.querySelector(".cm-scroller");
  if (scroller && scroller.scrollWidth > scroller.clientWidth + 1) out.push("the code scrolls sideways");
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
  // An open menu covers the rest: only its items.
  const scope = document.querySelector(".code-menu") ? ".code-menu button" : ".code-page :is(button, input, [role=tab]), .composer :is(button, textarea)";
  const controls = [...document.querySelectorAll(scope)].filter(visible);
  const off = [], covered = [], small = [], zoom = [];
  for (const e of controls) {
    const r = e.getBoundingClientRect();
    const box = { left: 0, top: 0, right: W, bottom: H };
    let scrolls = false;
    for (let s = e.parentElement; s; s = s.parentElement)
      if (/(auto|scroll)/.test(getComputedStyle(s).overflowY + getComputedStyle(s).overflowX)) {
        scrolls = true;
        const b = s.getBoundingClientRect();
        box.left = Math.max(box.left, b.left); box.top = Math.max(box.top, b.top);
        box.right = Math.min(box.right, b.right); box.bottom = Math.min(box.bottom, b.bottom);
      }
    const inBox = r.left >= box.left - 1 && r.right <= box.right + 1 && r.top >= box.top - 1 && r.bottom <= box.bottom + 1;
    // Scrolled out of its box: there to scroll to.
    if (!scrolls && (r.right > W + 1 || r.left < -1 || r.bottom > H + 1)) { off.push(name(e)); continue; }
    if (!inBox) continue;
    const cx = r.left + r.width / 2, cy = r.top + r.height / 2;
    const hit = document.elementFromPoint(cx, cy);
    if (hit && !e.contains(hit) && !hit.contains(e)) covered.push(`${name(e)} under ${name(hit)}`);
    const isText = e.matches("textarea, input");
    const least = e.matches(".code-key, .code-tree.drawer .code-row, .cm-agent-badge") ? 39.5 : 43.5;
    if (touch && !isText && Math.min(r.width, r.height) < least && !e.closest(".composer"))
      small.push(`${name(e)} ${Math.round(r.width)}x${Math.round(r.height)}`);
    if (touch && isText && parseFloat(getComputedStyle(e).fontSize) < 16) zoom.push(`${name(e)} ${getComputedStyle(e).fontSize}`);
  }
  if (off.length) out.push(`off screen: ${off.join(" | ")}`);
  if (covered.length) out.push(`covered: ${covered.join(" | ")}`);
  if (small.length) out.push(`small: ${small.join(" | ")}`);
  if (zoom.length) out.push(`zooms (under 16px): ${zoom.join(" | ")}`);
  return out.length ? out.join("\n") : "ok";
}, touch);

const options = { viewport, hasTouch: touch };
if (touch && engineName !== "firefox") options.isMobile = true;
const context = await browser.newContext(options);
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", m => { if (m.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(m.text())) errors.push(`console: ${m.text()}`); });
page.on("pageerror", e => errors.push(`page: ${e.message}`));
if (pwa) {
  const cdp = await context.newCDPSession(page);
  await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
}
const tap = locator => touch ? locator.tap() : locator.click();
const shoot = async tag => {
  if (!process.env.SHOTS) return;
  await page.screenshot({ path: join(process.env.SHOTS, `editor-${engineName}-${part}-${tag}.png`) });
};
const step = async label => {
  section(label);
  console.log(await audit(page));
  await shoot(label.replace(/[^a-z0-9]+/gi, "-"));
};
const strip = async () => (await page.locator(".code-strip").innerText()).replace(/\s+/g, " ").trim();
const disk = () => readFileSync(file, "utf8");
const lineOf = n => page.locator(".cm-content .cm-line").nth(n - 1);
const mod = engineName === "webkit" && process.platform === "darwin" ? "Meta" : "Control";

try {
  await page.goto(url, { timeout: 60000 });
  await page.locator("form input[type=password]").fill(token);
  await tap(page.locator("form button[type=submit]"));
  await page.locator(".composer textarea").waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; } .cm-cursorLayer { display: none !important; }" });

  // In: /edit on a phone, Alt+E and Go to file on a desktop.
  if (touch) {
    await page.locator(".composer textarea").fill("/edit src/compact.ml");
    await page.locator(".composer textarea").press("Enter");
  } else {
    await page.keyboard.press("Alt+e");
    await page.locator(".code-tree").waitFor();
    await page.keyboard.press(`${mod}+p`);
    await page.waitForFunction(() => document.activeElement?.id === "code-filter");
    await page.keyboard.type("compml");
    await page.locator(".code-tree .code-section-title", { hasText: / of [0-9]/ }).waitFor();
    await page.locator(".code-tree .code-row.file", { hasText: "compact" }).waitFor();
    console.log(`found: ${(await page.locator(".code-tree .code-row.file").allInnerTexts()).map(t => t.replace(/\s+/g, "")).join(" | ")}`);
    await page.keyboard.press("Enter");
  }
  await page.locator(".cm-content .cm-line", { hasText: "let fit" }).waitFor();
  await step("a file open");
  console.log(`strip: ${await strip()}`);
  console.log(`lines: ${await page.locator(".cm-content .cm-line").count()}`);
  // The comment's line is long: wrapped, its rest under its indent.
  console.log(await page.evaluate(() => {
    const line = [...document.querySelectorAll(".cm-line")].find(l => l.textContent.startsWith("(* Drop"));
    const range = document.createRange();
    range.selectNodeContents(line);
    const tops = new Set([...range.getClientRects()].filter(r => r.width > 0).map(r => Math.round(r.top)));
    return `the long comment: ${tops.size > 1 ? "wrapped" : "one line"}`;
  }));

  // Typed into and saved.
  await tap(lineOf(1));
  await page.keyboard.press("End");
  await page.keyboard.type(" (* edited *)");
  await page.locator(".code-strip .code-dirty").waitFor();
  console.log(`dirty: ${await strip()}`);
  if (touch) {
    await step("typing, the keys over the keyboard");
    console.log(`keys: ${(await page.locator(".code-keys .code-key").allInnerTexts()).map(t => t.trim()).join(" ")}`);
    console.log(`composer while typing: ${await page.locator(".composer").isVisible() ? "shown" : "hidden"}`);
    await tap(page.locator(".code-key[aria-label=Indent]"));
    await tap(page.locator(".code-key[aria-label=Undo]"));
    await tap(page.locator(".code-more"));
    await page.locator(".code-menu").waitFor();
    await step("the menu");
    await tap(page.locator(".code-menu-item", { hasText: "Save" }));
  } else {
    await page.keyboard.press(`${mod}+s`);
  }
  await page.locator(".code-strip .code-dirty").waitFor({ state: "detached" });
  console.log(`saved: ${disk().split("\n")[0]}`);

  // Changed on the host meanwhile: the save is refused, then forced.
  writeFileSync(file, disk().replace("open! Import", "open! Import (* theirs *)"));
  await tap(lineOf(2));
  await page.keyboard.type("(* mine *)");
  if (touch) {
    await tap(page.locator(".code-more"));
    await tap(page.locator(".code-menu-item", { hasText: "Save" }));
  } else {
    await page.keyboard.press(`${mod}+s`);
  }
  await page.locator(".code-notice.conflict").waitFor();
  await step("a conflict");
  console.log(`notice: ${(await page.locator(".code-notice.conflict").innerText()).replace(/\s+/g, " ")}`);
  await tap(page.locator(".code-notice.conflict .btn", { hasText: "Overwrite" }));
  await page.locator(".code-notice.conflict").waitFor({ state: "detached" });
  console.log(`overwritten: ${JSON.stringify(disk().split("\n").slice(0, 2))}`);

  // Lines asked about: the chip in the composer, then the prompt.
  await tap(page.locator(".cm-content .cm-line", { hasText: "let fit" }));
  await page.keyboard.press("Home");
  for (let i = 0; i < 3; i++) await page.keyboard.press("Shift+ArrowDown");
  if (touch) {
    // On a phone, Ask opens the box under the lines.
    await tap(page.locator(".code-key.code-ask"));
    await page.locator(".cm-ask .cm-ask-input").waitFor();
    console.log(`ask box: ${(await page.locator(".cm-ask-head").innerText()).replace(/\s+/g, " ")}`);
    await page.locator(".cm-ask .cm-ask-input").fill("Pin the system turn");
    await step("the ask box under the lines");
    await page.locator(".cm-ask .cm-ask-input").press("Enter");
    await page.locator(".cm-ask").waitFor({ state: "detached" });
  } else {
    await page.keyboard.press(`${mod}+l`);
    await page.locator(".composer .code-chip").waitFor();
    console.log(`chip: ${(await page.locator(".composer .code-chip").innerText()).replace(/\s+/g, " ")}`);
    await page.locator(".composer textarea").fill("Pin the system turn");
    await step("asking about lines");
    await page.locator(".composer textarea").press("Enter");
    await page.locator(".composer .code-chip").waitFor({ state: "detached" });
  }

  // The agent's two changes, shown in the file: one kept, one undone.
  await page.locator(".code-notice.agent").waitFor();
  await page.waitForFunction(() => document.querySelectorAll(".cm-agent-change").length === 2);
  await page.waitForFunction(() => !document.querySelector(".app .running, .code-peek.running"));
  console.log(`agent: ${(await page.locator(".code-notice.agent").innerText()).replace(/\s+/g, " ")}`);
  console.log(`removed: ${(await page.locator(".cm-agent-removed").allInnerTexts()).join(" | ")}`);
  console.log(`added: ${(await page.locator(".cm-agent-added").allInnerTexts()).join(" | ")}`);
  if (touch) console.log(`peek: ${(await page.locator(".code-peek").innerText()).replace(/\s+/g, " ")}`);
  await step("the agent's change");
  const sent = await page.evaluate(() => [...document.querySelectorAll("#chat .user")].map(u => u.innerText).join("\n"));
  console.log(`sent: ${sent.replace(/\b\d\d:\d\d\b/g, "HH:MM").replace(/\s+/g, " ").trim().slice(0, 160)}`);
  console.log(`changes: ${(await page.locator(".cm-agent-head").allInnerTexts()).map(t => t.replace(/\s+/g, " ")).join(" | ")}`);
  await tap(page.locator(".cm-agent-change").first().locator(".cm-agent-act", { hasText: "Keep" }));
  await page.waitForFunction(() => document.querySelectorAll(".cm-agent-change").length === 1);
  await page.locator(".cm-agent-badge").waitFor();
  await tap(page.locator(".cm-agent-change .cm-agent-act", { hasText: "Undo" }));
  await page.waitForFunction(() => !document.querySelector(".cm-agent-change") && !document.querySelector(".code-dirty"));
  await page.locator(".code-notice.agent").waitFor({ state: "detached" });
  console.log(`first kept, second undone: ${disk().includes("Budget.usable budget") && disk().includes("go [] 0 (List.rev turns)") && !disk().includes("match turns with")}`);
  await step("a kept change's badge");
  await tap(page.locator(".cm-agent-badge"));
  await page.locator(".cm-agent-change").waitFor();
  console.log(`reopened: ${(await page.locator(".cm-agent-head").innerText()).replace(/\s+/g, " ")}`);
  await step("a kept change shown again");
  await tap(page.locator(".cm-agent-change .cm-agent-act", { hasText: "Keep" }));
  await page.locator(".cm-agent-change").waitFor({ state: "detached" });

  // The files, and the chat.
  if (touch) {
    await tap(page.locator(".code-peek"));
    await page.locator(".app.code-chat").waitFor();
    await step("the chat pulled up");
    await tap(page.locator(".code-peek"));
    await page.locator(".app.code-chat").waitFor({ state: "detached" });
    await tap(page.locator(".code-tree-toggle"));
    await page.locator(".code-tree.drawer").waitFor();
    await step("the files drawer");
    console.log(`drawer: ${(await page.locator(".code-tree.drawer .code-row").allInnerTexts()).map(t => t.replace(/\s+/g, "")).join(" | ")}`);
    // Go to file, at the drawer's foot: fuzzy over every file.
    await tap(page.locator("#code-filter"));
    await page.keyboard.type("tmli");
    await page.locator(".code-matches .code-row", { hasText: "turn" }).waitFor();
    console.log(`found: ${(await page.locator(".code-matches .code-row").allInnerTexts()).map(t => t.replace(/\s+/g, "")).join(" | ")}`);
    await step("go to file");
    await tap(page.locator(".code-matches .code-row", { hasText: "turn" }));
    await page.locator(".cm-content .cm-line", { hasText: "val tokens" }).waitFor();
    console.log(`drawer gone: ${await page.locator(".code-tree.drawer").count() === 0}`);
    console.log(`strip: ${await strip()}`);
    // A swipe sideways over the text: the other open file.
    // (Pointer events as a touch screen sends them: Playwright has taps but
    // no swipes.)
    await page.locator(".cm-content").evaluate(content => {
      const r = content.getBoundingClientRect();
      const y = r.top + 120;
      const fire = (type, x) => content.dispatchEvent(new PointerEvent(type, {
        bubbles: true, cancelable: true, pointerId: 7, pointerType: "touch", isPrimary: true, clientX: x, clientY: y,
      }));
      fire("pointerdown", 320);
      for (const x of [290, 240, 190, 140]) fire("pointermove", x);
      fire("pointerup", 140);
    });
    await page.locator(".cm-content .cm-line", { hasText: "let fit" }).waitFor();
    console.log(`swiped: ${await strip()}`);
  } else {
    // Sending opened the chat beside the code; Alt+C hides and shows it.
    console.log(`chat after sending: ${await page.locator(".app.code-chat").count() ? "beside" : "hidden"}`);
    await page.keyboard.press("Alt+c");
    await page.locator(".app.code-chat").waitFor({ state: "detached" });
    await page.keyboard.press("Alt+c");
    await page.locator(".app.code-chat").waitFor();
    await step("the chat beside");
    await tap(page.locator(".code-tree .code-row", { hasText: "src/" }).last());
    await tap(page.locator(".code-tree .code-row.file", { hasText: "turn.mli" }));
    await page.locator(".cm-content .cm-line", { hasText: "val tokens" }).waitFor();
    console.log(`tabs: ${(await page.locator(".code-tab").allInnerTexts()).map(t => t.trim()).join(" | ")}`);
    await page.locator(".cm-content").click();
    await page.keyboard.press(`${mod}+f`);
    await page.locator(".cm-search").waitFor();
    await step("find");
    await page.keyboard.press("Escape");
  }
  await page.locator(".cm-content").first().blur();
  await tap(page.locator(".code-more"));
  await tap(page.locator(".code-menu-item", { hasText: "Leave the editor" }));
  await page.locator(".code-page").waitFor({ state: "detached" });
  console.log(`left: ${await page.locator("#chat").isVisible() ? "the chat" : "?"}`);
  if (errors.length) {
    section("errors");
    console.log(errors.join("\n"));
  }
} catch (e) {
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  await shoot("FAILED").catch(() => {});
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
