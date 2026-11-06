// render_ui's components (the Intelligent UI): the faux model calls the real
// tool with every component (ui_specs.mjs); the page draws them, and they
// work: a sorted table, chart tooltips on hover and keyboard focus, the
// calculator, quiz, flashcards, tabs, accordion, images opening in the
// viewer; a choice sent through ui_action (the backend's note, the component
// marked sent) and still sent after a reload; nothing overflows on a phone.
// Screenshots (SHOTS) in the themes, light and dark, desktop and phones.
// Run by prigh_web.sh; prints normalised results (ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { png } from "./png.mjs";
import { answer, charts, dashboard, learn } from "./ui_specs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: ui.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "ui");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".prigh", "sessions"), { recursive: true });
mkdirSync(join(cwd, "shots"), { recursive: true });
setTimeout(() => { console.error("ui.mjs: timed out"); process.exit(2); }, 280000).unref();

writeFileSync(join(cwd, "shots", "before.png"), png(160, 100, (x, y) => (y < 50 ? [60, 110, 190] : [230, 120, 60])));
writeFileSync(join(cwd, "shots", "after.png"), png(160, 100, (x, y) => (x < 80 ? [70, 170, 110] : [240, 200, 80])));
const withPaths = spec => JSON.parse(JSON.stringify(spec).replaceAll("\"shots/", `"${cwd}/shots/`));
const call = (id, spec) => ({ id, name: "render_ui", arguments: withPaths(spec) });
writeFileSync(join(dir, "script.json"), JSON.stringify([
  { text: "Here is where the build time goes.", tool_calls: [call("u1", dashboard), call("u2", charts), call("u3", learn), call("u4", answer)] },
  { text: "That's the overview." },
  { text: "Got it: caching the dependencies first." },
]));

let server;
const url = await new Promise((resolve, reject) => {
  server = spawn(
    backend,
    ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
      "-token", token, "-cwd", cwd],
    { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
  );
  let err = "";
  const timer = setTimeout(() => reject(new Error(`backend did not start:\n${err}`)), 30000);
  server.stderr.on("data", data => {
    err += data;
    const m = err.match(/^prigh: prigh-web on (\S+)$/m);
    if (m) { clearTimeout(timer); resolve(m[1]); }
  });
  server.on("exit", code => { clearTimeout(timer); reject(new Error(`backend exited (${code}):\n${err}`)); });
});

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const watch = page => {
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
};
const section = label => console.log(`=== ${engineName}: ui: ${label} ===`);
const tidy = text => text.replace(/\b\d\d:\d\d\b/g, "<TIME>").replace(/[ \t]*\n[\s]*/g, " | ").trim();

let page;
let shot = 0;
const screenshot = async (label, selector) => {
  if (!process.env.SHOTS) return;
  shot++;
  const path = join(process.env.SHOTS, `${engineName}-ui-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`);
  await page.waitForFunction(() => document.getAnimations().every(a => a.playState !== "running"));
  if (selector) await page.locator(selector).first().screenshot({ path });
  else await page.screenshot({ path });
};
const composer = () => page.locator(".composer textarea");
const command = async text => {
  await composer().fill(text);
  await composer().press("Enter");
  await page.waitForFunction(() => document.querySelector(".composer textarea").value === "");
};
const signIn = async () => {
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
};
const ui = n => page.locator(`#chat .tool-ui[data-call=u${n}]`);
const allShown = () => page.waitForFunction(() =>
  [1, 2, 3, 4].every(n => document.querySelector(`#chat .tool-ui[data-call=u${n}] .genui`))
  && [...document.querySelectorAll("#chat .ui-image-frame")].every(f => f.querySelector("img")?.complete));
// What the page draws, per component kind.
const census = () => page.evaluate(() => {
  const n = sel => document.querySelectorAll(`#chat ${sel}`).length;
  return [
    `charts ${n(".ui-chart")}: line ${n(".ui-chart[data-kind=line]")}, area ${n(".ui-chart[data-kind=area]")}, bar ${n(".ui-chart[data-kind=bar]")}, `
      + `stacked ${n(".ui-chart[data-kind=stacked_bar]")}, horizontal ${n(".ui-chart[data-kind=horizontal_bar]")}, pie ${n(".ui-chart[data-kind=pie]")}, `
      + `donut ${n(".ui-chart[data-kind=donut]")}, scatter ${n(".ui-chart[data-kind=scatter]")}`,
    `stats ${n(".ui-stat")}, tables ${n(".ui-table")}, progress ${n(".ui-progress")}, callouts ${n(".ui-callout")}`,
    `calculators ${n(".ui-calc")}, quizzes ${n(".ui-quiz")}, flashcards ${n(".ui-flash")}, tabs ${n(".ui-tabs")}, accordions ${n(".ui-accordion")}`,
    `forms ${n(".ui-form")}, choices ${n(".ui-choices")}, checklists ${n(".ui-checklist")}, images ${n(".ui-image")}`,
    `failed render_ui cards ${n(".tool.error")}`,
  ].join("\n");
});
// Nothing wider than the page; tables scroll in their own box.
const fits = () => page.evaluate(() => {
  const over = [...document.querySelectorAll("#chat .genui *")].filter(el => {
    const r = el.getBoundingClientRect();
    return r.width > 0 && (r.right > window.innerWidth + 1 || r.left < -1) && !el.closest(".ui-table-scroll, .code, .diff-view, [aria-hidden=true]");
  });
  return `page fits: ${document.documentElement.scrollWidth <= window.innerWidth}; overflowing: ${over.length}${over.length ? ` (${[...new Set(over.map(e => e.className))].slice(0, 4).join(", ")})` : ""}`;
});

try {
  const context = await browser.newContext({ viewport: { width: 1280, height: 900 } });
  context.setDefaultTimeout(20000);
  page = await context.newPage();
  watch(page);
  await page.goto(url);
  await signIn();
  await command("Show me the build health");
  await allShown();
  await page.waitForFunction(() => document.querySelector("#chat").innerText.includes("That's the overview."));

  section("drawn");
  console.log(await census());
  console.log(await fits());
  console.log(tidy(await ui(1).locator(".ui-callout").innerText()));
  await screenshot("dashboard", ".tool-ui[data-call=u1]");

  section("table sorting");
  const column = n => ui(1).locator(".ui-table tbody tr").evaluateAll((rows, n) => rows.map(r => r.cells[n].innerText).join(", "), 1);
  const time = ui(1).locator(".ui-table th .ui-sort", { hasText: "Time" });
  console.log(`as given: ${await column()}`);
  await time.click();
  console.log(`${await ui(1).locator(".ui-table th[aria-sort]").getAttribute("aria-sort")}: ${await column()}`);
  await time.click();
  console.log(`${await ui(1).locator(".ui-table th[aria-sort]").getAttribute("aria-sort")}: ${await column()}`);

  section("chart tooltips");
  const line = ui(1).locator(".ui-chart[data-kind=line]");
  await line.locator(".ui-chart-col").nth(3).hover();
  await page.waitForFunction(() => [...document.querySelectorAll(".ui-chart[data-kind=line] .ui-chart-tip")].some(t => t.checkVisibility()));
  console.log(`hover: ${tidy(await line.locator(".ui-chart-tip").filter({ visible: true }).first().innerText())}`);
  await screenshot("line tooltip", ".tool-ui[data-call=u1] .ui-chart[data-kind=line]");
  await page.mouse.move(0, 0);
  await ui(1).locator(".ui-chart[data-kind=donut] [tabindex='0']").first().focus();
  await page.keyboard.press("Tab");
  await page.waitForFunction(() => [...document.querySelectorAll(".ui-chart[data-kind=donut] .ui-chart-tip")].some(t => t.checkVisibility()));
  console.log(`focus: ${tidy(await ui(1).locator(".ui-chart[data-kind=donut] .ui-chart-tip").filter({ visible: true }).first().innerText())}`);
  await page.locator("body").focus();
  await ui(2).scrollIntoViewIfNeeded();
  await screenshot("charts", ".tool-ui[data-call=u2]");

  section("calculator");
  const outputs = () => ui(3).locator(".ui-calc-outputs").innerText().then(tidy);
  console.log(await outputs());
  await ui(3).locator(".ui-calc-field input:not([type=range])").first().fill("120");
  console.log(await outputs());
  await ui(3).locator(".ui-calc-field input:not([type=range])").nth(1).fill("x");
  console.log(await outputs());

  section("quiz, flashcards, tabs, accordion");
  await ui(3).locator(".ui-option", { hasText: "prettier" }).click();
  await ui(3).locator(".ui-option", { hasText: "effects" }).click();
  console.log(tidy(await ui(3).locator(".ui-quiz").innerText()));
  await screenshot("quiz", ".tool-ui[data-call=u3] .ui-tabs");
  await ui(3).locator(".ui-tab", { hasText: "Flashcards" }).click();
  await ui(3).locator(".ui-flash-card").click();
  console.log(`flipped: ${await ui(3).locator(".ui-flash-card").getAttribute("aria-label")}`);
  await screenshot("flashcard", ".tool-ui[data-call=u3] .ui-tabs");
  await ui(3).locator(".ui-tab", { hasText: "Plan" }).click();
  console.log(tidy(await ui(3).locator(".ui-tabs [role=tabpanel]").innerText()));
  await ui(3).locator(".ui-acc-head", { hasText: "The change" }).click();
  console.log(`accordion open: ${await ui(3).locator(".ui-acc-head[aria-expanded=true]").allInnerTexts()}`);
  await screenshot("learn", ".tool-ui[data-call=u3]");

  section("images");
  console.log(await ui(4).locator(".ui-image img").evaluateAll(imgs => imgs.map(i => `${i.alt} ${i.naturalWidth}x${i.naturalHeight}`).join(", ")));
  await ui(4).locator(".ui-image img").first().click();
  await page.locator("dialog.viewer[open] .viewer-img").waitFor();
  console.log(`viewer: ${await page.locator("dialog.viewer .viewer-count").innerText()}`);
  await page.keyboard.press("Escape");
  await page.locator("dialog.viewer[open]").waitFor({ state: "detached" });
  await screenshot("answer", ".tool-ui[data-call=u4]");

  for (const theme of ["dark", "light"]) {
    await command(`/theme ${theme}`);
    await page.waitForFunction(t => document.documentElement.dataset.theme === t, theme);
    for (const n of [1, 2, 3, 4]) {
      await ui(n).scrollIntoViewIfNeeded();
      await screenshot(`${theme} ${n}`, `.tool-ui[data-call=u${n}]`);
    }
  }
  await command("/theme original");

  section("a choice sent");
  await ui(4).locator(".ui-choice", { hasText: "Cache deps" }).click();
  await page.waitForFunction(() => document.querySelector("#chat").innerText.includes("Got it: caching"));
  console.log(tidy(await ui(4).locator(".ui-choices").innerText()));
  console.log(`buttons disabled: ${await ui(4).locator(".ui-choice[disabled]").count()}`);
  console.log(`note: ${tidy(await page.locator("#chat .note", { hasText: "Sent" }).last().innerText())}`);
  await screenshot("sent", ".tool-ui[data-call=u4]");

  section("after a reload");
  await page.reload();
  await signIn();
  await allShown();
  console.log(tidy(await ui(4).locator(".ui-choices").innerText()));
  console.log(`form still open: ${await ui(4).locator(".ui-form button[type=submit]:not([disabled])").count()}`);
  await context.close();

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], [390, 844, true]]) {
    const tag = `${w}x${h}${safe ? " pwa" : ""}`;
    const options = { viewport: { width: w, height: h }, hasTouch: true };
    if (engineName !== "firefox") options.isMobile = true;
    const phone = await browser.newContext(options);
    phone.setDefaultTimeout(20000);
    page = await phone.newPage();
    watch(page);
    if (safe && engineName === "chromium") {
      const cdp = await phone.newCDPSession(page);
      await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
    }
    await page.goto(url);
    await signIn();
    await allShown();
    section(`phone ${tag}`);
    console.log(await fits());
    if (!safe) {
      for (const n of [1, 2, 3, 4]) {
        await ui(n).scrollIntoViewIfNeeded();
        await screenshot(`phone ${tag} ${n}`, `.tool-ui[data-call=u${n}]`);
      }
      await ui(1).locator(".ui-chart[data-kind=bar] .ui-chart-col, .ui-chart[data-kind=bar] [tabindex='0']").first().tap();
      console.log(`tap shows a tooltip: ${await page.evaluate(() => [...document.querySelectorAll(".ui-chart[data-kind=bar] .ui-chart-tip")].some(t => t.checkVisibility()))}`);
    } else {
      await ui(3).scrollIntoViewIfNeeded();
      await screenshot(`phone ${tag}`);
    }
    await phone.close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure").catch(() => {});
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => server.kill(), () => browser.close());
}
