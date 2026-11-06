// Copying on a page without the Clipboard API, as on a backend's plain http
// address other than localhost (not a secure context): the API is taken
// away before the page loads. A code block's Copy and a message's Copy
// still put the text on the clipboard (read back by pasting it), say so,
// and leave the focus and the selection where they were.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: clipboard.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "clipboard");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("clipboard.mjs: timed out"); process.exit(2); }, 240000).unref();

writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([{ text: "Run this:\n\n```sh\nmake check && echo 'all good'\n```\n\nThen push." }]),
);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
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
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
context.setDefaultTimeout(20000);
await context.addInitScript(() => {
  Object.defineProperty(Navigator.prototype, "clipboard", { value: undefined, configurable: true });
  Object.defineProperty(navigator, "clipboard", { value: undefined, configurable: true });
});
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget/.test(message.text()))
    errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: clipboard: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-clipboard-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

const composer = () => page.locator(".composer textarea");
const theReply = () => page.locator("#chat .msg.assistant", { hasText: "Then push." });
// What is on the clipboard as plain text: what a paste into the composer
// gets (and does not insert).
const pasted = async () => {
  await composer().focus();
  await composer().evaluate(ta => {
    window.pastedText = undefined;
    ta.addEventListener("paste", ev => {
      window.pastedText = ev.clipboardData.getData("text/plain");
      ev.preventDefault();
    }, { once: true });
  });
  await page.keyboard.press("ControlOrMeta+v");
  await page.waitForFunction(() => window.pastedText !== undefined);
  return JSON.stringify(await page.evaluate(() => window.pastedText));
};
// Where the focus is and what is selected, and no textarea left behind.
const state = () => page.evaluate(() => {
  const active = document.activeElement;
  const where = active === document.body ? "body" : `${active.tagName.toLowerCase()}${active.closest(".composer") ? " (composer)" : ""}`;
  const ta = document.querySelector(".composer textarea");
  return `focus: ${where}; composer caret: ${ta.selectionStart}-${ta.selectionEnd}; `
    + `textareas: ${document.querySelectorAll("textarea").length}; scrolled: ${window.scrollY}`;
});

try {
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  section("the page");
  console.log(await page.evaluate(() => `clipboard API: ${typeof navigator.clipboard}`));

  await composer().fill("How do I check it?");
  await composer().press("Enter");
  await theReply().waitFor();
  await page.waitForFunction(() => !document.querySelector("#chat .msg.assistant.streaming"));

  // A code block's Copy: Copied on the button, its code on the clipboard;
  // the composer keeps the focus and its caret.
  section("code block");
  await composer().fill("draft");
  await composer().evaluate(ta => ta.setSelectionRange(1, 3));
  const button = theReply().locator(".copyable button.copy");
  await button.click();
  await theReply().locator(".copyable button.copy.copied").waitFor();
  console.log(`button: ${await button.innerText()}`);
  await screenshot("code block copied");
  console.log(await state());
  console.log(`clipboard: ${await pasted()}`);

  // The message's menu: Copy, acknowledged once it is on the clipboard.
  section("message");
  await theReply().click({ button: "right", position: { x: 40, y: 8 } });
  await page.locator(".context-menu.placed [data-item=copy_message]").click();
  const toast = page.locator(".toast", { hasText: "Copied" }).first();
  await toast.waitFor();
  console.log(`toast: ${await toast.innerText()}`);
  await screenshot("message copied");
  console.log(await state());
  console.log(`clipboard: ${await pasted()}`);

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(await page.evaluate(() => document.querySelector(".toasts")?.innerText));
  console.error(await page.evaluate(() => document.querySelector("#chat")?.outerHTML.slice(0, 3000)));
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
