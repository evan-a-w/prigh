// ./test's browser e2e (SUITE=core): what only a real browser catches on
// the main flows, in one script, one backend, one browser and three page
// loads. Signing in; a prompt streamed back with a tool call; a second
// session and back from the sidebar; Ctrl+K and Esc; the backend
// restarting under the page, which reconnects to the same session and goes
// on; then the session on a phone, by touch: the chat and the drawer with
// nothing overflowing, covered or under a thumb's size. Every other script
// (./test --full) goes into what these touch in depth.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE (the built site),
// TEST_DIR, TOKEN, and optionally SHOTS (a directory for screenshots).
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { hostName } from "./host_name.mjs";
import { audit } from "./phone_audit.mjs";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: smoke.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "smoke");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
normalVerbosity(home);
setTimeout(() => { console.error("smoke.mjs: timed out"); process.exit(2); }, 120000).unref();

// The model's replies in order, before and after the backend restarts.
writeFileSync(join(dir, "script.json"), JSON.stringify([
  { text: "let me look", tool_calls: [{ id: "c1", name: "bash", arguments: { command: "printf 'one\\ntwo\\nthree\\n'" } }] },
  { text: "three lines" },
  { text: "second session reply" },
]));
writeFileSync(join(dir, "script2.json"), JSON.stringify([{ text: "back again" }]));

// ---- the backend, which the script restarts on the same port

let server;
const startServer = (listen, scriptFile) =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-web", listen, "-web-root", site, "-faux-script", scriptFile, "-token", token, "-cwd", cwd],
      { env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh" }, stdio: ["ignore", "ignore", "pipe"] },
    );
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
    server.on("exit", resolve);
    server.kill();
  });

const url = await startServer("127.0.0.1:0", join(dir, "script.json"));
const port = new URL(url).port;

// ---- the browser

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const watch = page => {
  page.on("console", m => {
    // The backend going away is logged by the browser.
    if (m.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket|ERR_CONNECTION_REFUSED|establish a connection/.test(m.text()))
      errors.push(`console: ${m.text()}`);
  });
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
};

const clean = text => text
  .replaceAll(cwd, "<CWD>")
  .replace(/127\.0\.0\.1:\d+/g, "127.0.0.1:<PORT>")
  .replace(/[0-9a-f]{16}/g, "<ID>")
  .replace(/just now|\d+[smhd] ago/g, "<AGE>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .replace(/(Yesterday )?\b\d\d:\d\d\b/g, "<TIME>")
  .replaceAll(hostName(), "<HOSTNAME>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "" && line !== "Today")
  .join("\n");
const section = label => console.log(`=== ${engineName}: smoke: ${label} ===`);
let shot = 0;
const screenshot = async (page, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({ path: join(process.env.SHOTS, `smoke-${engineName}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`) });
};

const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
const page = await context.newPage();
watch(page);
const composer = () => page.locator(".composer textarea");
const bodyHas = text => page.waitForFunction(t => document.body.innerText.includes(t), text);
const bodyLacks = text => page.waitForFunction(t => !document.body.innerText.includes(t), text);
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const send = async text => {
  await composer().fill(text);
  await page.locator(".btn.send").click();
};
const sessionId = () => new URL(page.url()).searchParams.get("session");
// From the last prompt to the end of the chat, tool calls opened.
const showTurn = async label => {
  await page.evaluate(() => document.querySelectorAll("details:not(.image):not(.note)").forEach(d => { d.open = true; }));
  section(label);
  console.log(clean(await page.evaluate(() => {
    const prompts = document.querySelectorAll(".chat .msg.user");
    const last = prompts[prompts.length - 1];
    const start = last && last.parentElement.classList.contains("noted") ? last.parentElement : last;
    const turn = [];
    for (let e = start; e; e = e.nextElementSibling) turn.push(e.innerText);
    return turn.join("\n");
  })));
  await screenshot(page, label);
};
const sidebarSessions = () => page.locator(".sidebar .session .session-title").evaluateAll(es => es.map(e => e.innerText.trim()).join(" | "));

try {
  // Signing in: the form, then the app (the page reloads into it).
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  section("sign in");
  console.log(clean(await page.locator("form").innerText()));
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  await page.locator(".chat .empty").waitFor();
  console.log(`signed in: the composer and the sidebar: ${await page.locator(".sidebar").count() > 0}`);

  // A prompt, a streamed reply with a bash call and its output.
  await send("count some lines");
  await bodyHas("three lines");
  await idle();
  await page.locator(".tool").first().waitFor();
  await showTurn("a run with a tool call");
  const first = sessionId();
  console.log(`the session in the address: ${/^[0-9a-f]{16}$/.test(first ?? "")}`);

  // A second session, then back to the first from the sidebar.
  await page.locator(".sidebar").getByRole("button", { name: "New session", exact: true }).click();
  await bodyLacks("three lines");
  await page.locator(".chat .empty").waitFor();
  await send("hello again");
  await bodyHas("second session reply");
  await idle();
  await page.waitForFunction(() => document.querySelectorAll(".sidebar .session").length === 2);
  section("sessions");
  console.log(`the sidebar: ${clean(await sidebarSessions())}`);
  console.log(`a new session in the address: ${sessionId() !== first}`);
  await page.locator(".sidebar .session", { hasText: "count some lines" }).click();
  await bodyHas("three lines");
  await bodyLacks("second session reply");
  console.log(`back to the first: ${sessionId() === first}`);

  // Ctrl+K: the control centre takes the focus, filters, and Esc gives the
  // focus back to the editor.
  await composer().focus();
  await composer().press("Control+k");
  await page.locator("#control-centre").waitFor();
  section("Ctrl+K");
  console.log(`focus in its filter: ${await page.evaluate(() => document.activeElement?.id === "control-input")}`);
  await page.locator("#control-input").fill("verbosity");
  await page.waitForFunction(() => /verbosity/i.test(document.querySelector("#control-list > .selected .control-label")?.textContent ?? ""));
  console.log(`"verbosity" selects: ${clean(await page.locator("#control-list > .selected .control-label").innerText())}`);
  await screenshot(page, "Ctrl+K");
  await page.keyboard.press("Escape");
  await page.locator("#control-centre").waitFor({ state: "detached" });
  await page.waitForFunction(() => document.activeElement?.closest(".composer"));
  console.log("Esc: closed, the focus back in the editor");

  // The backend restarts: the page says so, reconnects to the same session
  // and carries on.
  await stopServer();
  await bodyHas("reconnecting");
  section("the backend restarts");
  console.log(clean(await page.locator("body").innerText()).split("\n").filter(l => /connect/i.test(l))
    .map(l => l.replace(/reconnecting \(attempt \d+, next in [\d.]+s\)…/, "reconnecting…")).join("\n"));
  await startServer(`127.0.0.1:${port}`, join(dir, "script2.json"));
  await bodyLacks("Connection lost");
  await bodyHas("three lines");
  console.log(`the same session after reconnecting: ${sessionId() === first}`);
  await send("still there?");
  await bodyHas("back again");
  await idle();
  await showTurn("after reconnecting");

  // A phone, by touch, signed in as the desktop was: the session's chat and
  // the drawer.
  const state = await context.storageState();
  const options = { viewport: { width: 390, height: 844 }, hasTouch: true, storageState: state };
  if (engineName !== "firefox") options.isMobile = true;
  const phoneContext = await browser.newContext(options);
  const phone = await phoneContext.newPage();
  watch(phone);
  await phone.goto(page.url());
  await phone.locator(".composer textarea").waitFor();
  await phone.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("back again"));
  // Toasts come and go with timing; the audit measures controls at rest.
  await phone.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; }" });
  section("a phone: the chat");
  console.log(`narrow: ${await phone.locator(".app.narrow").count() > 0}`);
  console.log(await audit(phone));
  await screenshot(phone, "phone chat");
  await phone.getByRole("button", { name: "Sessions" }).first().tap();
  await phone.locator(".app.narrow.sidebar-open").waitFor();
  section("a phone: the drawer");
  console.log(await audit(phone));
  console.log(`its sessions: ${clean(await phone.locator(".sidebar .session .session-title").evaluateAll(es => es.map(e => e.innerText.trim()).join(" | ")))}`);
  await screenshot(phone, "phone drawer");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (e) {
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  process.exitCode = 1;
} finally {
  await end(() => stopServer(), () => browser.close());
}
