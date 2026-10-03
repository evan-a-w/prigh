// Drives a real browser against `prigh serve -pi-web ... -faux-script`
// (see pi_web.sh) and prints normalised snapshots of what the page shows.
// The expected output lives in pi_web.expected.
const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [url, token, engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!url || !token || !engine) {
  throw new Error("usage: pi_web.mjs URL TOKEN chromium|firefox|webkit");
}

const browser = await engine.launch({
  headless: true,
  args:
    engineName === "chromium"
      ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"]
      : [],
});
const page = await browser.newPage({ viewport: { width: 1100, height: 700 } });
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !message.text().includes("favicon.ico")) {
    errors.push(`console: ${message.text()}`);
  }
});
page.on("pageerror", error => {
  errors.push(`page: ${error.message}`);
  console.error(`page error: ${error.stack ?? error.message}`);
});
if (process.env.DEBUG_WS) {
  page.on("websocket", ws => {
    ws.on("framesent", frame => console.error("SENT", frame.payload.slice(0, 120)));
    ws.on("framereceived", frame => console.error("RECV", frame.payload.includes("list_sessions") ? frame.payload : frame.payload.slice(0, 120)));
  });
}

const clean = text => text
  .replaceAll(process.env.TEST_CWD, "<CWD>")
  .replace(/[0-9a-f]{16}/g, "<ID>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .replace(/\d+ in · \d+ out/g, "<TOKENS>")
  .replace(/\d+% ctx/g, "<CTX>% ctx")
  .replace(/just now|\d+[mhd] ago/g, "<WHEN>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n")
  .trim();

const show = async (label, selector = "body") => {
  console.log(`=== ${engineName}: ${label} ===`);
  console.log(clean(await page.locator(selector).innerText()));
};

const waitForText = (text, count = 1) =>
  page.waitForFunction(
    ([needle, n]) => document.body.innerText.split(needle).length > n,
    [text, count],
  );

const settled = () =>
  page.waitForFunction(() => !document.querySelector(".working-indicator"));

// The Send button, not Enter: with a slash command typed, Enter first
// accepts the autocomplete suggestion.
const type = async text => {
  await page.locator("textarea.editor-input").fill(text);
  await page.locator(".editor-button.send").click();
};

// 1. No token: the backend refuses hello, the connect form appears; the
//    token is remembered and the page reloads into a session.
await page.goto(url);
await waitForText("Connect to prigh");
await show("connect form");
await page.locator(".connect-form input").fill(token);
await page.locator(".connect-form button").click();
await waitForText("No messages yet");
await page.waitForSelector(".sidebar-project");
await page.waitForFunction(() => document.querySelector(".connection-dot.online"));
await show("connected", ".sidebar");
await show("status strip", ".status-strip");

// 2. A scripted run: bash tool, write tool (diff), a subagent (agents rail),
//    final text.
await type("go");
await waitForText("all done");
await settled();
await show("transcript", ".chat");
await show("agents rail", ".agents-rail");
await waitForText("8 msgs");
await show("sidebar after the run", ".sidebar");

// 3. Backend-side slash commands render as custom messages and dialogs.
await type("/help");
await waitForText("/sessions");
await show("help", ".msg-custom");
await type("/sessions");
await page.waitForSelector(".dialog");
await show("sessions dialog", ".dialog");
await page.locator(".dialog-cancel").click();
await page.waitForSelector(".dialog", { state: "detached" });

// 4. A client-side command and a shell command.
await type("/model");
await page.waitForSelector(".picker");
await page.locator(".picker-input").fill("gpt-4o mini");
await page.keyboard.press("Enter");
await page.waitForFunction(() => document.body.innerText.includes("GPT-4o mini"));
await show("status strip after /model", ".status-strip");
await type("!printf 'shell says hi\\n'");
await waitForText("shell says hi");
await show("last chat entry after !", ".chat .msg:last-child");
await type("/name My e2e session");
await waitForText("My e2e session");
await show("sidebar after /name", ".sidebar");

// 5. A reload rejoins the same session (the id is kept in the address bar).
await page.reload();
await waitForText("all done");
await page.waitForFunction(() => document.querySelector(".connection-dot.online"));
await waitForText("My e2e session");
await show("sidebar after reload", ".sidebar");
console.log(`url keeps session: ${/[?&]session=[0-9a-f]{16}/.test(page.url())}`);

// 6. New session, then back to the old one from the sidebar.
await page.locator(".sidebar-new-btn").click();
await waitForText("No messages yet");
await show("new session", ".chat");
await page.locator(".session-item", { hasText: "My e2e session" }).click();
await waitForText("all done");
await show("switched back", ".chat .msg-user >> nth=0");

// 7. The terminal panel: a shell in the session's directory that survives
//    hiding the panel and is replaced by a fresh one after it exits.
{
  console.log(`=== ${engineName}: terminal ===`);
  const xterm = () => page.evaluate(() => document.querySelector(".xterm-rows")?.textContent ?? "");
  const xtermHas = text => page.waitForFunction(
    needle => document.querySelector(".xterm-rows")?.textContent.includes(needle), text);
  const chatHeight = () => page.evaluate(() => document.querySelector(".chat").getBoundingClientRect().height);
  const fullHeight = await chatHeight();
  await page.locator(".topbar .terminal-open").click();
  await page.locator(".xterm-rows").waitFor();
  console.log(`opening shrinks the chat: ${(await chatHeight()) < fullHeight}`);
  await page.waitForFunction(() => document.activeElement?.classList.contains("xterm-helper-textarea"));
  console.log("the terminal has the keyboard");
  await page.keyboard.type("pwd && echo sum=$((6*7))\n");
  await xtermHas("sum=42");
  console.log(`it runs in the session directory: ${(await xterm()).includes(process.env.TEST_CWD)}`);
  await page.locator(".terminal-close").click();
  await page.locator(".terminal-panel").waitFor({ state: "detached" });
  console.log(`closing restores the chat: ${(await chatHeight()) === fullHeight}`);
  await page.locator(".topbar .terminal-open").click();
  await xtermHas("sum=42");
  console.log("reopening shows the same shell");
  await page.keyboard.type("exit\n");
  await page.locator(".terminal-status", { hasText: "shell exited" }).waitFor();
  console.log("exiting the shell says so");
  await page.keyboard.type("x");
  await page.waitForFunction(() => !document.querySelector(".xterm-rows").textContent.includes("sum=42"));
  await page.keyboard.type("echo fresh\n");
  await xtermHas("fresh");
  console.log("a key starts a new shell");
  await page.locator(".topbar .terminal-open").click();
  await page.locator(".terminal-panel").waitFor({ state: "detached" });
  console.log("the topbar button hides it again");
}

await browser.close();
if (errors.length > 0) {
  console.log("=== errors ===");
  console.log(errors.join("\n"));
  process.exit(1);
}
