// Drives a real browser against `prigh serve -prigh-web ... -faux-script`
// (see prigh_web.sh) and prints normalised snapshots of what the page shows.
// It owns the backend so that it can restart it. The expected output lives
// in prigh_web.expected.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE (the built site),
// TEST_DIR (scratch: home/, cwd/, the faux scripts), TOKEN, and optionally
// SHOTS (a directory for screenshots of each step).
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { png, pngSize } from "./png.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: prigh_web.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TEST_DIR: dir, TOKEN: token } = process.env;
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });

// The model's replies, in order, across every session of one backend run.
const script = [
  { text: "let me look", tool_calls: [{ id: "c1", name: "bash", arguments: { command: "printf 'one\\ntwo\\nthree\\n'" } }] },
  { text: "three lines" },
  { text: "opening it", tool_calls: [{ id: "r1", name: "read", arguments: { path: "flag.png" } }] },
  { text: "a red flag with a blue stripe" },
  { text: "you sent me two images" },
  { text: "second session reply" },
];
// After the restart.
const script2 = [{ text: "back again" }];
writeFileSync(join(dir, "script.json"), JSON.stringify(script));
writeFileSync(join(dir, "script2.json"), JSON.stringify(script2));

const blue = [40, 80, 220];
const red = [220, 40, 40];
writeFileSync(join(cwd, "flag.png"), png(24, 16, (x, y) => (y < 4 ? blue : red)));
const pasted = png(20, 10, () => [30, 160, 90]);
const dropped = png(12, 30, () => [200, 200, 40]);

// ---- the backend

let server;
const startServer = (listen, scriptFile) =>
  new Promise((resolve, reject) => {
    server = spawn(
      backend,
      ["serve", "-prigh-web", listen, "-prigh-web-root", site, "-faux-script", scriptFile,
        "-token", token, "-cwd", cwd],
      { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
    );
    let err = "";
    server.stderr.on("data", data => {
      err += data;
      process.env.DEBUG_SERVER && process.stderr.write(data);
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
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
const page = await context.newPage();
const errors = [];
page.on("console", message => {
  const text = message.text();
  // The backend going away (step 7) is logged by the browser.
  if (message.type() === "error" && !/favicon\.ico|WebSocket|ERR_CONNECTION_REFUSED/.test(text)) {
    errors.push(`console: ${text}`);
  }
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .replaceAll(cwd, "<CWD>")
  .replace(/[0-9a-f]{16}/g, "<ID>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .replace(/\d+% ctx/g, "<CTX>% ctx")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");

const section = label => console.log(`=== ${engineName}: ${label} ===`);

let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  const name = `${engineName}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`;
  await page.screenshot({ path: join(process.env.SHOTS, name) });
};

// Tool calls and thinking are collapsible: open them so that their text shows.
const show = async (label, selector) => {
  await page.evaluate(() => document.querySelectorAll("details").forEach(d => { d.open = true; }));
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await screenshot(label);
};

// The top bar's text, with each select showing its chosen option.
const showHeader = async label => {
  section(label);
  console.log(clean(await page.evaluate(() =>
    [...document.querySelectorAll("header *")]
      .filter(e => e.tagName === "SELECT" || (e.children.length === 0 && e.tagName !== "OPTION"))
      .map(e => (e.tagName === "SELECT" ? `[${e.selectedOptions[0]?.text ?? ""}]` : e.textContent))
      .join("\n"))));
};

// From the last prompt to the end of the chat.
const showTurn = async label => {
  await page.evaluate(() => document.querySelectorAll("details").forEach(d => { d.open = true; }));
  section(label);
  console.log(clean(await page.evaluate(() => {
    const prompts = document.querySelectorAll(".chat .msg.user");
    const turn = [];
    for (let e = prompts[prompts.length - 1]; e; e = e.nextElementSibling) turn.push(e.innerText);
    return turn.join("\n");
  })));
  await screenshot(label);
};

// What each image [locator] matches shows, once loaded.
const showImages = async (label, locator) => {
  const describe = () => locator.evaluateAll(list =>
    list.every(img => img.complete && img.naturalWidth > 0)
      ? list.map(img => `${img.src.slice(0, img.src.indexOf(",") + 1)} ${img.naturalWidth}x${img.naturalHeight}`)
      : null);
  let images;
  while (!(images = await describe())) await page.waitForTimeout(50);
  section(label);
  console.log(images.join("\n"));
};

const bodyHas = text => page.waitForFunction(t => document.body.innerText.includes(t), text);
const bodyLacks = text => page.waitForFunction(t => !document.body.innerText.includes(t), text);
const composer = () => page.locator(".composer textarea");
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));

const send = async text => {
  await composer().fill(text);
  await page.locator(".btn.send").click();
};

const sessionId = () => new URL(page.url()).searchParams.get("session");

// A second client asks the backend what the session's messages are.
const backendMessages = session =>
  page.evaluate(
    ([token, session]) =>
      new Promise((resolve, reject) => {
        const ws = new WebSocket(`${location.origin.replace(/^http/, "ws")}/ws`);
        const replies = {};
        ws.onerror = () => reject(new Error("websocket failed"));
        ws.onmessage = event => {
          const msg = JSON.parse(event.data);
          if (msg.id === undefined) return;
          if (msg.error) reject(new Error(JSON.stringify(msg.error)));
          replies[msg.id] = msg.result;
          if (msg.id === 1) ws.send(JSON.stringify({ id: 2, method: "get_messages", params: {} }));
          if (msg.id === 2) { ws.close(); resolve(msg.result); }
        };
        ws.onopen = () =>
          ws.send(JSON.stringify({ id: 1, method: "hello", params: { name: "e2e", token, session } }));
      }),
    [token, session],
  );

// The bytes of a PNG as a File, delivered the way a browser does.
const deliver = (kind, bytes, name) =>
  page.evaluate(
    ([kind, bytes, name]) => {
      const data = new DataTransfer();
      data.items.add(new File([new Uint8Array(bytes)], name, { type: "image/png" }));
      const target = document.querySelector(".composer textarea");
      const init = { bubbles: true, cancelable: true };
      let event;
      if (kind === "paste") {
        // Firefox ignores [clipboardData] in a script's ClipboardEvent.
        event = new ClipboardEvent("paste", init);
        Object.defineProperty(event, "clipboardData", { value: data });
      } else {
        event = new DragEvent("drop", { ...init, dataTransfer: data });
      }
      target.dispatchEvent(event);
    },
    [kind, [...bytes], name],
  );

try {
  // 1. No token saved: hello is refused and the sign-in form appears;
  //    signing in remembers the password and reloads into the app.
  await page.goto(url);
  await page.waitForSelector("form input[type=password]");
  await show("sign-in page", "form");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer().waitFor();
  await page.waitForSelector(".sidebar");
  await show("signed in: sidebar", ".sidebar");
  await show("signed in: chat", ".chat");
  await showHeader("signed in: top bar");

  // 2. A scripted run with a bash tool call and its output.
  await send("count some lines");
  await bodyHas("three lines");
  await idle();
  await page.locator(".tool").first().waitFor();
  await showTurn("bash run");
  await showHeader("top bar after a run");

  // 3. The model reads a real PNG: the tool result shows it.
  await send("what does flag.png look like?");
  await bodyHas("a red flag with a blue stripe");
  await idle();
  await showTurn("read an image");
  await showImages("the read tool's image", page.locator(".tool img"));

  // 4. An image pasted and one dropped into the composer go with the prompt.
  await deliver("paste", pasted, "pasted.png");
  await deliver("drop", dropped, "dropped.png");
  await page.waitForFunction(() => document.querySelectorAll(".composer img").length === 2);
  await showImages("attached in the composer", page.locator(".composer img"));
  await screenshot("attached");
  await send("what are these?");
  await bodyHas("you sent me two images");
  await idle();
  console.log(`composer cleared: ${(await page.locator(".composer img").count()) === 0}`);
  await showImages("the prompt's images", page.locator(".msg.user").last().locator("img"));
  {
    const messages = await backendMessages(sessionId());
    const user = messages.filter(m => m.role === "user").at(-1);
    section("the backend's copy of the prompt");
    console.log(user.text);
    for (const image of user.images ?? []) {
      console.log(`${image.mime_type} ${pngSize(Buffer.from(image.data, "base64"))}`);
    }
  }

  // 5. A reload rejoins the same session (its id is in the address bar).
  const first = sessionId();
  console.log(`url has the session: ${/^[0-9a-f]{16}$/.test(first ?? "")}`);
  await page.reload();
  await composer().waitFor();
  await bodyHas("you sent me two images");
  console.log(`same session after reload: ${sessionId() === first}`);
  await page.locator(".sidebar .session").first().waitFor();
  await show("sidebar after reload", ".sidebar");

  // 6. A new session, then back to the first from the sidebar.
  await page.locator(".sidebar").getByRole("button", { name: /new session/i }).click();
  await bodyLacks("three lines");
  await show("new session", ".chat");
  await send("hello again");
  await bodyHas("second session reply");
  await idle();
  console.log(`new session in the url: ${sessionId() !== first}`);
  await page.waitForFunction(() => document.querySelectorAll(".sidebar .session").length === 2);
  await show("two sessions", ".sidebar");
  await page.locator(".sidebar .session", { hasText: "count some lines" }).click();
  await bodyHas("three lines");
  await bodyLacks("second session reply");
  console.log(`switched back: ${sessionId() === first}`);
  await show("switched back", ".chat .msg.user");

  // 7. The backend restarts: the page says so, reconnects to the same
  //    session and carries on.
  await stopServer();
  await bodyHas("reconnecting");
  section("backend gone");
  console.log(clean(await page.locator("body").innerText()).split("\n").filter(l => /connect/i.test(l)).join("\n"));
  await screenshot("backend gone");
  await startServer(`127.0.0.1:${port}`, join(dir, "script2.json"));
  await bodyLacks("Connection lost");
  await bodyHas("three lines");
  console.log(`same session after reconnecting: ${sessionId() === first}`);
  await send("still there?");
  await bodyHas("back again");
  await idle();
  await showTurn("after reconnecting");

  // The layout at phone size.
  await page.setViewportSize({ width: 390, height: 844 });
  await screenshot("phone");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  await stopServer();
}
