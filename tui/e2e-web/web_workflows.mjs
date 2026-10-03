const { chromium, firefox, webkit, devices } = await import(process.env.PLAYWRIGHT_MODULE);

const [url, token, engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!url || !token || !engine) {
  throw new Error("usage: web_workflows.mjs URL TOKEN chromium|firefox|webkit");
}

const browser = await engine.launch({
  headless: true,
  args:
    engineName === "chromium"
      ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"]
      : [],
});
const page = await browser.newPage({ viewport: { width: 960, height: 560 } });
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !message.text().includes("favicon.ico")) {
    errors.push(`console: ${message.text()}`);
  }
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .replaceAll(process.env.TEST_CWD, "<CWD>")
  .replace(/ws:\/\/127\.0\.0\.1:\d+\/ws/g, "<BACKEND>")
  .replace(/session [0-9a-f]+/g, "session <ID>")
  .replace(/─+/g, "<RULE>")
  .replace(/ctx:\d+% [0-9.km]+/g, "ctx:<USAGE>")
  .replace(/\$\d+\.\d+/g, "$<COST>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n")
  .trim();

const screen = async label => {
  const lines = await page.locator("pre.screen .line").allTextContents();
  const text = clean(lines.filter(line => line.trim() !== "").join("\n"));
  console.log(`=== ${engineName}: ${label} ===`);
  console.log(text);
};

// A phone: taps must open the keyboard and keep it open, the grid must
// shrink to the visible viewport when the keyboard takes part of the screen,
// text committed by an IME must arrive, and swipes must scroll.
const mobile = async () => {
  const ctx = await browser.newContext({ ...devices["Pixel 7"] });
  const phone = await ctx.newPage();
  phone.on("pageerror", error => errors.push(`phone: ${error.message}`));
  await phone.goto(url);
  await phone.locator("#token").click();
  await phone.keyboard.type(token);
  await phone.locator("#connect-form button").click();
  await phone.waitForFunction(() =>
    document.body.textContent.includes("/help for commands")
    && !document.body.textContent.includes("connecting…"));
  const active = () => phone.evaluate(() => document.activeElement?.id);
  const rows = () => phone.evaluate(() => document.querySelectorAll("pre.screen .line").length);
  const firstLine = () => phone.evaluate(() =>
    [...document.querySelectorAll("pre.screen .line")].map(l => l.textContent.trim()).find(Boolean));
  const expect = (label, ok) => { if (!ok) throw new Error(`phone: ${label}`); };

  await phone.evaluate(() => document.activeElement.blur());
  await phone.touchscreen.tap(200, 300);
  expect(`tap focuses the keyboard input (active=${await active()})`, await active() === "keyboard-input");
  let blurs = 0;
  await phone.exposeFunction("onBlur", () => blurs++);
  await phone.evaluate(() => document.getElementById("keyboard-input").addEventListener("blur", () => window.onBlur()));
  await phone.touchscreen.tap(150, 250);
  await phone.waitForTimeout(100);
  expect(`tapping again keeps the keyboard (blurs=${blurs})`, blurs === 0 && await active() === "keyboard-input");

  const tallRows = await rows();
  await phone.setViewportSize({ width: 412, height: 400 });
  await phone.waitForFunction(rows => document.querySelectorAll("pre.screen .line").length < rows, tallRows);
  const prompt = await phone.evaluate(() =>
    [...document.querySelectorAll("pre.screen .line")].map(l => l.textContent.trim()).filter(Boolean).at(-2));
  expect(`the prompt stays visible above the keyboard (${prompt})`, prompt.startsWith(">"));

  await phone.locator("#keyboard-input").evaluate(input => {
    input.value = "ime";
    input.dispatchEvent(new Event("input", { bubbles: true }));
  });
  await phone.waitForFunction(() => document.body.textContent.includes("> ime"));
  for (let i = 1; i <= 4; i++) {
    await phone.keyboard.press("Enter");
    await phone.waitForFunction(i => document.body.textContent.split("faux reply").length > i, i);
    await phone.keyboard.type(`more ${i}`);
  }

  await phone.setViewportSize({ width: 412, height: 180 });
  await phone.waitForTimeout(200);
  const top = await firstLine();
  const cdp = await ctx.newCDPSession(phone);
  const swipe = async (from, to) => {
    await cdp.send("Input.dispatchTouchEvent", { type: "touchStart", touchPoints: [{ x: 200, y: from }] });
    const step = from < to ? 20 : -20;
    for (let y = from + step; step > 0 ? y <= to : y >= to; y += step) {
      await cdp.send("Input.dispatchTouchEvent", { type: "touchMove", touchPoints: [{ x: 200, y }] });
    }
    await cdp.send("Input.dispatchTouchEvent", { type: "touchEnd", touchPoints: [] });
  };
  await swipe(30, 170);
  await phone.waitForTimeout(200);
  expect(`dragging down scrolls up (${top} -> ${await firstLine()})`, await firstLine() !== top);
  expect(`swiping does not close the keyboard (blurs=${blurs})`, blurs === 0);
  await swipe(170, 30);
  await phone.waitForTimeout(200);
  expect(`dragging back returns (${await firstLine()})`, await firstLine() === top);
  await ctx.close();
};

// The terminal panel: a shell on the backend that keeps running while the
// panel is hidden, gives the keyboard back to the app when closed, and starts
// a fresh shell after the old one exits.
const terminal = async () => {
  const expect = (label, ok) => {
    if (!ok) throw new Error(`terminal: ${label}`);
    console.log(label);
  };
  const xterm = () => page.evaluate(() =>
    document.querySelector(".xterm-rows")?.textContent ?? "");
  const appRows = () => page.locator("pre.screen .line").count();
  console.log(`=== ${engineName}: terminal ===`);
  const fullRows = await appRows();
  await page.locator(".terminal-open").click();
  await page.locator(".xterm-rows").waitFor();
  await page.waitForFunction(rows =>
    document.querySelectorAll("pre.screen .line").length < rows, fullRows);
  expect("opening shrinks the app", true);
  await page.waitForFunction(() =>
    document.activeElement?.classList.contains("xterm-helper-textarea"));
  expect("the terminal has the keyboard", true);
  await page.keyboard.type("cd .. && cd - >/dev/null && pwd && echo sum=$((6*7))\n");
  await page.waitForFunction(() =>
    document.querySelector(".xterm-rows").textContent.includes("sum=42"));
  expect("it runs in the session directory", (await xterm()).includes(process.env.TEST_CWD));
  await page.locator(".terminal-close").click();
  await page.waitForFunction(rows =>
    document.querySelectorAll("pre.screen .line").length === rows, fullRows);
  await page.waitForFunction(() => document.activeElement?.id === "keyboard-input");
  expect("closing restores the app and its keyboard", true);
  await page.keyboard.type("app again", { delay: 20 });
  await page.waitForFunction(() => document.body.textContent.includes("> app again"));
  expect("the app takes keys again", true);
  await page.locator(".terminal-open").click();
  await page.waitForFunction(() =>
    document.querySelector(".xterm-rows")?.textContent.includes("sum=42"));
  expect("reopening shows the same shell", true);
  await page.keyboard.type("exit\n");
  await page.locator(".terminal-status", { hasText: "shell exited" }).waitFor();
  expect("exiting the shell says so", true);
  await page.keyboard.type("x");
  await page.waitForFunction(() =>
    !document.querySelector(".xterm-rows").textContent.includes("sum=42"));
  await page.keyboard.type("echo fresh\n");
  await page.waitForFunction(() =>
    document.querySelector(".xterm-rows").textContent.includes("fresh"));
  expect("a key starts a new shell", true);
  await page.locator(".terminal-close").click();
  await page.waitForFunction(rows =>
    document.activeElement?.id === "keyboard-input"
    && document.querySelectorAll("pre.screen .line").length === rows, fullRows);
  for (let i = 0; i < "app again".length; i++) await page.keyboard.press("Backspace");
  await page.waitForFunction(() =>
    !document.body.textContent.includes("app again")
    && document.body.textContent.includes("deepseek-flash"));
};

try {
  await page.goto(url);
  await page.bringToFront();
  await page.locator("#connect-form").waitFor();
  console.log(`=== ${engineName}: authentication ===`);
  console.log(clean(await page.locator("#connect-form").textContent()));
  console.log(clean(`backend=${await page.locator("#backend").inputValue()}`));
  console.log(`token-in-url=${new URL(page.url()).searchParams.has("token")}`);

  // Native text insertion is what the connect form depends on; fail loudly if
  // this environment's browser cannot deliver it.
  await page.evaluate(() => {
    const input = document.createElement("input");
    input.id = "scratch";
    document.getElementById("root").appendChild(input);
  });
  await page.locator("#scratch").click();
  await page.keyboard.type("ab");
  const scratch = await page.locator("#scratch").inputValue();
  if (scratch !== "ab") {
    throw new Error(`keyboard typing does not work here: ${JSON.stringify(scratch)}`);
  }
  await page.locator("#scratch").evaluate(input => input.remove());

  await page.locator("#token").click();
  await page.keyboard.type(token, { delay: 20 });
  if (await page.locator("#token").inputValue() !== token) {
    throw new Error(`connect form lost the typed token: ${JSON.stringify(await page.locator("#token").inputValue())}`);
  }
  await page.locator("#connect-form button").click();
  await page.waitForURL(/\?backend=/);
  const storedToken = await page.evaluate(() => localStorage.getItem("prigh.token"));
  if (storedToken !== token) {
    throw new Error(`connect form saved ${JSON.stringify(storedToken)} instead of the token`);
  }
  await page.locator("pre.screen").waitFor();
  await page.waitForFunction(() =>
    document.body.textContent.includes("deepseek-flash")
    && !document.body.textContent.includes("connecting…"));
  if (await page.locator("#keyboard-input").evaluate(input => document.activeElement === input) !== true) {
    throw new Error(`keyboard input is not focused; active=${await page.evaluate(() => document.activeElement?.id)}`);
  }
  await screen("ready");

  await page.keyboard.type("hello web", { delay: 20 });
  await page.waitForFunction(() => document.body.textContent.includes("> hello web"));
  await screen("typed");

  await page.keyboard.press("ArrowLeft");
  await page.keyboard.press("Backspace");
  await page.keyboard.type("X");
  await page.waitForFunction(() => document.body.textContent.includes("> hello wXb"));
  await screen("edited");

  await page.keyboard.press("Enter");
  await page.waitForFunction(() => document.body.textContent.includes("faux reply"));
  await screen("replied");

  await page.reload();
  await page.waitForFunction(() =>
    document.body.textContent.includes("deepseek-flash")
    && document.body.textContent.includes("faux reply")
    && !document.body.textContent.includes("connecting…"));
  if (await page.locator("#keyboard-input").evaluate(input => document.activeElement === input) !== true) {
    throw new Error(`keyboard input lost focus after reload; active=${await page.evaluate(() => document.activeElement?.id)}`);
  }
  await screen("reloaded");

  await terminal();
  await screen("after terminal");

  if (engineName === "chromium") await mobile();

  if (errors.length > 0) throw new Error(errors.join("\n"));
} catch (error) {
  console.error(`url=${page.url()}`);
  console.error(`active=${await page.evaluate(() => document.activeElement?.id).catch(() => "unavailable")}`);
  console.error(`body=${JSON.stringify(await page.locator("body").textContent().catch(() => "unavailable"))}`);
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
}
