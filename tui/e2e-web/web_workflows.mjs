const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [url, token, engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!url || !token || !engine) {
  throw new Error("usage: web_workflows.mjs URL TOKEN chromium|firefox|webkit");
}

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox"] : [],
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
  .replace(/^[ \t]+|[ \t]+$/gm, "")
  .replace(/^\s*$/gm, "")
  .trim();

const screen = async label => {
  const lines = await page.locator("pre.screen .line").allTextContents();
  const text = clean(lines.filter(line => line.trim() !== "").join("\n"));
  console.log(`=== ${engineName}: ${label} ===`);
  console.log(text);
};

try {
  await page.goto(url);
  await page.locator("#connect-form").waitFor();
  console.log(`=== ${engineName}: authentication ===`);
  console.log(clean(await page.locator("#connect-form").textContent()));
  console.log(clean(`backend=${await page.locator("#backend").inputValue()}`));
  console.log(`token-in-url=${new URL(page.url()).searchParams.has("token")}`);

  await page.locator("#token").fill(token);
  if (await page.locator("#token").inputValue() !== token) {
    throw new Error("connect form did not retain the token input");
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
