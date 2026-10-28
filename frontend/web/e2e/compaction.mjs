// The settings page's Compaction section: the threshold typed and refused,
// the switch, the summary model's picker; saved in the account's
// config.json, shown after a reload and by /compaction; and the section on
// a phone. Run by prigh_web.sh; prints normalised snapshots (the expected
// output is in ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: compaction.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "compaction");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", cwd],
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
const open = async viewport => {
  const context = await browser.newContext({ viewport, reducedMotion: "reduce" });
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  return page;
};

const section = label => console.log(`=== ${engineName}: compaction: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-compaction-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const composer = p => p.locator(".composer textarea");
const signIn = async p => {
  await p.goto(url);
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await composer(p).waitFor();
};
const compaction = p => p.locator(".settings-section", { has: p.locator(".setting.compact-at") });
// The settings page, scrolled to the section, once the backend's config is in.
const openSettings = async p => {
  await p.keyboard.press("Control+,");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".compact-at-input:not([disabled])").waitFor();
  await compaction(p).scrollIntoViewIfNeeded();
};
const show = async p => {
  console.log((await compaction(p).innerText()).replace(/\n+/g, "\n"));
  const input = p.locator(".compact-at-input");
  console.log(`field: ${JSON.stringify(await input.inputValue())} (placeholder ${await input.getAttribute("placeholder")}); \
Off pressed: ${await p.locator(".setting.compact-at button.segment").getAttribute("aria-pressed")}; \
mask: ${await p.locator(".setting.mask .switch").getAttribute("aria-checked")}`);
};
const configFile = () => {
  const c = JSON.parse(readFileSync(join(home, ".prigh/config.json"), "utf8"));
  return ["compact_at_tokens", "compaction_model", "mask_observations"]
    .map(k => `${k}=${JSON.stringify(c[k])}`)
    .join(" ");
};
// set_config's reply, which comes once config.json is written.
const saved = (p, text) => p.locator(".toast", { hasText: text }).last().waitFor();

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  await openSettings(page);
  section("default");
  await show(page);
  await screenshot(page, "default");

  await page.locator(".compact-at-input").fill("lots");
  await page.locator(".compact-at-input").press("Enter");
  await page.locator(".setting.compact-at .setting-note").waitFor();
  section("refused");
  console.log(await page.locator(".setting.compact-at .setting-note").innerText());
  console.log(`still open: ${await page.locator(".settings-dialog").count()}`);
  await screenshot(page, "refused");

  await page.locator(".compact-at-input").fill("150k");
  await page.locator(".compact-at-input").press("Enter");
  await saved(page, "at 150k tokens");
  await page.locator(".setting.mask .switch").click();
  await page.locator(".setting.mask .switch[aria-checked=false]").waitFor();
  await saved(page, "observation masking off");
  section("set");
  await show(page);
  console.log(configFile());
  await screenshot(page, "set");

  // The summary model: the model picker, then back on the page.
  await page.locator(".summary-model-choice").click();
  await page.locator(".modal input").waitFor();
  section("picker");
  console.log(`${await page.locator(".modal-head h2").innerText()}; first: \
${(await page.locator(".picker-item").first().innerText()).replace(/\s+/g, " ")}`);
  await screenshot(page, "picker");
  // Filtered as typed, Enter takes the first.
  await page.locator(".modal input").fill("haiku");
  await page.locator(".picker-item", { hasText: "conversation" }).waitFor({ state: "detached" });
  await page.locator(".modal input").press("Enter");
  await page.locator(".settings-dialog").waitFor();
  await page.locator(".summary-model-choice", { hasText: "Haiku" }).waitFor();
  await saved(page, "summaries by anthropic/");
  console.log(`chosen: a Haiku; ${configFile().replace(/"anthropic\/[^"]+"/, "anthropic/(a haiku)")}`);

  // Back to the conversation's model, from the same picker.
  await page.locator(".summary-model-choice").click();
  await page.locator(".picker-item").first().click();
  await page.locator(".summary-model-choice", { hasText: "conversation" }).waitFor();

  section("reloaded");
  await page.reload();
  await composer(page).waitFor();
  await openSettings(page);
  await show(page);
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal-backdrop", { state: "detached" });
  await composer(page).fill("/compaction");
  await composer(page).press("Control+Enter");
  section("/compaction");
  console.log(await page.locator(".toast", { hasText: "Compaction:" }).last().innerText());

  // Changed by /compaction while the page is open: the page follows.
  await composer(page).fill("/compaction at off");
  await composer(page).press("Enter");
  await saved(page, "80% of the model's window;");
  await openSettings(page);
  await page.locator(".setting.compact-at .segment[aria-pressed=true]").waitFor();
  section("off by /compaction");
  await show(page);
  console.log(configFile());

  const phone = await open({ width: 390, height: 844 });
  await signIn(phone);
  await openSettings(phone);
  section("phone");
  console.log(`fits: ${await phone.evaluate(() => {
    const s = document.querySelector(".settings-section:has(.compact-at)");
    return s.scrollWidth <= s.clientWidth;
  })}`);
  await screenshot(phone, "phone");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
