// The account switcher against `prigh serve -tokens alice=a,bob=b
// -superusers alice`: signing in, adding a second account, switching between
// them (each sees its own sessions), acting as bob from alice and back, and
// signing one out. Run by prigh_web.sh after prigh_web.mjs; prints normalised
// snapshots (the expected output is in prigh_web.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: accounts.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site } = process.env;
const dir = join(process.env.TEST_DIR, "accounts");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
writeFileSync(
  join(dir, "script.json"),
  // Every user's agent starts the script afresh.
  JSON.stringify([{ text: "noted" }]),
);

const server = spawn(
  backend,
  ["serve", "-prigh-web", "127.0.0.1:0", "-prigh-web-root", site, "-faux-script", join(dir, "script.json"),
    "-tokens", "alice=a,bob=b", "-superusers", "alice", "-cwd", cwd],
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

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
const errors = [];
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const clean = text => text
  .replace(/127\.0\.0\.1:\d+/g, "127.0.0.1:<PORT>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: accounts: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-accounts-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const show = async (label, selector) => {
  section(label);
  console.log(clean(await page.locator(selector).first().innerText()));
  await screenshot(label);
};
const bodyHas = text => page.waitForFunction(t => document.body.innerText.includes(t), text);
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
const send = async text => {
  await page.locator(".composer textarea").fill(text);
  await page.locator(".btn.send").click();
  await idle();
};
const signIn = async (user, password) => {
  await page.locator("#signin-form").waitFor();
  await page.fill("#user", user);
  await page.fill("#password", password);
  await page.click("#signin-form button[type=submit]");
  await page.locator(".account-button").waitFor();
};
const sessions = async () => {
  section("sessions");
  await page.locator(".sidebar .session").first().waitFor();
  console.log((await page.locator(".sidebar .session-title").allInnerTexts()).join("\n"));
};
const menu = async () => {
  await page.locator(".account-button").click();
  await page.locator(".account-menu").waitFor();
};
const choose = async text => {
  await page.locator(".account-menu .menu-item", { hasText: text }).click();
};

try {
  // Alice signs in and talks.
  await page.goto(url);
  await signIn("alice", "a");
  await send("I am alice");
  await bodyHas("noted");
  await sessions();
  await menu();
  await show("alice's menu", ".account-menu");

  // Adding bob keeps alice.
  await choose("Add account");
  await show("add an account", ".signin");
  await signIn("bob", "b");
  await send("I am bob");
  await bodyHas("noted");
  await sessions();

  // One click back to alice, in her last session.
  await menu();
  await show("bob's menu", ".account-menu");
  await choose("alice");
  await page.locator(".account-button").waitFor();
  await bodyHas("I am alice");
  await sessions();

  // Acting as bob, and back.
  await menu();
  await choose("Act as");
  await page.locator(".picker-dialog").waitFor();
  await show("act as", ".picker-items");
  await page.locator(".picker-item", { hasText: "bob" }).click();
  await bodyHas("alice as bob");
  await sessions();
  await show("acting as bob", ".sidebar-footer");
  await menu();
  await choose("Back to alice");
  await bodyHas("Back to alice");
  await page.waitForFunction(() => !document.body.innerText.includes("acting as"));
  await sessions();

  // Signing bob out leaves alice one click away.
  await menu();
  await choose("bob");
  await page.locator(".account-button").waitFor();
  await bodyHas("I am bob");
  await menu();
  await choose("Sign out");
  await show("signed out", ".signin");
  await page.click("#account-0");
  await page.locator(".account-button").waitFor();
  await page.locator(".sidebar .session", { hasText: "I am alice" }).waitFor();
  await show("alice again", ".sidebar-footer");
  section("saved accounts");
  console.log(await page.evaluate(() =>
    JSON.parse(localStorage.getItem("prigh-web.accounts")).map(a => a.user).join(", ")));

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  server.kill();
}
