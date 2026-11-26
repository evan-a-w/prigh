// Sharing a model login between users of `prigh serve -tokens
// alice=a,bob=b`: alice, on a phone, shares her DeepSeek key with bob from
// Settings → Models (Share…, a picker of the other users); bob's open
// Settings show it at once ("from alice"), without its key; alice revokes
// it at 320x640 and it is gone from bob's. At each phone step the audit
// checks nothing overflows or is under 44px. Prints normalised snapshots
// (ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { audit } from "./phone_audit.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: sharing.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site } = process.env;
const dir = join(process.env.TEST_DIR, "sharing");
const home = join(dir, "home");
normalVerbosity(home);
const cwd = join(dir, "cwd");
mkdirSync(cwd, { recursive: true });
const aliceConfig = join(home, ".prigh/namespaces/alice/.config/prigh");
mkdirSync(aliceConfig, { recursive: true });
const secret = "sk-alice-secret-key";
writeFileSync(join(aliceConfig, "auth.json"), JSON.stringify({ deepseek: { type: "api_key", key: secret } }));
// What ran on her key (today, UTC): hers and bob's, once she shares it.
const today = new Date().toISOString().slice(0, 10);
const ran = (requests, cost_usd) => ({ [today]: { requests, input: requests * 4000, output: requests * 300, cache_read: 0, cost_usd } });
mkdirSync(join(home, ".prigh/namespaces/alice/.prigh"), { recursive: true });
writeFileSync(
  join(home, ".prigh/namespaces/alice/.prigh/account-usage.json"),
  JSON.stringify({ "deepseek/main": { alice: ran(3, 0.42), bob: ran(2, 0.25) } }),
);
writeFileSync(join(dir, "script.json"), JSON.stringify([{ text: "noted" }]));

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-tokens", "alice=a,bob=b", "-cwd", cwd],
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
const errors = [];
const section = label => console.log(`=== ${engineName}: sharing: ${label} ===`);
const lines = text => text.split("\n").map(l => l.trim()).filter(Boolean).join("\n");

const open = async (user, password, w, h, phone) => {
  const options = { viewport: { width: w, height: h } };
  if (phone) {
    options.hasTouch = true;
    if (engineName !== "firefox") options.isMobile = true;
  }
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  const page = await context.newPage();
  page.on("console", m => { if (m.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(m.text())) errors.push(`console: ${m.text()}`); });
  page.on("pageerror", e => errors.push(`page: ${e.message}`));
  // Whatever reaches the page: the key must never.
  page.on("websocket", ws => ws.on("framereceived", f => {
    if (String(f.payload).includes(secret)) errors.push(`${user} received the key`);
  }));
  await page.goto(url);
  await page.locator("#signin-form").waitFor();
  await page.fill("#user", user);
  await page.fill("#password", password);
  await page.click("#signin-form button[type=submit]");
  await page.locator(".composer textarea").waitFor();
  await page.addStyleTag({ content: ".toasts, .toast { display: none !important; } *, *::before, *::after { animation: none !important; transition: none !important; }" });
  return page;
};
let shot = 0;
const step = async (page, label) => {
  await page.waitForTimeout(150);
  section(label);
  console.log(await audit(page));
  if (process.env.SHOTS) {
    shot++;
    await page.screenshot({ path: join(process.env.SHOTS, `${engineName}-sharing-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`) });
  }
};
// Settings → Models, by the phone's ⋯ sheet, or /settings on a desktop.
const models = async (page, phone) => {
  if (phone) {
    await page.getByRole("button", { name: "Panels and settings" }).tap();
    await page.locator(".chrome-menu .settings-row").tap();
    await page.locator(".settings-home-row[data-settings-tab=models]").tap();
  } else {
    await page.locator(".composer textarea").fill("/settings models");
    await page.locator(".composer textarea").press("Enter");
  }
  await page.locator(".provider-login").first().waitFor();
};
const logins = async page => lines((await page.locator(".provider-login").allInnerTexts()).join("\n"));

try {
  const bob = await open("bob", "b", 1280, 800, false);
  await bob.locator(".composer textarea").fill("/settings models");
  await bob.locator(".composer textarea").press("Enter");
  await bob.locator(".settings-panel").waitFor();
  section("bob's logins before");
  console.log(await bob.locator(".provider-login").count());

  const alice = await open("alice", "a", 390, 844, true);
  await models(alice, true);
  await alice.locator(".provider-login").first().scrollIntoViewIfNeeded();
  await step(alice, "390x844: alice's logins");
  console.log(await logins(alice));
  await alice.locator(".provider-login button", { hasText: "Share…" }).tap();
  await alice.locator(".picker-items").waitFor();
  await step(alice, "390x844: share with");
  console.log(lines(await alice.locator(".picker-items").innerText()));
  await alice.locator(".picker-items [role=option]", { hasText: "bob" }).first().tap();

  // Bob's settings follow, live.
  await bob.waitForFunction(() => document.body.innerText.includes("from alice"));
  section("bob's logins after");
  console.log(await logins(bob));
  await alice.close();

  const small = await open("alice", "a", 320, 640, true);
  await models(small, true);
  await small.locator(".provider-login").first().scrollIntoViewIfNeeded();
  await step(small, "320x640: alice's logins");
  console.log(await logins(small));
  await small.locator(".provider-login button", { hasText: "Share…" }).tap();
  await small.locator(".picker-items").waitFor();
  await step(small, "320x640: share with");
  console.log(lines(await small.locator(".picker-items").innerText()));
  await small.locator(".picker-items [role=option]", { hasText: "bob" }).first().tap();
  await bob.waitForFunction(() => !document.body.innerText.includes("from alice"));
  section("bob's logins once revoked");
  console.log(await bob.locator(".provider-login").count());
  if (errors.length) console.log(errors.join("\n"));
} finally {
  await end(() => server.kill(), () => browser.close());
}
