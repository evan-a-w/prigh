// The settings page's Usage section: Claude's and ChatGPT's subscription
// limits from a fake of both providers' usage endpoints (PRIGH_USAGE_ORIGIN),
// read as the section opens and on Refresh, then a provider that fails; and
// the section on phones (390x844, 320x640; touch, and Chromium as an
// installed PWA with the notch's insets). Run by prigh_web.sh; prints
// normalised snapshots (the expected output is in ENGINE.expected) and,
// with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { createServer } from "node:http";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: usage.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "usage");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".config/prigh"), { recursive: true });
mkdirSync(cwd, { recursive: true });
const far = Date.now() + 365 * 24 * 3600 * 1000;
writeFileSync(
  join(home, ".config/prigh/auth.json"),
  JSON.stringify({
    anthropic: { type: "oauth", access: "sk-ant-oat01-e2e", refresh: "r1", expires: far },
    "openai-codex": { type: "oauth", access: "codex-e2e", refresh: "r2", expires: far, accountId: "acc-e2e" },
  }),
);

// Both endpoints, as recorded from the providers; times from when asked.
const asked = { claude: 0, chatgpt: 0 };
const count = () => `Claude ${asked.claude}, ChatGPT ${asked.chatgpt}`;
let claudeFails = false;
const fake = createServer((req, res) => {
  const now = Date.now();
  const iso = ms => new Date(now + ms).toISOString();
  const json = (status, body) => {
    res.writeHead(status, { "content-type": "application/json" });
    res.end(JSON.stringify(body));
  };
  const min = 60 * 1000;
  if (req.url === "/api/oauth/usage") {
    asked.claude++;
    if (req.headers["anthropic-beta"] !== "oauth-2025-04-20" || req.headers.authorization !== "Bearer sk-ant-oat01-e2e")
      return json(400, { error: { message: "bad headers" } });
    if (claudeFails) return json(429, { error: { type: "rate_limit_error", message: "Rate limited" } });
    return json(200, {
      five_hour: { utilization: 33.0, resets_at: iso(134 * min + 30000) },
      seven_day: { utilization: 91.0, resets_at: iso((3 * 24 * 60 + 4 * 60) * min + 30000) },
      seven_day_opus: { utilization: 4.0, resets_at: null },
      seven_day_oauth_apps: null,
    });
  }
  if (req.url === "/backend-api/wham/usage") {
    asked.chatgpt++;
    if (req.headers["chatgpt-account-id"] !== "acc-e2e") return json(400, { detail: "bad headers" });
    return json(200, {
      plan_type: "plus",
      rate_limit: {
        allowed: true,
        limit_reached: false,
        primary_window: { used_percent: asked.chatgpt === 1 ? 72 : 75, limit_window_seconds: 18000, reset_after_seconds: 2550, reset_at: Math.floor(now / 1000) + 2550 },
        secondary_window: { used_percent: 12, limit_window_seconds: 604800, reset_after_seconds: 518430, reset_at: Math.floor(now / 1000) + 518430 },
      },
      credits: null,
    });
  }
  json(404, {});
});
await new Promise(resolve => fake.listen(0, "127.0.0.1", resolve));
const origin = `http://127.0.0.1:${fake.address().port}`;

const env = { ...process.env, HOME: home, XDG_CONFIG_HOME: join(home, ".config"), TMUX_TMPDIR: dir, SHELL: "/bin/sh", PRIGH_USAGE_ORIGIN: origin };
for (const v of ["ANTHROPIC_API_KEY", "ANTHROPIC_OAUTH_TOKEN"]) delete env[v];
const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", cwd],
  { env, stdio: ["ignore", "ignore", "pipe"] },
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
const open = async (viewport, { phone = false, safe = false } = {}) => {
  const options = { viewport, reducedMotion: "reduce" };
  if (phone) {
    options.hasTouch = true;
    if (engineName !== "firefox") options.isMobile = true;
  }
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  const page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  if (safe) {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }
  return page;
};

const section = label => console.log(`=== ${engineName}: usage: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-usage-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const composer = p => p.locator(".composer textarea");
const signIn = async p => {
  await p.goto(url, { timeout: 60000 });
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await composer(p).waitFor();
};
const run = async (p, command) => {
  await composer(p).fill(command);
  await composer(p).press("Control+Enter");
};
const panel = p => p.locator(".settings-panel");
const loaded = p => p.locator(".usage-refresh button:not([disabled])", { hasText: "Refresh" }).waitFor();
const openUsage = async p => {
  await run(p, "/usage");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".usage-window").first().waitFor();
  await loaded(p);
};
// The clock and the date depend on when and where this runs.
const normal = text =>
  text
    .replace(/\n+/g, "\n")
    .replace(/(\b\d{1,2} [A-Z][a-z]{2}( \d{4})? )?\b\d{1,2}:\d{2}\b/g, "TIME");
const show = async p => {
  console.log(normal(await panel(p).innerText()));
  const bars = await p.locator(".usage-track").evaluateAll(els =>
    els.map(el => `${el.getAttribute("aria-label")}=${el.getAttribute("aria-valuenow")}`),
  );
  console.log(`bars: ${bars.join(", ")}`);
};
const fits = p =>
  p.evaluate(() => {
    const bad = [];
    const w = document.documentElement.clientWidth;
    for (const el of document.querySelectorAll(".settings-panel button, .settings-panel .usage-meter")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      if (r.left < 0 || r.right > w + 0.5) bad.push(`${el.className || el.tagName} ${Math.round(r.left)}-${Math.round(r.right)}`);
    }
    const s = document.querySelector(".settings-panel");
    if (s.scrollWidth > s.clientWidth) bad.push(`panel ${s.scrollWidth} > ${s.clientWidth}`);
    return bad.length ? bad.join(", ") : "yes";
  });
const targets = p =>
  p.evaluate(() => {
    const bad = [];
    for (const el of document.querySelectorAll(".settings-panel button")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      if (r.height < 40) bad.push(`${el.innerText.trim()} ${Math.round(r.height)}px high`);
    }
    return bad.length ? bad.join(", ") : "all ok";
  });

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  await openUsage(page);
  section("opened");
  await show(page);
  console.log(`asked: ${count()}`);
  await screenshot(page, "opened");

  await page.locator(".usage-refresh button").click();
  await page.locator(".usage-track[aria-label='5-hour session used'][aria-valuenow='75']").waitFor();
  await loaded(page);
  section("refreshed");
  console.log(`asked: ${count()}; ChatGPT's session: ${await page.locator(".usage-percent").nth(3).innerText()}`);

  claudeFails = true;
  await page.locator(".usage-refresh button").click();
  await page.locator(".usage-failed").waitFor();
  await loaded(page);
  section("Claude rate limited");
  await show(page);
  await screenshot(page, "failed");
  claudeFails = false;

  // Away and back: asked again as the section opens, and only then.
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal-backdrop", { state: "detached" });
  const before = asked.claude + asked.chatgpt;
  await openUsage(page);
  await page.locator(".usage-failed").waitFor({ state: "detached" });
  section("reopened");
  console.log(`asked again: ${asked.claude + asked.chatgpt - before}`);

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone);
    await phone.addStyleTag({ content: "*, *::before, *::after { animation: none !important; transition: none !important; }" });
    await openUsage(phone);
    section(`phone ${tag}`);
    console.log(`fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}`);
    await phone.locator(".usage-window").last().scrollIntoViewIfNeeded();
    await screenshot(phone, `phone-${tag}-end`);
    await phone.locator(".modal-back").tap();
    await phone.locator(".settings-home-row.settings-tab-usage").waitFor();
    console.log(`back: ${(await phone.locator(".settings-tab-usage").innerText()).replace(/\n+/g, " | ")}`);
    await screenshot(phone, `phone-${tag}-home`);
    await phone.context().close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill(), () => new Promise(resolve => fake.close(resolve)));
}
