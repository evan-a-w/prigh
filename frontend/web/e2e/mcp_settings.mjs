// The settings page's MCP servers section: servers kept in the account's
// settings (~/.prigh/mcp-servers.json), added with the form (env values
// masked until shown, and reaching the server: fake_mcp.mjs starts only
// with the right TOKEN) or as pasted mcpServers JSON, switched off and on,
// removed (asked first), one shadowed by the host's ~/.prigh/mcp.json,
// /mcp disable from the composer, a change on another page followed; and
// the section on phones (390x844, 320x640; touch, and Chromium as an
// installed PWA with the notch's insets). Run by prigh_web.sh; prints
// normalised snapshots (the expected output is in ENGINE.expected) and,
// with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { hostname } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: mcp_settings.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "mcp_settings");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".prigh"), { recursive: true });
mkdirSync(cwd, { recursive: true });
const node = process.execPath;
const fake = join(dirname(fileURLToPath(import.meta.url)), "fake_mcp.mjs");
// The host's own file: "local", which a server of the same name in the
// settings does not replace.
writeFileSync(join(home, ".prigh/mcp.json"), JSON.stringify({ mcpServers: { local: { command: node, args: [fake, "local"] } } }));
const settingsFile = join(home, ".prigh/mcp-servers.json");

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", cwd],
  {
    env: { ...process.env, HOME: home, TMUX_TMPDIR: dir, SHELL: "/bin/sh", FAKE_MCP_TOKEN: "s3cret" },
    stdio: ["ignore", "ignore", "pipe"],
  },
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

const section = label => console.log(`=== ${engineName}: mcp settings: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-mcp-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
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
const tidy = text =>
  text
    .replace(/\n+/g, "\n")
    .replaceAll(fake, "FAKE")
    .replaceAll(node, "NODE")
    .replaceAll(home, "HOME")
    .replaceAll(cwd, "CWD")
    .replaceAll(hostname(), "HOST")
    .replaceAll(hostname().toUpperCase(), "HOST");
const row = (p, name) => p.locator(`.mcp-server[data-server="${name}"]`).first();
const mine = (p, name) => p.locator(`.mcp-server:not(.mcp-host-server)[data-server="${name}"]`);
const openMcp = async p => {
  await run(p, "/settings mcp");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".mcp-actions").waitFor();
  await p.locator(".mcp-host-server, .mcp-host-empty").first().waitFor();
};
const show = async p => console.log(tidy(await panel(p).innerText()));
const settingsJson = () => {
  try {
    const { mcpServers = {}, disabled = [] } = JSON.parse(readFileSync(settingsFile, "utf8"));
    const mode = (statSync(settingsFile).mode & 0o777).toString(8);
    return `file (mode ${mode}): ${Object.entries(mcpServers)
      .map(([name, s]) => `${name}${disabled.includes(name) ? " (off)" : ""}: ${tidy(JSON.stringify(s))}`)
      .join("; ")}`;
  } catch (e) {
    return `(unreadable: ${e.message})`;
  }
};
// The page's elements that overflow it sideways.
const fits = p =>
  p.evaluate(() => {
    const bad = [];
    const w = document.documentElement.clientWidth;
    for (const el of document.querySelectorAll(".settings-panel :is(button, input, textarea, .setting-note)")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      if (r.left < 0 || r.right > w + 0.5) bad.push(`${el.className || el.tagName} ${Math.round(r.left)}-${Math.round(r.right)}`);
    }
    const s = document.querySelector(".settings-panel");
    if (s.scrollWidth > s.clientWidth) bad.push(`panel ${s.scrollWidth} > ${s.clientWidth}`);
    return bad.length ? bad.join(", ") : "yes";
  });
// Targets smaller than a thumb's 44px, and inputs smaller than 16px.
const targets = p =>
  p.evaluate(() => {
    const bad = [];
    for (const el of document.querySelectorAll(".settings-panel :is(button, input, textarea)")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      const after = el.matches(".switch") ? parseFloat(getComputedStyle(el, "::after").top) : 0;
      const label = el.getAttribute("aria-label") || el.innerText.trim() || el.id;
      if (!el.matches(".mcp-tools-toggle") && r.height - 2 * Math.min(after, 0) < 40) bad.push(`${label} ${Math.round(r.height)}px high`);
      if (el.matches("input, textarea") && parseFloat(getComputedStyle(el).fontSize) < 16) bad.push(`${label} font ${getComputedStyle(el).fontSize}`);
    }
    return bad.length ? bad.join(", ") : "all ok";
  });

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  await run(page, "hello");
  await page.getByText("faux reply").first().waitFor();
  await openMcp(page);
  await row(page, "local").getByText("Ready").waitFor();
  section("empty, the host's own");
  await show(page);
  await screenshot(page, "empty");

  // The form: a command with its arguments one per line, and a secret.
  await page.locator(".mcp-add").click();
  await page.locator("#mcp-name").fill("echo");
  await page.locator("#mcp-command").fill(node);
  await page.locator("#mcp-args").fill(`${fake}\necho`);
  await page.locator(".mcp-pair-add").click();
  await page.locator(".mcp-pair-key").fill("TOKEN");
  await page.locator(".mcp-pair-value").fill("s3cret");
  // A new value shows while it is typed; Hide masks it, Show shows it again.
  const valueType = async () => page.locator(".mcp-pair-value").getAttribute("type");
  const toggle = async (label, type) => {
    await page.locator(".mcp-pair-show", { hasText: label }).click();
    await page.locator(`.mcp-pair-value[type=${type}]`).waitFor();
  };
  section("adding");
  console.log(`value while typing: ${await valueType()}`);
  await toggle("Hide", "password");
  console.log(`hidden: ${await valueType()}`);
  await toggle("Show", "text");
  console.log(`shown again: ${await valueType()}`);
  await screenshot(page, "adding");
  await page.locator("#mcp-args").press("Control+Enter");
  await page.locator(".mcp-draft").waitFor({ state: "detached" });
  await mine(page, "echo").getByText("Ready").waitFor();
  section("added: started with the env");
  console.log(tidy(await mine(page, "echo").innerText()));
  console.log(settingsJson());

  // Editing shows the stored value masked.
  await mine(page, "echo").locator(".mcp-edit").click();
  await page.locator(".mcp-pair-value").waitFor();
  console.log(`editing, the value: ${await page.locator(".mcp-pair-value").getAttribute("type")}`);
  await page.keyboard.press("Escape");
  await page.locator(".mcp-draft").waitFor({ state: "detached" });
  console.log(`settings still open: ${await page.locator(".settings-dialog").count()}`);

  // A wrong secret: the server fails, with what it said.
  await mine(page, "echo").locator(".mcp-edit").click();
  await page.locator(".mcp-pair-value").fill("wrong");
  await page.locator(".mcp-save").click();
  await page.locator(".mcp-draft").waitFor({ state: "detached" });
  await mine(page, "echo").locator(".mcp-health.warm").waitFor();
  section("a wrong secret");
  console.log(tidy(await mine(page, "echo").innerText()));
  await mine(page, "echo").locator(".mcp-edit").click();
  await page.locator(".mcp-pair-value").fill("s3cret");
  await page.locator(".mcp-save").click();
  await mine(page, "echo").getByText("Ready").waitFor();

  // Pasted JSON: all or nothing; "local" is the host file's name too.
  await page.locator(".mcp-paste-open").click();
  await page.locator("#mcp-paste-text").fill(JSON.stringify({ mcpServers: { pasted: { command: node, args: [fake, "pasted"] }, broken: {} } }));
  await page.locator("#mcp-paste-text").press("Control+Enter");
  await page.locator(".mcp-paste .mcp-error, .mcp-paste .setting-note.warm").first().waitFor();
  section("pasted, one broken");
  console.log(tidy(await page.locator(".mcp-paste").innerText()));
  await page.locator("#mcp-paste-text").fill(
    JSON.stringify({ mcpServers: { pasted: { command: node, args: [fake, "pasted"] }, local: { url: "https://example.com/mcp", headers: { Authorization: "Bearer t" } } } }),
  );
  await page.locator(".mcp-paste-save").click();
  await page.locator(".mcp-paste").waitFor({ state: "detached" });
  await mine(page, "pasted").getByText("Ready").waitFor();
  await mine(page, "local").getByText("Overridden").waitFor();
  section("pasted");
  await show(page);
  console.log(settingsJson());
  await screenshot(page, "pasted");

  // Off and on, followed on another page.
  const other = await open({ width: 1280, height: 800 });
  await signIn(other);
  await openMcp(other);
  await mine(page, "pasted").locator(".switch").click();
  await mine(page, "pasted").getByText("Off").waitFor();
  await mine(other, "pasted").getByText("Off").waitFor();
  section("off, followed on another page");
  console.log(tidy(await mine(other, "pasted").innerText()));
  console.log(settingsJson());
  await other.close();
  await mine(page, "pasted").locator(".switch").click();
  await mine(page, "pasted").getByText("Ready").waitFor();

  // Remove asks first; Keep keeps it.
  await mine(page, "local").locator(".mcp-remove").click();
  await page.locator("#mcp-remove-yes").waitFor();
  section("removing");
  console.log(tidy(await mine(page, "local").innerText()));
  await screenshot(page, "removing");
  await mine(page, "local").getByRole("button", { name: "Keep" }).click();
  await mine(page, "local").locator(".mcp-remove").click();
  await page.locator("#mcp-remove-yes").click();
  await mine(page, "local").waitFor({ state: "detached" });
  console.log(settingsJson());

  // From the composer.
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal-backdrop", { state: "detached" });
  await run(page, "/mcp disable pasted");
  await page.locator(".toast", { hasText: "pasted" }).waitFor();
  section("/mcp disable");
  console.log((await page.locator(".toast", { hasText: "pasted" }).innerText()).replace(/\n+/g, " "));
  console.log(settingsJson());
  await run(page, "/mcp add echo https://example.com/mcp");
  await page.locator(".toast", { hasText: "echo" }).waitFor();
  console.log((await page.locator(".toast", { hasText: "echo" }).innerText()).replace(/\n+/g, " "));
  console.log(`composer: ${await composer(page).inputValue()}`);
  await composer(page).fill("");

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone);
    await phone.addStyleTag({ content: "*, *::before, *::after { animation: none !important; transition: none !important; }" });
    await openMcp(phone);
    await mine(phone, "echo").getByText("Ready").waitFor();
    section(`phone ${tag}`);
    console.log(`list fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-list`);
    await mine(phone, "echo").locator(".mcp-edit").tap();
    await phone.locator("#mcp-name").waitFor();
    console.log(`editing fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-editing`);
    await phone.locator(".mcp-draft button", { hasText: "Cancel" }).tap();
    await phone.locator(".mcp-paste-open").tap();
    await phone.locator("#mcp-paste-text").waitFor();
    console.log(`pasting fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await phone.locator(".mcp-paste button", { hasText: "Cancel" }).tap();
    await mine(phone, "echo").locator(".mcp-remove").tap();
    await phone.locator("#mcp-remove-yes").waitFor();
    console.log(`removing fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-removing`);
    await mine(phone, "echo").getByRole("button", { name: "Keep" }).tap();
    await phone.locator(".modal-back").tap();
    await phone.locator(".settings-home-row.settings-tab-mcp").waitFor();
    console.log(`back: ${(await phone.locator(".settings-tab-mcp").innerText()).replace(/\n+/g, " | ")}`);
    await phone.context().close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => server.kill());
}
