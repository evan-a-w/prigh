// Signing in to a remote MCP server with OAuth, from Settings → MCP servers:
// a fake server (here, in-process) that answers 401 with the MCP
// authorization spec's challenge, its protected resource and authorization
// server metadata, dynamic client registration, an authorization endpoint
// that approves at once and a token endpoint. Sign in gives the sign-in
// page's link; following it in another tab comes back to the backend's
// /mcp/oauth/callback, and the section turns to "Signed in" by itself; Sign
// out goes back. Then the signed-in state on phones (390x844, 320x640;
// touch, and Chromium as an installed PWA with the notch's insets). Run by
// prigh_web.sh; prints normalised snapshots and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { createServer } from "node:http";
import { mkdirSync, readFileSync, statSync, writeFileSync } from "node:fs";
import { hostName } from "./host_name.mjs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: mcp_oauth.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "mcp_oauth");
const home = join(dir, "home");
normalVerbosity(home);
const cwd = join(dir, "cwd");
mkdirSync(join(home, ".prigh"), { recursive: true });
mkdirSync(cwd, { recursive: true });

// The fake server: MCP at /mcp and its own authorization server.
const log = [];
const accessToken = "tok-1";
let base = "";
const fake = createServer((req, res) => {
  let body = "";
  req.on("data", d => (body += d));
  req.on("end", () => {
    const u = new URL(req.url, base);
    const json = (status, value, headers = {}) => {
      res.writeHead(status, { "Content-Type": "application/json", ...headers });
      res.end(JSON.stringify(value));
    };
    if (u.pathname === "/mcp" && req.method === "POST") {
      if (req.headers.authorization !== `Bearer ${accessToken}`) {
        if (!log.includes("401")) log.push("401");
        return json(401, { error: "unauthorized" }, {
          "WWW-Authenticate": `Bearer resource_metadata="${base}/.well-known/oauth-protected-resource/mcp"`,
        });
      }
      const { id, method, params } = JSON.parse(body);
      if (id === undefined) {
        res.writeHead(202);
        return res.end();
      }
      const reply = result => json(200, { jsonrpc: "2.0", id, result });
      if (method === "initialize")
        return reply({ protocolVersion: params?.protocolVersion ?? "2025-06-18", capabilities: { tools: {} }, serverInfo: { name: "designs", version: "1" } });
      if (method === "tools/list")
        return reply({ tools: [{ name: "get_design", description: "A design's frames", inputSchema: { type: "object" } }] });
      return json(200, { jsonrpc: "2.0", id, error: { code: -32601, message: `no method ${method}` } });
    }
    if (u.pathname.startsWith("/.well-known/oauth-protected-resource")) {
      log.push("protected resource metadata");
      return json(200, { resource: `${base}/mcp`, authorization_servers: [base] });
    }
    if (u.pathname === "/.well-known/oauth-authorization-server") {
      log.push("authorization server metadata");
      return json(200, {
        issuer: base,
        authorization_endpoint: `${base}/authorize`,
        token_endpoint: `${base}/token`,
        registration_endpoint: `${base}/register`,
        response_types_supported: ["code"],
        code_challenge_methods_supported: ["S256"],
      });
    }
    if (u.pathname === "/register") {
      const { redirect_uris } = JSON.parse(body);
      log.push(`register (redirect ${redirect_uris.map(r => r.replace(/^https?:\/\/[^/]+/, "PRIGH")).join(" ")})`);
      return json(201, { client_id: "client-1", redirect_uris });
    }
    if (u.pathname === "/authorize") {
      const p = u.searchParams;
      log.push(`authorize (client ${p.get("client_id")}, ${p.get("code_challenge_method")}, resource ${p.get("resource") === `${base}/mcp`}, state ${p.has("state")})`);
      res.writeHead(302, { Location: `${p.get("redirect_uri")}?code=code-1&state=${encodeURIComponent(p.get("state"))}` });
      return res.end();
    }
    if (u.pathname === "/token") {
      const p = new URLSearchParams(body);
      log.push(`token (${p.get("grant_type")}, verifier ${p.has("code_verifier")})`);
      return json(200, { access_token: accessToken, token_type: "Bearer", expires_in: 3600, refresh_token: "refresh-1" });
    }
    res.writeHead(404);
    res.end();
  });
});
await new Promise(resolve => fake.listen(0, "127.0.0.1", resolve));
base = `http://127.0.0.1:${fake.address().port}`;
writeFileSync(join(home, ".prigh/mcp-servers.json"), JSON.stringify({ mcpServers: { designs: { type: "http", url: `${base}/mcp` } } }));

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

const section = label => console.log(`=== ${engineName}: mcp oauth: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-mcp-oauth-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
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
const tidy = text =>
  text
    .replace(/\n+/g, "\n")
    .replaceAll(base, "FAKE")
    .replaceAll(home, "HOME")
    .replaceAll(hostName(), "HOST")
    .replaceAll(hostName().toUpperCase(), "HOST");
const mine = p => p.locator(`.mcp-server:not(.mcp-host-server)[data-server="designs"]`);
const openMcp = async p => {
  await run(p, "/settings mcp");
  await p.locator(".settings-dialog").waitFor();
  await p.locator(".mcp-actions").waitFor();
};
const show = async p => console.log(tidy(await mine(p).innerText()));
const fits = p =>
  p.evaluate(() => {
    const bad = [];
    const w = document.documentElement.clientWidth;
    for (const el of document.querySelectorAll(".settings-panel :is(button, a.btn, .setting-note)")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      if (r.left < 0 || r.right > w + 0.5) bad.push(`${el.className || el.tagName} ${Math.round(r.left)}-${Math.round(r.right)}`);
    }
    return bad.length ? bad.join(", ") : "yes";
  });
const targets = p =>
  p.evaluate(() => {
    const bad = [];
    for (const el of document.querySelectorAll(".settings-panel :is(.mcp-sign-in, .mcp-sign-in-open, .mcp-sign-out)")) {
      const r = el.getBoundingClientRect();
      if (r.width === 0) continue;
      if (r.height < 44) bad.push(`${el.innerText.trim()} ${Math.round(r.height)}px high`);
    }
    return bad.length ? bad.join(", ") : "all ok";
  });
const storedToken = () => {
  try {
    const file = join(home, ".config/prigh/auth.json");
    const entries = Object.keys(JSON.parse(readFileSync(file, "utf8"))).map(k => k.replaceAll(base, "FAKE"));
    return `${entries.join(", ") || "nothing"} (mode ${(statSync(file).mode & 0o777).toString(8)})`;
  } catch (e) {
    return `(none: ${e.code ?? e.message})`;
  }
};

const page = await open({ width: 1280, height: 800 });
try {
  await signIn(page);
  await openMcp(page);
  await mine(page).locator(".mcp-sign-in").waitFor();
  section("needs sign-in");
  await show(page);
  await screenshot(page, "needs-sign-in");
  await mine(page).locator(".mcp-sign-in").click();
  const link = mine(page).locator("a.mcp-sign-in-open");
  await link.waitFor();
  section("the sign-in page's link");
  console.log(`target ${await link.getAttribute("target")}, rel ${await link.getAttribute("rel")}`);
  await screenshot(page, "sign-in-link");
  const tab = await page.context().newPage();
  await tab.goto(await link.getAttribute("href"));
  section("the callback page");
  console.log(tidy(await tab.locator("body").innerText()));
  await mine(page).getByText("Signed in").waitFor();
  section("signed in, without a reload");
  await show(page);
  console.log(`server saw: ${[...new Set(log)].join("; ")}`);
  console.log(`stored: ${storedToken()}`);
  await screenshot(page, "signed-in");
  await tab.close();

  for (const [w, h, safe] of [[390, 844, false], [320, 640, false], ...(engineName === "chromium" ? [[390, 844, true]] : [])]) {
    const tag = `${w}x${h}${safe ? "-pwa" : ""}`;
    const phone = await open({ width: w, height: h }, { phone: true, safe });
    await signIn(phone);
    await phone.addStyleTag({ content: "*, *::before, *::after { animation: none !important; transition: none !important; }" });
    await openMcp(phone);
    await mine(phone).locator(".mcp-sign-out").waitFor();
    section(`phone ${tag}`);
    console.log(`fits: ${await fits(phone)}; targets: ${await targets(phone)}`);
    await screenshot(phone, `phone-${tag}-signed-in`);
    await phone.context().close();
  }

  await mine(page).locator(".mcp-sign-out").click();
  await mine(page).locator(".mcp-sign-in").waitFor();
  section("signed out");
  await show(page);
  console.log(`stored: ${storedToken()}`);

  section("errors");
  console.log(errors.length ? errors.join("\n") : "(none)");
} catch (e) {
  console.log(`FAILED: ${e.stack ?? e}`);
  await screenshot(page, "failed");
  process.exitCode = 1;
} finally {
  await end(() => browser.close(), () => { server.kill(); fake.close(); });
}
