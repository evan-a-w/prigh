// Signing a program in through the browser when a sign-in proxy (here a
// fake of authentik's forward auth) guards prigh-web: `prigh browser-signin`
// opens the site at its one-time link; the browser signs in to the proxy,
// then to prigh, and allows it; the program gets the proxy's cookie and
// saves it. On a desktop and on a phone. Run by prigh_web.sh; prints
// normalised snapshots (the expected output is in ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync } from "node:fs";
import http from "node:http";
import net from "node:net";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: cli_signin.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site } = process.env;
const dir = join(process.env.TEST_DIR, "cli_signin");
const dirs = {};
for (const name of ["server-home", "cwd", "laptop-home"]) {
  dirs[name] = join(dir, name);
  mkdirSync(dirs[name], { recursive: true });
}

const waitFor = (stream, proc, pattern, what) => new Promise((resolve, reject) => {
  let out = "";
  stream.on("data", data => {
    out += data;
    const m = out.match(pattern);
    if (m) resolve(m[1]);
  });
  proc.on("exit", code => reject(new Error(`${what} exited (${code}):\n${out}`)));
});

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", "sekrit", "-cwd", dirs.cwd],
  { env: { ...process.env, HOME: dirs["server-home"] }, stdio: ["ignore", "ignore", "pipe"] },
);
const upstream = await waitFor(server.stderr, server, /^prigh: prigh-web on http:\/\/(\S+)\/$/m, "the backend");
const [upstreamHost, upstreamPort] = upstream.split(":");

// The proxy: everything goes through with the session cookie; without it,
// a redirect to its login page (a different origin, as authentik's is).
const session = "proxy_session=s3cret";
const proxied = [];
const signedIn = req => (req.headers.cookie ?? "").split(/;\s*/).includes(session);
const proxy = http.createServer((req, res) => {
  if (req.url.startsWith("/login")) {
    res.writeHead(302, { "Set-Cookie": `${session}; Path=/; HttpOnly; SameSite=Lax`, Location: "/" });
    return res.end();
  }
  if (!signedIn(req)) {
    res.writeHead(302, { Location: "https://auth.example.com/outpost.goauthentik.io/start" });
    return res.end();
  }
  proxied.push(`${req.method} ${req.url.split("?")[0]}`);
  const upstreamReq = http.request(
    { host: upstreamHost, port: upstreamPort, method: req.method, path: req.url, headers: req.headers },
    upstreamRes => {
      res.writeHead(upstreamRes.statusCode, upstreamRes.headers);
      upstreamRes.pipe(res);
    },
  );
  req.pipe(upstreamReq);
});
proxy.on("upgrade", (req, socket, head) => {
  if (!signedIn(req)) {
    socket.end("HTTP/1.1 302 Found\r\nLocation: https://auth.example.com/\r\nContent-Length: 0\r\n\r\n");
    return;
  }
  proxied.push(`upgrade ${req.url}`);
  const up = net.connect(Number(upstreamPort), upstreamHost, () => {
    up.write(`${req.method} ${req.url} HTTP/1.1\r\n`);
    for (let i = 0; i < req.rawHeaders.length; i += 2) up.write(`${req.rawHeaders[i]}: ${req.rawHeaders[i + 1]}\r\n`);
    up.write("\r\n");
    up.write(head);
    up.pipe(socket);
    socket.pipe(up);
  });
  up.on("error", () => socket.destroy());
  socket.on("error", () => up.destroy());
});
await new Promise(resolve => proxy.listen(0, "127.0.0.1", resolve));
const site_url = `http://127.0.0.1:${proxy.address().port}/`;

const laptopEnv = { ...process.env, HOME: dirs["laptop-home"] };
for (const name of ["PRIGH_CONNECT", "PRIGH_TOKEN", "PRIGH_USER"]) delete laptopEnv[name];
const signin = (program) => {
  const proc = spawn(
    backend,
    ["browser-signin", "-connect", site_url, "-no-browser", "-program", program],
    { env: laptopEnv, stdio: ["ignore", "ignore", "pipe"] },
  );
  const url = waitFor(proc.stderr, proc, /^(http:\/\/\S+#cli-signin=\S+)$/m, "prigh browser-signin");
  const done = new Promise(resolve => {
    let err = "";
    proc.stderr.on("data", data => { err += data; });
    proc.on("exit", code => resolve({ code, err }));
  });
  return { proc, url, done };
};

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
const watch = p => {
  p.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget|401 \(Unauthorized\)/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  p.on("pageerror", error => errors.push(`page: ${error.message}`));
};
const clean = text => text
  .replaceAll(dir, "$DIR")
  .replace(/127\.0\.0\.1:\d+/g, "127.0.0.1:<PORT>")
  .replace(/-L \d+:/g, "-L <PORT>:")
  .replace(/cli-signin=\d+\.[0-9a-f]+/g, "cli-signin=<PORT>.<STATE>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: cli_signin: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-cli_signin-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const children = [];

// Nothing of the confirmation is off screen or covered.
const layout = p => p.evaluate(() => {
  const out = [];
  for (const el of document.querySelectorAll(".cli-signin button")) {
    const r = el.getBoundingClientRect();
    const covered = document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2);
    out.push(`${el.textContent}: ${r.height >= 44 ? "44px+" : `${r.height}px`}, ${r.right <= innerWidth && r.bottom <= innerHeight ? "on screen" : "OFF SCREEN"}${covered === el || el.contains(covered) ? "" : ", COVERED"}`);
  }
  out.push(`overflows sideways: ${document.documentElement.scrollWidth > innerWidth}`);
  return out.join("\n");
});

const contexts = [
  ["desktop", { viewport: { width: 1280, height: 800 } }],
  ["phone", {
    viewport: { width: 390, height: 844 },
    hasTouch: true,
    ...(engineName === "firefox" ? {} : { isMobile: true }),
  }],
];

try {
  for (const [label, options] of contexts) {
    const context = await browser.newContext(options);
    const page = await context.newPage();
    watch(page);

    section(`${label}: the program`);
    const first = signin(label === "desktop" ? "tui" : "tool-host");
    children.push(first.proc);
    const url = await first.url;
    console.log(clean(url));

    // Not signed in to the proxy: the browser lands on its login (blocked
    // here: another origin), so sign in to the proxy first, as a user would.
    await page.goto(new URL("/login", site_url).href);
    // A new tab, as the program opens (not a change of fragment on the page).
    await page.goto("about:blank");
    await page.goto(url);
    // Not signed in to prigh: the page asks to sign in first.
    await page.locator(".cli-signin").waitFor();
    section(`${label}: asked`);
    console.log(clean(await page.locator(".cli-signin").innerText()));
    console.log(clean(page.url()));
    await screenshot(page, `${label} asked`);
    await page.locator("#cli-signin-allow").click();
    await page.locator("#signin-form").waitFor();
    section(`${label}: not signed in to prigh`);
    console.log(clean(await page.locator(".signin-error").innerText()));
    await page.locator("#password").fill("sekrit");
    await page.locator("#signin-form button[type=submit]").click();
    // Signed in, the page asks again.
    await page.locator("#cli-signin-allow").waitFor();
    await page.waitForFunction(() => !document.querySelector("#cli-signin-allow").disabled);
    section(`${label}: asked again, signed in`);
    console.log(clean(await page.locator(".cli-signin").innerText()));
    console.log(await layout(page));
    await screenshot(page, `${label} asked again`);
    await page.locator("#cli-signin-allow").click();
    await page.getByText("Signed in").first().waitFor();
    section(`${label}: the program's page`);
    console.log(clean(await page.locator("body").innerText()));
    await screenshot(page, `${label} done`);
    const { code, err } = await first.done;
    section(`${label}: the program signed in`);
    console.log(`exit ${code}`);
    console.log(clean(err.split("\n").filter(l => !l.includes("#cli-signin=")).join("\n")));
    const saved = readFileSync(join(dirs["laptop-home"], ".prigh", "tui-connection.sexp"), "utf8");
    console.log(clean(saved).replace(session, "<THE PROXY'S COOKIE>"));

    // Cancelling tells the program, which stops.
    const second = signin("web");
    children.push(second.proc);
    await page.goto("about:blank");
    await page.goto(await second.url);
    await page.locator("#cli-signin-cancel").waitFor();
    await page.keyboard.press("Escape");
    const cancelled = await second.done;
    section(`${label}: cancelled with Esc`);
    console.log(`exit ${cancelled.code}`);
    console.log(clean(cancelled.err.split("\n").filter(l => !l.includes("#cli-signin=")).join("\n")));
    await context.close();
  }

  section("through the proxy");
  console.log([...new Set(proxied.filter(r => /cli-signin|upgrade/.test(r)))].join("\n"));
  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(
    () => children.forEach(c => c.kill()),
    () => server.kill(),
    () => proxy.close(),
    () => browser.close(),
  );
}
