// `prigh desktop -ui remote` (the desktop app's process) for a backend whose
// site is behind a sign-in proxy (a fake of authentik's forward auth, /ws
// exempt): instead of the window looping through the proxy's login, the
// page here offers signing in with the default browser or in the window.
// The window is a page here; the "default browser" another context, as the
// shell would open it. Through the browser (Esc cancels, Open it again),
// the window then goes to the site with the proxy's cookie (sent as the
// shell would); the sign-in expiring brings it back to the choice, on a
// desktop and on phones; signing in in the window. Run by prigh_web.sh;
// prints normalised snapshots (the expected output is in ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync } from "node:fs";
import http from "node:http";
import net from "node:net";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: desktop_signin.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site } = process.env;
const dir = join(process.env.TEST_DIR, "desktop_signin");
const dirs = {};
for (const name of ["server-home", "server-cwd", "laptop-home", "laptop-cwd"]) {
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
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", "sekrit", "-cwd", dirs["server-cwd"]],
  { env: { ...process.env, HOME: dirs["server-home"] }, stdio: ["ignore", "ignore", "pipe"] },
);
const upstream = await waitFor(server.stderr, server, /^prigh: prigh-web on http:\/\/(\S+)\/$/m, "the backend");
const [upstreamHost, upstreamPort] = upstream.split(":");

// The proxy: pages go through with the session cookie, else a redirect to
// its login (another origin, with a new state each time); /ws and
// /terminal always (prigh checks its own token there).
const session = "proxy_session=s3cret";
let logins = 0;
const signedIn = req => (req.headers.cookie ?? "").split(/;\s*/).includes(session);
const exempt = req => /^\/(ws|terminal)(\?|$)/.test(req.url);
const proxy = http.createServer((req, res) => {
  if (req.url.startsWith("/login")) {
    res.writeHead(302, { "Set-Cookie": `${session}; Path=/; HttpOnly; SameSite=Lax`, Location: "/" });
    return res.end();
  }
  if (!signedIn(req)) {
    logins++;
    res.writeHead(302, { Location: `https://auth.example.com/outpost.goauthentik.io/start?state=${logins}` });
    return res.end();
  }
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
  if (!signedIn(req) && !exempt(req)) {
    socket.end("HTTP/1.1 302 Found\r\nLocation: https://auth.example.com/\r\nContent-Length: 0\r\n\r\n");
    return;
  }
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
const siteUrl = `http://127.0.0.1:${proxy.address().port}/`;

// `prigh desktop`, its stdout's page lines read one at a time, as the shell
// does.
const laptopEnv = { ...process.env, HOME: dirs["laptop-home"] };
for (const name of ["PRIGH_CONNECT", "PRIGH_TOKEN", "PRIGH_USER"]) delete laptopEnv[name];
const desktop = spawn(
  backend,
  ["desktop", "-ui", "remote", "-connect", `${siteUrl}`, "-token", "sekrit", "-listen", "127.0.0.1:0",
    "-web-root", site, "-cwd", dirs["laptop-cwd"], "-name", "laptop", "-host-id", "host-laptop"],
  { env: laptopEnv, stdio: ["pipe", "pipe", "pipe"] },
);
let err = "";
desktop.stderr.on("data", data => { err += data; });
const lines = [];
const waiting = [];
let out = "";
desktop.stdout.on("data", data => {
  out += data;
  let i;
  while ((i = out.indexOf("\n")) >= 0) {
    const line = JSON.parse(out.slice(0, i));
    out = out.slice(i + 1);
    if (waiting.length) waiting.shift()(line);
    else lines.push(line);
  }
});
const exited = new Promise(done => desktop.on("exit", (code, signal) => done(code ?? signal)));
const nextLine = () => lines.length
  ? Promise.resolve(lines.shift())
  : new Promise((resolve, reject) => {
    waiting.push(resolve);
    exited.then(code => reject(new Error(`prigh desktop exited (${code}):\n${err}`)));
  });

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
  .replace(/#desktop=[0-9a-f]+/g, "#desktop=<key>")
  .replace(/cli-signin=\d+\.[0-9a-f]+/g, "cli-signin=<PORT>.<STATE>")
  .replaceAll(session, "<THE PROXY'S COOKIE>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "")
  .join("\n");
const section = label => console.log(`=== ${engineName}: desktop_signin: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-desktop_signin-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const showLine = line => console.log(`page line: ${clean(JSON.stringify(line))}`);

// Every button 44px or more, on screen and not covered; nothing sideways.
const layout = p => p.evaluate(() => {
  const out = [];
  for (const el of document.querySelectorAll(".signin button")) {
    const r = el.getBoundingClientRect();
    const covered = document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2);
    out.push(`${el.textContent}: ${r.height >= 44 ? "44px+" : `${Math.round(r.height)}px`}, ${r.left >= 0 && r.right <= innerWidth && r.bottom <= innerHeight ? "on screen" : "OFF SCREEN"}${covered === el || el.contains(covered) ? "" : ", COVERED"}`);
  }
  out.push(`overflows sideways: ${document.documentElement.scrollWidth > innerWidth}`);
  out.push(`focused: ${document.activeElement?.id || "nothing"}`);
  return out.join("\n");
});
const phone = (width, height) => {
  const options = { viewport: { width, height }, hasTouch: true };
  if (engineName !== "firefox") options.isMobile = true;
  return options;
};

let page;
try {
  section("its stdout");
  const first = await nextLine();
  showLine(first);

  // The window: the choice, not the proxy's login.
  const windowContext = await browser.newContext({ viewport: { width: 1100, height: 760 } });
  page = await windowContext.newPage();
  watch(page);
  await page.goto(first.url);
  await page.locator("#desktop-signin-browser").waitFor();
  section("the choice");
  console.log(clean(await page.locator(".signin").innerText()));
  console.log(await layout(page));
  await screenshot(page, "the choice");

  // Through the browser: the shell is told to open it; Esc cancels.
  await page.keyboard.press("Enter");
  showLine(await nextLine());
  await page.locator("#desktop-signin-cancel").waitFor();
  section("waiting");
  console.log(clean(await page.locator(".signin").innerText()));
  console.log(await layout(page));
  await screenshot(page, "waiting");
  await page.keyboard.press("Escape");
  await page.locator("#desktop-signin-browser").waitFor();
  section("cancelled with Esc");
  console.log(clean(await page.locator(".signin").innerText()));

  await page.locator("#desktop-signin-browser").click();
  const external = await nextLine();
  showLine(external);
  await page.locator("#desktop-signin-again").click();
  section("opened again");
  showLine(await nextLine());

  // The shell puts the proxy's cookie in the window's cookies before it
  // goes to the site.
  const signedInLine = nextLine();
  const cookieSet = signedInLine.then(({ cookie }) => {
    const [name, value] = cookie.split("=");
    return windowContext.addCookies([{ name, value, url: siteUrl }]);
  });
  await page.route(`${siteUrl}**`, async route => {
    await cookieSet;
    await route.continue();
  });

  // The default browser, already signed in to the proxy: prigh-web asks to
  // allow prigh desktop (after signing in to prigh).
  const other = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
  watch(other);
  await other.goto(new URL("/login", siteUrl).href);
  await other.goto("about:blank");
  await other.goto(external.url);
  await other.locator("#cli-signin-allow").click();
  await other.locator("#signin-form").waitFor();
  await other.locator("#password").fill("sekrit");
  await other.locator("#signin-form button[type=submit]").click();
  await other.locator("#cli-signin-allow").waitFor();
  await other.waitForFunction(() => !document.querySelector("#cli-signin-allow").disabled);
  section("the default browser asks");
  console.log(clean(await other.locator(".cli-signin").innerText()));
  await other.locator("#cli-signin-allow").click();
  await other.getByText("Signed in").first().waitFor();
  console.log(clean(await other.locator("body").innerText()));
  await other.context().close();

  section("signed in: the window goes to the site");
  const signedIn = await signedInLine;
  showLine(signedIn);
  await page.locator("#signin-form, .composer textarea").first().waitFor();
  console.log(clean(page.url()));
  console.log(`the site's sign-in to prigh: ${await page.locator("#signin-form").count()}`);
  await screenshot(page, "the site");

  // The sign-in expires: the proxy sends the window to its login, and the
  // shell brings it back to the choice.
  await page.unroute(`${siteUrl}**`);
  await windowContext.clearCookies();
  await page.goto(signedIn.signin);
  await page.locator("#desktop-signin-browser").waitFor();
  section("expired");
  console.log(clean(await page.locator(".signin").innerText()));
  await screenshot(page, "expired");

  for (const [width, height] of [[390, 844], [320, 640]]) {
    const small = await (await browser.newContext(phone(width, height))).newPage();
    watch(small);
    await small.goto(signedIn.signin);
    await small.locator("#desktop-signin-browser").waitFor();
    section(`expired, on a ${width}x${height} phone`);
    console.log(await layout(small));
    await screenshot(small, `phone ${width}`);
    await small.context().close();
  }

  // In the window this time.
  await page.locator("#desktop-signin-here").click();
  section("in the window");
  showLine(await nextLine());

  desktop.stdin.end();
  section("its window gone");
  console.log(`exit: ${await exited}`);
  console.log(clean(err.split("\n").filter(l => !/tool host|tools run/.test(l)).join("\n")));
  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  if (page) await screenshot(page, "failure").catch(() => {});
  console.error(errors.join("\n"));
  console.error(err);
  throw error;
} finally {
  await end(() => desktop.kill(), () => server.kill(), () => proxy.close(), () => browser.close());
}
