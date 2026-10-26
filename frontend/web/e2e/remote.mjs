// `prigh web -connect`: prigh-web served on "this machine" for a backend on
// "another" (two homes, two directories): the page opens signed in by its
// one-time link, its tools run on this machine's tool host, a reload stays
// signed in, and the spent link signs nobody else in. Run by prigh_web.sh;
// prints normalised snapshots (the expected output is in ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: remote.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site } = process.env;
const dir = join(process.env.TEST_DIR, "remote");
const dirs = {};
for (const name of ["server-home", "server-cwd", "laptop-home", "laptop-cwd"]) {
  dirs[name] = join(dir, name);
  mkdirSync(dirs[name], { recursive: true });
}
writeFileSync(join(dirs["laptop-cwd"], "on-the-laptop.txt"), "");
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { text: "looking", tool_calls: [{ id: "c1", name: "bash", arguments: { command: "pwd; ls" } }] },
    { text: "found it" },
  ]),
);

// Resolves with the first match of [pattern] in [proc]'s stderr.
const waitFor = (proc, pattern, what) => new Promise((resolve, reject) => {
  let err = "";
  proc.stderr.on("data", data => {
    err += data;
    const m = err.match(pattern);
    if (m) resolve(m[1]);
  });
  proc.on("exit", code => reject(new Error(`${what} exited (${code}):\n${err}`)));
});

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", "sekrit", "-cwd", dirs["server-cwd"]],
  { env: { ...process.env, HOME: dirs["server-home"] }, stdio: ["ignore", "ignore", "pipe"] },
);
let laptop;
const serverAddress = await waitFor(server, /^prigh: prigh-web on http:\/\/(\S+)\/$/m, "the backend");

const env = { ...process.env, HOME: dirs["laptop-home"] };
for (const name of ["PRIGH_CONNECT", "PRIGH_TOKEN", "PRIGH_USER", "PRIGH_WEB_LISTEN"]) delete env[name];
laptop = spawn(
  backend,
  ["web", "-connect", serverAddress, "-token", "sekrit", "-listen", "127.0.0.1:0", "-web-root", site,
    "-cwd", dirs["laptop-cwd"], "-name", "laptop", "-host-id", "host-laptop"],
  { env, stdio: ["ignore", "ignore", "pipe"] },
);
const url = await waitFor(laptop, /^prigh web: prigh-web for \S+ on (\S+)$/m, "prigh web");

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
const page = await context.newPage();
const errors = [];
const watch = p => {
  p.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|403 \(Forbidden\)/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  p.on("pageerror", error => errors.push(`page: ${error.message}`));
};
watch(page);

const clean = text => text
  .replaceAll(dir, "$DIR")
  .replace(/127\.0\.0\.1:\d+/g, "127.0.0.1:<PORT>")
  .replace(/#signin=[0-9a-f]+/g, "#signin=<code>")
  .replace(/session=[0-9a-f]+/g, "session=<id>")
  .replace(/(Yesterday )?\b\d\d:\d\d\b/g, "<TIME>")
  .split("\n")
  .map(line => line.trim())
  .filter(line => line !== "" && line !== "Today")
  .join("\n");
const section = label => console.log(`=== ${engineName}: remote: ${label} ===`);
let shot = 0;
const screenshot = async (p, label) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-remote-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const idle = () => page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));

try {
  section("prigh web");
  console.log(clean(url));

  // Signed in by the link, which leaves the address bar.
  await page.goto(url);
  await page.locator(".composer textarea").waitFor();
  section("opened");
  console.log(clean(page.url()));
  console.log(`sign-in form: ${await page.locator("#signin-form").count()}`);
  await screenshot(page, "opened");

  // The tools run on this machine, in -cwd.
  await page.locator(".composer textarea").fill("where am I?");
  await page.locator(".btn.send").click();
  await page.waitForFunction(() => document.body.innerText.includes("found it"));
  await idle();
  await page.getByText("on-the-laptop.txt").first().waitFor();
  section("the tool ran on the laptop");
  const chat = await page.locator("#chat").innerText();
  console.log(`in the laptop's directory: ${chat.includes(dirs["laptop-cwd"])}`);
  console.log(`in the server's directory: ${chat.includes(dirs["server-cwd"])}`);
  await screenshot(page, "tool ran on the laptop");

  // A reload stays signed in, on this machine's host.
  await page.reload();
  await page.locator(".composer textarea").waitFor();
  await page.waitForFunction(() => document.body.innerText.includes("found it"));
  section("reloaded");
  console.log(`sign-in form: ${await page.locator("#signin-form").count()}`);
  console.log(`prefer_host: ${await page.evaluate(() => localStorage.getItem("prigh.prefer_host"))}`);

  // The link is spent: another browser gets the sign-in form.
  const other = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
  watch(other);
  await other.goto(url);
  await other.locator("#signin-form").waitFor();
  section("the spent link elsewhere");
  console.log(clean(await other.locator(".signin h1").innerText()));
  await screenshot(other, "spent link");

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  await screenshot(page, "failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await browser.close();
  laptop?.kill();
  server.kill();
}
