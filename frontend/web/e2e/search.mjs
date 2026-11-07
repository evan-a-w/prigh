// The sidebar's search: words matched in the titles at once, then the
// sessions whose messages match (the backend's search_sessions), each with
// the passage that matched, the matched words marked; nothing found; Esc
// clearing it; and the same on two phones, nothing spilling sideways.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: search.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "search");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
const sessions = join(home, ".prigh", "sessions");
mkdirSync(sessions, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("search.mjs: timed out"); process.exit(2); }, 240000).unref();

// Three saved sessions, two of them pinned; the fourth is made in the page,
// its reply and the pinned checklist's the only places "configurable" is said.
const usage = { input: 1200, output: 300, cache_read: 0 };
const writeSession = (sid, name, minutesAgo, messages, { pinned = false } = {}) => {
  let seq = 0;
  let parent = null;
  let at = Math.floor(Date.now() / 1000) - minutesAgo * 60;
  const created = new Date(at * 1000).toISOString().replace("T", " ").replace(/\.(\d+)Z$/, ".$1000Z");
  const lines = [JSON.stringify(["Header", { id: sid, cwd, created_at: created }])];
  const entry = payload => {
    const eid = `${sid.slice(0, 8)}${String(seq++).padStart(8, "0")}`;
    lines.push(JSON.stringify(["Entry", { id: eid, parent, at: (at += 7), payload }]));
    parent = eid;
  };
  entry(["Name", { name }]);
  if (pinned) entry(["Pinned", { pinned: true }]);
  for (const m of messages) entry(["Message", m]);
  writeFileSync(join(sessions, `20261001-100000000_${sid}.jsonl`), lines.join("\n") + "\n");
};
const reply = text => ["Assistant", { content: [["Text", text]], stop_reason: ["End_turn"], usage, model: "faux" }];
writeSession("a1a1a1a1a1a1a1a1", "Port the tmux tests", 60, [
  ["User", { text: "move the tmux tests to a private socket" }],
  reply("Done: they use tmux -L now."),
], { pinned: true });
writeSession("b2b2b2b2b2b2b2b2", "Release notes", 120, [
  ["User", { text: "write the release notes" }],
  reply("Here are the notes for this release."),
]);
writeSession("c3c3c3c3c3c3c3c3", "Deploy checklist", 180, [
  ["User", { text: "what must the deploy check?" }],
  reply("That the port stays configurable, and the certificates are fresh."),
], { pinned: true });
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { text: "You can make the listen port configurable through the config file: add a `port` key." },
  ]),
);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
  { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
);
const url = await new Promise((resolve, reject) => {
  let err = "";
  const timer = setTimeout(() => reject(new Error(`backend did not start:\n${err}`)), 30000);
  server.stderr.on("data", data => {
    err += data;
    const m = err.match(/^prigh: prigh-web on (\S+)$/m);
    if (m) { clearTimeout(timer); resolve(m[1]); }
  });
  server.on("exit", code => { clearTimeout(timer); reject(new Error(`backend exited (${code}):\n${err}`)); });
});
const stopServer = () =>
  new Promise(resolve => {
    server.removeAllListeners("exit");
    if (server.exitCode !== null || server.signalCode !== null) return resolve();
    const timer = setTimeout(() => { server.kill("SIGKILL"); }, 5000);
    server.on("exit", () => { clearTimeout(timer); resolve(); });
    server.kill();
  });

const browser = await engine.launch({
  headless: true,
  args: engineName === "chromium" ? ["--no-sandbox", "--disable-dev-shm-usage", "--disable-gpu"] : [],
});
const errors = [];
let page;
const open = async options => {
  const context = await browser.newContext(options);
  context.setDefaultTimeout(20000);
  page = await context.newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text()))
      errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
};

const section = label => console.log(`=== ${engineName}: search: ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.waitForFunction(() => document.getAnimations().every(a => a.playState !== "running"));
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-search-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};

let sessionUrl = null;
const signIn = async () => {
  await page.goto(sessionUrl ?? url);
  await page.waitForSelector("form input[type=password]");
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();
};

// Each group's heading, then its rows: the title (after "📌 " when it shows
// the pin), then its passage, the marked words in [..].
const rows = () => page.locator(".sessions .sessions-group, .sessions .session").evaluateAll(es => es.map(e => {
  if (e.matches(".sessions-group"))
    return `-- ${e.querySelector(".divider-label").textContent} ${e.querySelector(".divider-count").textContent} --`;
  const marked = el => el
    ? [...el.childNodes].map(n => (n.nodeName === "MARK" ? `[${n.textContent}]` : n.textContent)).join("")
    : null;
  const pin = e.querySelector(".session-title .session-pin") ? "📌 " : "";
  const title = pin + marked(e.querySelector(".session-title")).trim();
  const snippet = marked(e.querySelector(".session-snippet"));
  return snippet ? `${title}\n    ${snippet}` : title;
}).join("\n"));
const search = async (query, options = {}) => {
  await page.locator("#session-search").fill(query, options);
  await page.waitForFunction(q => document.querySelector("#session-search")?.value === q, query, options);
};

const query = "configurable port";
// Both passages that say it: the pinned checklist's and the new session's.
const configurableTwice = (options = {}) =>
  page.waitForFunction(
    () => [...document.querySelectorAll(".session-snippet mark")].filter(m => m.textContent === "configurable").length === 2,
    null,
    options,
  );

try {
  await open({ viewport: { width: 1280, height: 800 } });
  await signIn();
  const editor = page.locator(".composer textarea");
  await editor.fill("where does the server listen?");
  await editor.press("Enter");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("through the config file"));
  sessionUrl = page.url();
  await page.locator(".session", { hasText: "Release notes" }).waitFor();

  section("a title and a reply");
  await search("port");
  await page.waitForFunction(() => document.querySelectorAll(".session-snippet").length === 2);
  console.log(await rows());

  section("the messages, once typing pauses");
  await search(query);
  await configurableTwice();
  console.log(await rows());
  await screenshot("desktop found");

  section("nothing matches");
  await search("zebra quokka");
  await page.locator(".sessions-empty").waitFor();
  console.log(await page.locator(".sessions-empty").innerText());

  section("Esc clears the search");
  await page.locator("#session-search").press("Escape");
  await page.waitForFunction(() => document.querySelector("#session-search").value === "");
  await page.locator(".session", { hasText: "Release notes" }).waitFor();
  console.log(`query: "${await page.locator("#session-search").inputValue()}"; rows: ${await page.locator(".sessions .session").count()}`);
  await page.context().close();

  for (const [w, h] of [[390, 844], [320, 640]]) {
    const options = { viewport: { width: w, height: h }, hasTouch: true };
    if (engineName !== "firefox") options.isMobile = true;
    await open(options);
    await signIn();
    // The session rejoined, its state in: it resets the search.
    await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("through the config file"));
    await page.getByRole("button", { name: "Sessions" }).first().tap();
    await page.locator("#session-search").waitFor();
    await page.locator(".session", { hasText: "Release notes" }).waitFor();
    // Just signed in, the page may still reset the search box once as the
    // session's state arrives (it may reload): type again until the passage shows.
    for (let attempt = 0; ; attempt++) {
      try {
        await search(query, { timeout: 4000 });
        await configurableTwice({ timeout: 4000 });
        break;
      } catch (e) {
        if (attempt >= 3) throw e;
      }
    }
    section(`${w}x${h}`);
    console.log(await rows());
    console.log(await page.evaluate(() => {
      const doc = document.documentElement;
      const sidebar = document.querySelector(".sidebar");
      const inside = [...document.querySelectorAll(".session-snippet")].map(e => e.getBoundingClientRect())
        .every(s => s.left >= 0 && s.right <= innerWidth && s.top >= 0 && s.bottom <= innerHeight);
      return [
        `page overflows sideways: ${doc.scrollWidth > doc.clientWidth}`,
        `sidebar overflows sideways: ${sidebar.scrollWidth > sidebar.clientWidth}`,
        `snippets inside the screen: ${inside}`,
      ].join("\n");
    }));
    await screenshot(`${w}x${h} found`);
    await page.context().close();
  }

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure").catch(() => {});
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => stopServer(), () => browser.close());
}
