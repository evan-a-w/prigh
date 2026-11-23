// The control centre (Ctrl+K, the sidebar's Search): the session's
// settings with nothing typed; a setting found, its values, one set; the
// sessions, by title at once and then by message (the backend's
// search_sessions), each with the passage that matched, the matched words
// marked; nothing found; Esc; and the same on two phones, nothing spilling
// sideways. Also the sidebar's row and the composer's chips.
// Run by prigh_web.sh; prints normalised results (the expected output is in
// ENGINE.expected).
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, utimesSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";
import { hostName } from "./host_name.mjs";

const phoneSizes = [[390, 844], [320, 640]];
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
  const file = join(sessions, `20261001-100000000_${sid}.jsonl`);
  writeFileSync(file, lines.join("\n") + "\n");
  // The list orders sessions by their files' mtimes: as of their last entries.
  utimesSync(file, at, at);
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

const out = console.log;
console.log = (...xs) => out(...xs.map(x => typeof x === "string" ? x.replaceAll(hostName(), "<HOSTNAME>") : x));
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

// The control centre's rows: each group's heading, then its rows, the label
// (matched letters in [..]), its passage and what is on its right.
const rows = () => page.locator("#control-list > *").evaluateAll(es => es.map(e => {
  const marked = el => el
    ? [...el.childNodes].map(n => (n.nodeName === "MARK" ? `[${n.textContent}]` : n.textContent)).join("").trim()
    : null;
  if (e.matches(".control-group")) return `-- ${e.textContent} --`;
  const sel = e.matches(".selected") ? "> " : "  ";
  const dot = e.querySelector(".control-dot.on") ? "● " : e.querySelector(".control-dot") ? "○ " : "";
  const label = marked(e.querySelector(".control-label"));
  const snippet = marked(e.querySelector(".control-snippet"));
  const right = [e.querySelector(".control-detail")?.textContent, e.querySelector(".control-keys")?.textContent]
    .filter(Boolean).join(" ");
  const about = e.querySelector(".control-about")?.textContent;
  return `${sel}${dot}${label}${snippet ? ` ${snippet}` : ""}${about ? ` — ${about}` : ""}${right ? ` | ${right}` : ""}`
    .replace(/\d+[mhd] ago|just now/, "<age>");
}).join("\n")).then(s => s.replaceAll(cwd, "<CWD>"));
const search = async query => {
  await page.locator("#control-input").fill(query);
  await page.waitForFunction(q => document.querySelector("#control-input")?.value === q, query);
};
const configurableTwice = (options = {}) =>
  page.waitForFunction(
    () => [...document.querySelectorAll(".control-snippet mark")].filter(m => m.textContent === "configurable").length === 2,
    null,
    options,
  );
const fits = () => page.evaluate(async () => {
  // Measured where it comes to rest, not while it slides in.
  await Promise.all(document.querySelector("#control-centre").getAnimations().map(a => a.finished));
  const doc = document.documentElement;
  const box = document.querySelector("#control-centre").getBoundingClientRect();
  const input = document.querySelector("#control-input");
  return [
    `page overflows sideways: ${doc.scrollWidth > doc.clientWidth}`,
    `the box inside the screen: ${box.left >= 0 && box.right <= innerWidth + 0.5 && box.top >= 0 && box.bottom <= innerHeight + 0.5}`,
    `its input's font: ${getComputedStyle(input).fontSize}`,
    `rows at least 44px: ${[...document.querySelectorAll(".control-row")].every(r => r.getBoundingClientRect().height >= 44)}`,
  ].join("\n");
});
const query = "configurable port";

try {
  await open({ viewport: { width: 1280, height: 800 } });
  await signIn();
  const editor = page.locator(".composer textarea");
  await editor.fill("where does the server listen?");
  await editor.press("Enter");
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("through the config file"));
  sessionUrl = page.url();
  await page.locator(".session", { hasText: "Release notes" }).waitFor();

  section("the sidebar's row and the composer's");
  await editor.evaluate(e => e.blur());
  console.log(await page.locator(".sidebar-actions > *").evaluateAll(es => es.map(e => {
    const r = e.getBoundingClientRect();
    return `${e.innerText.trim()}: ${r.width < 120 ? "narrow" : "wide"}, ${Math.round(r.height)}px high`;
  }).join("\n")));
  console.log(`composer: ${await page.locator(".composer-row button:visible").evaluateAll(bs => bs.map(b => (b.getAttribute("aria-label") || b.innerText).trim().split("\n")[0].split(":")[0]).join(", "))}`);
  await screenshot("desktop composer unfocused");
  await editor.focus();
  await screenshot("desktop composer focused");

  section("Ctrl+K: the session's settings, recent sessions, commands");
  await editor.press("Control+k");
  await page.locator("#control-centre").waitFor();
  console.log(`focus in the filter: ${await page.evaluate(() => document.activeElement?.id)}`);
  console.log(await rows());
  console.log(await page.locator(".control-foot").innerText());
  await screenshot("palette empty");

  section("verbos");
  await search("verbos");
  // "Set verbosity" is in the list before the query too: wait for its results.
  await page.waitForFunction(() => {
    const groups = [...document.querySelectorAll("#control-list > .control-group")].map(g => g.textContent);
    return groups.join() === "Settings,Commands";
  });
  console.log(await rows());
  await screenshot("palette verbos");

  section("Enter opens the setting's values; ⌫ goes back; Enter sets one");
  await page.keyboard.press("Enter");
  await page.locator(".control-crumb").waitFor();
  console.log(`crumb: ${await page.locator(".control-crumb").innerText()}`);
  console.log(await rows());
  await screenshot("palette submenu");
  await page.keyboard.press("Backspace");
  await page.locator(".control-crumb").waitFor({ state: "detached" });
  console.log(`back: "${await page.locator("#control-input").inputValue()}"`);
  await page.keyboard.press("Enter");
  await page.locator(".control-crumb").waitFor();
  await page.locator(".control-row", { hasText: "Verbose" }).click();
  await page.locator("#control-centre").waitFor({ state: "detached" });
  await page.keyboard.press("Control+k");
  await search("set verbosity");
  await page.locator(".control-row", { hasText: "Set verbosity" }).waitFor();
  console.log(`now: ${await page.locator(".control-row", { hasText: "Set verbosity" }).locator(".control-detail").innerText()}`);

  section("Esc closes it");
  await page.keyboard.press("Escape");
  await page.locator("#control-centre").waitFor({ state: "detached" });
  console.log(`open: ${await page.locator("#control-centre").count() > 0}`);

  section("the sidebar's Search: a title and a reply");
  await page.locator("#control-open").click();
  await page.locator("#control-centre").waitFor();
  await search("port");
  await page.waitForFunction(() => document.querySelectorAll(".control-snippet").length === 2);
  console.log(await rows());

  section("the messages, once typing pauses");
  await search(query);
  await configurableTwice();
  console.log(await rows());
  await screenshot("desktop found");

  section("nothing matches");
  await search("zebra quokka");
  await page.locator(".control-empty").waitFor();
  console.log(await page.locator(".control-empty").innerText());

  section("Enter opens the session");
  await search("release notes");
  await page.locator(".control-row.selected", { hasText: "Release notes" }).waitFor();
  await page.keyboard.press("Enter");
  await page.locator("#control-centre").waitFor({ state: "detached" });
  await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("notes for this release"));
  console.log("switched to Release notes");
  await page.context().close();

  for (const [w, h] of phoneSizes) {
    const options = { viewport: { width: w, height: h }, hasTouch: true };
    if (engineName !== "firefox") options.isMobile = true;
    await open(options);
    await signIn();
    await page.waitForFunction(() => document.querySelector("#chat")?.innerText.includes("through the config file"));
    await page.getByRole("button", { name: "Sessions" }).first().tap();
    await page.waitForFunction(() => document.querySelector(".app.narrow.sidebar-open #control-open:not([inert] *)"));
    await page.locator("#control-open").tap();
    await page.locator("#control-centre").waitFor();
    section(`${w}x${h}`);
    console.log(await fits());
    await screenshot(`${w}x${h} palette`);
    await search(query);
    await configurableTwice();
    console.log(await rows());
    console.log(await fits());
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
