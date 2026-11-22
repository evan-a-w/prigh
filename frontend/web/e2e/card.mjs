// The context ring at the composer's right (the activity, the context, the
// cost: its card) and what applies over the chat: as they stand idle, while
// the agent works with a subagent running, with only the subagent left, and
// with a mode on (then in the narrowest sidebar), in the dark and light
// themes, on a long branch; then with the sidebar closed and on a phone.
// For each it prints what the ring says (its name and its status for
// screen readers), whether it turns, its card's mode, the notes over the
// chat, and whether anything spills out. Run by prigh_web.sh; prints normalised
// results (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots of the card and the page.
//
// The main agent's bash call and its subagent's wait for files the script
// creates, so that what runs when is fixed.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS.
import { execFileSync, spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { hostname } from "node:os";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: card.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "card");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("card.mjs: timed out"); process.exit(2); }, 240000).unref();

const branch = "feature/a-rather-long-branch-for-the-sidebar-card";
const longHost = "build-runner-eu-west-a-long-hostname";
let toolHost;
const git = (...args) =>
  execFileSync("git", ["-c", "user.name=e2e", "-c", "user.email=e2e@example.com", ...args], { cwd, stdio: "ignore" });
writeFileSync(join(cwd, "README.md"), "card\n");
git("init", "-q", "-b", branch);
git("add", "README.md");
git("commit", "-q", "-m", "start");

// Each waits at most as long as the script may run, and every gate is
// opened at teardown, so no shell is left polling.
const waitFor = file => `timeout 300 sh -c 'until [ -f ${file} ]; do sleep 0.1; done'; echo ${file}`;
const gates = ["sub-go", "main-go"];
const open = gate => writeFileSync(join(cwd, gate), "");
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    {
      text: "Delegating the survey.",
      tool_calls: [
        { id: "s1", name: "subagent", arguments: { task: "Survey the project" } },
        { id: "w1", name: "bash", arguments: { command: waitFor("main-go") } },
      ],
    },
    { text: "Looking around.", tool_calls: [{ id: "b1", name: "bash", arguments: { command: waitFor("sub-go") } }] },
    { text: "Done waiting." },
    { text: "Summary: nothing to report." },
    { text: "The survey is in." },
  ]),
);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux-script", join(dir, "script.json"),
    "-token", token, "-cwd", cwd],
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
const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: card: ${label} ===`);
let shot = 0;
const screenshot = async (label, selector) => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(300); // what just appeared fades in
  shot++;
  const path = join(process.env.SHOTS, `${engineName}-card-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`);
  if (selector) await page.locator(selector).screenshot({ path });
  else await page.screenshot({ path });
};
const clean = text =>
  text.split("\n").map(line => line.replaceAll(hostname(), "<HOSTNAME>").replace(/\$\d+\.\d+/g, "$<COST>").replace(/\s+/g, " ").trim()).join("\n");

// The composer's chips, by row (they wrap rather than spill), with anything
// outside the composer or cut off without an ellipsis.
const chips = () => page.evaluate(() => {
  const root = document.querySelector(".composer-row");
  if (!root) return "(no chips)";
  const box = document.querySelector(".composer").getBoundingClientRect();
  const rows = [];
  for (const e of root.querySelectorAll(".chip-button")) {
    const r = e.getBoundingClientRect();
    if (r.width === 0) continue;
    let row = rows.find(row => Math.abs(row.top - r.top) < 6);
    if (!row) rows.push((row = { top: r.top, items: [] }));
    row.items.push({ left: r.left, text: e.innerText.trim().replace(/\s+/g, " ") || "icon" });
  }
  rows.sort((a, b) => a.top - b.top);
  // (Not the ring's card, its menus, or what only screen readers read.)
  const problems = [...root.querySelectorAll("*")].filter(e => !e.closest(".context-card, #chip-popover, .sr-only")).flatMap(e => {
    const r = e.getBoundingClientRect();
    if (r.width === 0) return [];
    const out = r.left < box.left - 1 || r.right > box.right + 1 ? ["outside"] : [];
    const cs = getComputedStyle(e);
    const cut = e.children.length === 0 && e.scrollWidth > e.clientWidth + 1 && cs.textOverflow !== "ellipsis"
      && cs.overflow !== "visible" ? ["cut off"] : [];
    return [...out, ...cut].map(p => `${e.className || e.tagName}: ${p}`);
  });
  const host = root.querySelector(".host-chip .chip-label");
  const truncated = host && host.scrollWidth > host.clientWidth + 1 ? " (host name truncated)" : "";
  return [
    ...rows.map((row, i) => `chips ${i + 1}: ${row.items.sort((a, b) => a.left - b.left).map(i => i.text).join(" | ")}`),
    `chip problems: ${problems.join(", ") || "none"}${truncated}`,
  ].join("\n");
});

// The ring: what it says (its name, its status for screen readers),
// whether it turns and its level; its card's mode; the notes over the chat;
// and whether the ring is outside the composer or on a neighbour.
const card = () => page.evaluate(() => {
  const anchor = document.querySelector(".context-ring-anchor");
  if (!anchor) return "(no ring)";
  const ring = anchor.querySelector(".context-ring");
  const shown = e => {
    const r = e.getBoundingClientRect();
    return r.width > 0 && r.height > 0 && getComputedStyle(e).visibility !== "hidden";
  };
  const level = ring.className.replace("context-ring", "").trim() || "calm";
  const status = anchor.querySelector("[role=status]")?.textContent.trim();
  const mode = anchor.querySelector(".context-card .mode-line .status-detail-value")?.textContent.trim();
  const notes = [...document.querySelectorAll(".chat-chrome .status-notes .status-item")].filter(shown)
    .filter(e => !e.parentElement.closest(".status-item")).map(e => e.innerText.trim().replace(/\s+/g, " ") || "icon");
  const r = ring.getBoundingClientRect();
  const box = document.querySelector(".composer-box").getBoundingClientRect();
  const problems = [];
  if (!shown(ring)) problems.push("hidden");
  if (r.left < box.left - 1 || r.right > box.right + 1) problems.push("outside the composer");
  for (const e of document.querySelectorAll(".composer-row button")) {
    if (e === ring || !shown(e) || e.closest(".context-card, #chip-popover")) continue;
    const o = e.getBoundingClientRect();
    if (r.left < o.right - 0.5 && o.left < r.right - 0.5 && r.top < o.bottom - 0.5 && o.top < r.bottom - 0.5)
      problems.push(`on ${e.getAttribute("aria-label") || e.className}`);
  }
  return [
    `ring: ${ring.getAttribute("aria-label")}; ${status}; turning: ${anchor.classList.contains("busy")}; ${level}`,
    `mode: ${mode}; over the chat: ${notes.join(" | ") || "nothing"}`,
    problems.length ? `problems: ${problems.join("; ")}` : "fits",
  ].join("\n");
});
const command = async text => {
  await page.locator(".composer textarea").fill(text);
  await page.locator(".composer textarea").press("Enter");
};
const theme = async name => {
  await command(`/theme ${name}`);
  await page.waitForFunction(t => document.documentElement.dataset.theme === t, name);
};
// The card in each theme, then back to dark.
const both = async label => {
  section(label);
  console.log(clean(await card()));
  for (const t of ["dark", "light"]) {
    await theme(t);
    await screenshot(`${label} ${t}`, ".composer");
  }
  await screenshot(label);
  await theme("dark");
};

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await page.locator(".composer textarea").waitFor();
  await page.locator(".context-ring-anchor:not(.busy)").waitFor({ state: "attached" });
  await both("idle");

  // The agent at work, its subagent too.
  await command("survey the project");
  // The subagent tool at work (its call written), as the card had it.
  await page.locator(".context-ring-anchor.busy [role=status]", { hasText: "Running tools" }).waitFor({ state: "attached" });
  await page.waitForFunction(() => document.querySelector(".chat")?.innerText.includes("main-go"));
  await both("working, a subagent running");

  // The agent done, its subagent still running.
  open("main-go");
  await page.locator(".context-ring-anchor:not(.busy)").waitFor({ state: "attached" });
  await both("ready, a subagent running");

  // All done, and a mode on (Alt+M: ask).
  open("sub-go");
  // The agent answers its subagent's report: wait for that turn to end.
  await page.waitForFunction(() => document.querySelector(".chat")?.innerText.includes("The survey is in."));
  await page.locator(".context-ring-anchor:not(.busy)").waitFor({ state: "attached" });
  const mode = () => document.querySelector(".context-card .mode-line .status-detail-value")?.textContent;
  const before = await page.evaluate(mode);
  await page.keyboard.press("Alt+m");
  await page.waitForFunction(([b, f]) => new Function(`return (${f})()`)() !== b, [before, mode.toString()]);
  await both("idle, a mode on");

  // The sidebar at its narrowest: still one line for the activity and the
  // cost, and nothing spills. A double click gives it its width back.
  const resizer = page.locator('.resizer[data-resize="sidebar"]');
  const box = await resizer.boundingBox();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x - 200, box.y + box.height / 2, { steps: 4 });
  await page.mouse.up();
  await page.waitForFunction(() => document.querySelector('.resizer[data-resize="sidebar"]').getAttribute("aria-valuenow") === "200");
  await both("the sidebar at its narrowest");
  await resizer.dblclick();
  await page.waitForFunction(() => document.querySelector('.resizer[data-resize="sidebar"]').getAttribute("aria-valuenow") === "280");

  // The composer's chip names where tools run, the backend's machine.
  section("the composer's chips");
  console.log(clean(await chips()));

  // Another host, with a long name, joins: the chip's menu lists it, a
  // filter finds it and, once picked, the chip names it; the card never
  // does.
  toolHost = spawn(
    backend,
    ["tool-host", "-connect", new URL(url).host, "-token", token, "-cwd", cwd,
      "-name", longHost, "-host-id", "c7"],
    { env: { ...process.env, HOME: home }, stdio: ["ignore", "ignore", "pipe"] },
  );
  let hostErr = "";
  toolHost.stderr.on("data", d => { hostErr += d; });
  toolHost.on("exit", code => errors.push(`tool host exited (${code}): ${hostErr}`));
  await page.locator(".host-chip").click();
  await page.locator("#chip-popover[data-chip=host] .popover-item", { hasText: longHost }).waitFor();
  section("another host to choose");
  console.log(clean((await page.locator("#chip-popover[data-chip=host] .popover-item").evaluateAll(items =>
    items.map(e => `${e.classList.contains("marked") ? "✓ " : ""}${e.querySelector(".menu-label").innerText}`)))
    .join("\n").replace(/\(in [0-9a-f]+\)/g, "(in <SESSION>)")));
  await screenshot("the host chip's menu");
  await page.locator("#popover-filter").fill("euwest");
  await page.waitForFunction(() => document.querySelectorAll("#chip-popover[data-chip=host] .popover-item").length === 1);
  await page.keyboard.press("Enter");
  // It asks in which directory, the session's at first.
  await page.locator(".modal input").waitFor();
  await page.locator(".modal input").press("Enter");
  await page.locator(".host-chip", { hasText: longHost }).waitFor();
  await both("on a host with a long name");
  console.log(clean(await chips()));

  // The ring's card's Details open the session's details.
  await page.locator(".composer .context-ring").click();
  await page.locator(".context-card .context-details").click();
  await page.locator(".modal").waitFor();
  section("the ring's card's Details open the session's details");
  console.log(clean(await page.locator(".modal h2, .modal .dialog-title").first().innerText()));
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal", { state: "detached" });

  // The sidebar closed: the chrome over the chat gains its ☰.
  await page.keyboard.press("Control+b");
  await page.locator(".chat-chrome").waitFor();
  section("the sidebar closed");
  console.log(clean(await card()));
  await screenshot("the sidebar closed");
  await page.keyboard.press("Control+b");
  await page.locator(".chat-chrome").waitFor();

  // A phone: ☰, the session's name and ⋯; the status at the composer.
  await page.setViewportSize({ width: 390, height: 760 });
  await page.locator(".chat-chrome").waitFor();
  // WebKit lays the phone out a few frames after the resize: wait until the
  // composer fits the new width and the layout holds still for two frames.
  await page.waitForFunction(() => {
    const box = document.querySelector(".composer")?.getBoundingClientRect();
    return window.innerWidth === 390 && box && box.right <= 390 && box.width > 0;
  });
  await page.evaluate(async () => {
    const sizes = () => [".composer", ".context-ring", ".composer-row"]
      .map(s => document.querySelector(s)?.getBoundingClientRect())
      .map(r => r && `${r.left},${r.top},${r.width},${r.height}`).join(";");
    const frame = () => new Promise(requestAnimationFrame);
    let last = sizes();
    for (let held = 0; held < 2; ) {
      await frame();
      const now = sizes();
      held = now === last ? held + 1 : 0;
      last = now;
    }
  });
  section("a phone");
  console.log(clean(await card()));
  console.log(clean(await chips()));
  await screenshot("a phone");

  // The host freezes (as a laptop that sleeps: its connection stays open,
  // nothing more comes): the backend gives up on it after 30s of silence
  // and the chip says it is offline; its menu keeps it, dimmed, and
  // choosing it says why at once. Resumed, it reconnects.
  await page.setViewportSize({ width: 390, height: 844 });
  process.kill(toolHost.pid, "SIGSTOP");
  await page.locator(".host-chip.offline").waitFor({ timeout: 60000 });
  section("the host stopped answering");
  console.log(clean(await chips()));
  await page.locator(".host-chip").click();
  const hostItems = "#chip-popover[data-chip=host] .popover-item";
  await page.locator(hostItems, { hasText: "offline" }).waitFor();
  console.log(clean((await page.locator(hostItems).evaluateAll(items =>
    items.map(e => `${e.classList.contains("marked") ? "✓ " : ""}${e.classList.contains("dimmed") ? "(dimmed) " : ""}`
      + `${e.querySelector(".menu-label").innerText} ${e.querySelector(".menu-detail")?.innerText ?? ""}`)))
    .join("\n").replace(/\(in [0-9a-f]+\)/g, "(in <SESSION>)").replaceAll(cwd, "<CWD>")));
  await screenshot("an offline host's menu 390");
  await page.setViewportSize({ width: 320, height: 640 });
  await screenshot("an offline host's menu 320");
  await page.locator(hostItems, { hasText: "offline" }).click();
  await page.locator(".toast.error").waitFor();
  console.log(clean(await page.locator(".toast.error").innerText()));
  await screenshot("choosing an offline host");
  process.kill(toolHost.pid, "SIGCONT");
  await page.locator(".host-chip:not(.offline)").waitFor({ timeout: 60000 });
  section("the host is back");
  console.log(clean(await chips()));

  if (errors.length) {
    section("errors");
    console.log(errors.join("\n"));
  }
} catch (e) {
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  process.exitCode = 1;
} finally {
  for (const gate of gates) open(gate);
  await end(() => browser.close(), () => { toolHost?.kill("SIGCONT"); toolHost?.kill(); }, () => server.kill());
}
