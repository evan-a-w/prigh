// The composer engine: a backend deployed with -engines harness,composer
// -adapters codex,claude and stand-ins for the CLIs on the tool host's PATH
// (`prigh fake-cli`, replaying a script: Codex installed and logged in,
// Claude Code not installed). The new-session screen's engine chip and its
// picker, a session on Codex (the CLI started on the backend's own tool
// host), a turn streaming text, the model chip naming the harness and its
// model, a command Codex asks to run answered in the approval dialog, a
// control Codex sessions do not have (/btw) refused with the reason, and
// Settings → Engines: each CLI's state on the host, with what to do. On a
// desktop and two phones (390x844, 320x640, touch, the larger as an
// installed PWA with the safe area's insets), the phones checked as
// mobile.mjs does. Run by prigh_web.sh; prints normalised snapshots (the
// expected output is in ENGINE.expected) and, with SHOTS, screenshots.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { chmodSync, existsSync, mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { delimiter, join } from "node:path";
import { end } from "./end.mjs";
import { audit } from "./phone_audit.mjs";
import { normalVerbosity } from "./verbose_prefs.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName, part = "desktop"] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
const viewports = {
  desktop: { width: 1280, height: 800 },
  phone: { width: 390, height: 844 },
  small: { width: 320, height: 640 },
};
const viewport = viewports[part];
if (!engine || !viewport) throw new Error("usage: engines.mjs chromium|firefox|webkit desktop|phone|small");
const touch = part !== "desktop";
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, `engines-${part}`);
const home = join(dir, "home");
normalVerbosity(home);
const cwd = join(dir, "cwd");
// The CLIs' directory goes on the PATH, which a ":" in TEST_DIR (prigh_web.sh's
// engines:desktop) would split.
const bin = mkdtempSync(join(tmpdir(), "prigh-engines-bin-"));
for (const d of [home, cwd]) mkdirSync(d, { recursive: true });
setTimeout(() => { console.error("engines.mjs: timed out"); process.exit(2); }, 240000).unref();

// What `codex` says and does: two turns of `codex app-server`, the second
// asking to run a command.
const thread = "thr_e2e";
const codexScript = () => {
  const ev = (turn, method, params) => ({ type: "emit_inbound", frame: { method, params: { threadId: thread, turnId: turn, ...params } } });
  const message = (turn, id, parts) => [
    ev(turn, "item/started", { item: { type: "agentMessage", id, text: "", phase: "final_answer" } }),
    ...parts.map(delta => ev(turn, "item/agentMessage/delta", { itemId: id, delta })),
    ev(turn, "item/completed", { item: { type: "agentMessage", id, text: parts.join(""), phase: "final_answer" } }),
  ];
  const usage = turn => ev(turn, "thread/tokenUsage/updated", {
    tokenUsage: {
      total: { totalTokens: 2400, inputTokens: 2300, cachedInputTokens: 0, outputTokens: 100, reasoningOutputTokens: 0 },
      last: { totalTokens: 2400, inputTokens: 2300, cachedInputTokens: 0, outputTokens: 100, reasoningOutputTokens: 0 },
      modelContextWindow: 258400,
    },
  });
  const turnFrame = (turn, status) => ({ id: turn, items: [], status, error: null });
  const startTurn = (id, turn) => [
    { type: "expect_outbound", frame: { id, method: "turn/start" } },
    { type: "emit_inbound", frame: { id, result: { turn: turnFrame(turn, "inProgress") } } },
    { type: "emit_inbound", frame: { method: "turn/started", params: { threadId: thread, turn: turnFrame(turn, "inProgress") } } },
  ];
  const endTurn = turn => [
    usage(turn),
    { type: "emit_inbound", frame: { method: "turn/completed", params: { threadId: thread, turn: turnFrame(turn, "completed") } } },
  ];
  const command = { type: "commandExecution", id: "c1", command: "/bin/bash -lc 'ls'", cwd, status: "inProgress", commandActions: [{ type: "listFiles", command: "ls", path: null }], aggregatedOutput: null, exitCode: null };
  return [
    { type: "command", args: ["--version"], stdout: "codex-cli 0.166.0\n" },
    { type: "command", args: ["login", "status"], stdout: "Logged in using ChatGPT\n" },
    { type: "expect_outbound", frame: { id: 1, method: "initialize" } },
    { type: "emit_inbound", frame: { id: 1, result: { userAgent: "codex_cli_rs/0.166.0 (linux; x86_64)", codexHome: join(home, ".codex"), platformFamily: "unix", platformOs: "linux" } } },
    { type: "expect_outbound", frame: { method: "initialized" } },
    { type: "expect_outbound", frame: { id: 2, method: "thread/start" } },
    { type: "emit_inbound", frame: { id: 2, result: { thread: { id: thread }, model: "gpt-5.2-codex", modelProvider: "openai", cwd, approvalPolicy: "untrusted", sandbox: { type: "readOnly" }, reasoningEffort: null } } },
    ...startTurn(3, "turn_1"),
    ...message("turn_1", "m1", ["Hello from ", "Codex: ", "ask me to look at the project."]),
    ...endTurn("turn_1"),
    ...startTurn(4, "turn_2"),
    ...message("turn_2", "m2", ["I'll list ", "the project's files."]),
    ev("turn_2", "item/started", { item: command }),
    { type: "emit_inbound", frame: { method: "item/commandExecution/requestApproval", id: 0, params: { threadId: thread, turnId: "turn_2", itemId: "c1", reason: "List the project's files?", command: "/bin/bash -lc 'ls'", cwd } } },
    { type: "expect_outbound", frame: { id: 0 } },
    { type: "emit_inbound", frame: { method: "serverRequest/resolved", params: { threadId: thread, requestId: 0 } } },
    ev("turn_2", "item/commandExecution/outputDelta", { itemId: "c1", delta: "README.md\nsrc\n" }),
    ev("turn_2", "item/completed", { item: { ...command, status: "completed", aggregatedOutput: "README.md\nsrc\n", exitCode: 0 } }),
    ...message("turn_2", "m3", ["Two entries: ", "README.md and src."]),
    ...endTurn("turn_2"),
  ];
};
writeFileSync(join(dir, "codex.jsonl"), codexScript().map(l => JSON.stringify(l)).join("\n") + "\n");
writeFileSync(join(bin, "codex"), `#!/bin/sh\nexec "${backend}" fake-cli codex --script "${join(dir, "codex.jsonl")}" -- "$@"\n`);
chmodSync(join(bin, "codex"), 0o755);
// No `claude` on the PATH: Claude Code is not installed (even where the
// machine running the tests has one).
const path = [bin, ...process.env.PATH.split(delimiter).filter(d => d && !existsSync(join(d, "claude")))].join(delimiter);

const server = spawn(
  backend,
  ["serve", "-web", "127.0.0.1:0", "-web-root", site, "-faux", "-token", token, "-cwd", cwd,
    "-engines", "harness,composer", "-adapters", "codex,claude"],
  { env: { ...process.env, HOME: home, PATH: path, TMUX_TMPDIR: dir, SHELL: "/bin/sh" }, stdio: ["ignore", "ignore", "pipe"] },
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
const context = await browser.newContext({
  viewport,
  reducedMotion: "reduce",
  hasTouch: touch,
  ...(touch && engineName !== "firefox" ? { isMobile: true } : {}),
});
context.setDefaultTimeout(20000);
const page = await context.newPage();
page.on("console", message => {
  if (message.type() === "error" && !/favicon\.ico|interactive-widget|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: engines (${part}): ${label} ===`);
let shot = 0;
const screenshot = async label => {
  if (!process.env.SHOTS) return;
  shot++;
  await page.screenshot({
    path: join(process.env.SHOTS, `${engineName}-engines-${part}-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
// On a phone, what mobile.mjs checks.
const phoneAudit = async () => {
  if (touch) console.log(`phone: ${await audit(page)}`);
};
const tap = locator => (touch ? locator.tap() : locator.click());
const composer = page.locator(".composer textarea");
const flat = s => s.replace(/\s+/g, " ").trim();
const clean = s => flat(s)
  .replace(/\b\d\d:\d\d\b/g, "<TIME>")
  .replaceAll(bin, "<BIN>")
  .replaceAll(dir, "<DIR>");
const send = async text => {
  await composer.fill(text);
  await tap(page.locator(".btn.send"));
};
const toast = text => page.locator(".toast", { hasText: text }).last();
const settled = locator => locator.evaluate(el => Promise.all(el.getAnimations({ subtree: true }).map(a => a.finished)));
const exactly = s => new RegExp(`^${s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}$`);
const pickerItem = label =>
  page.locator(".picker-dialog .picker-item", { has: page.locator(".picker-label", { hasText: exactly(label) }) });

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await composer.waitFor();
  if (part === "phone" && engineName === "chromium") {
    const cdp = await context.newCDPSession(page);
    await cdp.send("Emulation.setSafeAreaInsetsOverride", { insets: { top: 47, bottom: 34, left: 0, right: 0 } });
  }

  // 1. The new-session screen's engine chip, and its picker.
  section("the new-session engine chip");
  const chip = page.locator(".tray-engine");
  console.log(`chip: ${flat(await chip.innerText())} (${await chip.getAttribute("aria-label")})`);
  await tap(chip);
  const picker = page.locator(".picker-dialog");
  await pickerItem("Codex").waitFor();
  await settled(picker);
  section("the engine picker");
  console.log((await picker.locator(".picker-item").evaluateAll(items => items.map(i => i.innerText))).map(flat).join("\n"));
  await phoneAudit();
  await screenshot("engine picker");
  await tap(pickerItem("Codex"));
  await picker.waitFor({ state: "detached" });
  await page.locator(".tray-engine-name", { hasText: "Codex" }).waitFor();
  section("chosen");
  console.log(`chip: ${flat(await chip.innerText())}`);

  // 2. A turn on Codex: the CLI starts on the tool host and streams.
  await send("Hello");
  const reply = page.locator("#chat .msg.assistant", { hasText: "ask me to look at the project." });
  await reply.waitFor();
  await page.waitForFunction(() => !document.querySelector(".app.running, .composer .btn.stop"));
  section("the first turn");
  console.log(clean(await reply.innerText()));
  const model = page.locator(".composer .model-chip");
  await model.filter({ hasText: "gpt-5.2-codex" }).waitFor();
  console.log(`model chip: ${flat(await model.innerText())}`);
  // Listed with the others, like a harness session.
  const listed = page.locator(".sidebar [data-session]", { hasText: "Hello" });
  await listed.waitFor({ state: "attached" });
  console.log(`sidebar: ${await page.locator(".sidebar [data-session]").count()} session, "Hello"`);
  await phoneAudit();
  await screenshot("first turn");

  // The model menu's foot: the execution mode selector (Native / Host) beside
  // the access mode.
  await tap(model);
  const engineButton = page.locator(".model-engine .model-access-button");
  await tap(engineButton);
  const engineMenu = page.locator(".model-engine .model-access-menu");
  await engineMenu.locator(".model-access-item").first().waitFor();
  await settled(page.locator("#chip-popover"));
  section("the model menu's mode");
  console.log(`button: ${await engineButton.getAttribute("aria-label")}`);
  console.log((await engineMenu.locator(".model-access-item").evaluateAll(items => items.map(i => i.innerText))).map(clean).join("\n"));
  // The open menu covers the list by design: only its targets are checked.
  if (touch) {
    const small = await page.locator(".model-engine .model-access-button, .model-engine .model-access-item")
      .evaluateAll(els => els.filter(e => e.getBoundingClientRect().height < 44).length);
    console.log(`phone: ${small} targets under 44px`);
  }
  await screenshot("model menu mode");
  await page.keyboard.press("Escape");
  await engineMenu.waitFor({ state: "detached" });
  await page.keyboard.press("Escape");
  await page.locator("#chip-popover").waitFor({ state: "detached" });

  // 3. Ask mode: the command Codex asks to run goes to the approval dialog.
  await send("/mode ask");
  await toast("Ask").waitFor();
  await send("What's in the project?");
  const confirm = page.locator("#confirm");
  await confirm.waitFor();
  await settled(confirm);
  section("Codex asks to run a command");
  console.log(clean(await confirm.innerText()));
  await phoneAudit();
  await screenshot("approval");
  await tap(page.locator("#confirm .confirm-answer.allow"));
  await confirm.waitFor({ state: "detached" });
  const answer = page.locator("#chat .msg.assistant", { hasText: "README.md and src." });
  await answer.waitFor();
  section("the command ran");
  const card = page.locator("#chat .tool").last();
  console.log(`card: ${clean(await card.locator(".tool-head").innerText())}`);
  console.log(clean(await answer.innerText()));
  await screenshot("after the command");

  // 4. What Codex sessions cannot do is refused, saying why.
  await send("/btw what changed?");
  const refused = toast("side questions");
  await refused.waitFor();
  section("a control Codex sessions do not have");
  console.log(flat(await refused.innerText()));

  // 5. Settings → Engines: each CLI on the host, and what to do.
  await send("/settings engines");
  const settings = page.locator(".settings-dialog");
  await settings.waitFor();
  const codexRow = settings.locator(".adapter-status.adapter-codex");
  const claudeRow = settings.locator(".adapter-status.adapter-claude");
  await codexRow.waitFor();
  await claudeRow.waitFor();
  // Done checking: the button says Refresh again.
  await page.waitForFunction(() => !document.querySelector(".settings-dialog .adapters-refresh")?.innerText.includes("Checking"));
  await settled(settings);
  section("Settings → Engines");
  console.log(clean(await settings.locator(".adapters-refresh").innerText()));
  for (const row of [codexRow, claudeRow]) console.log(clean(await row.innerText()));
  console.log(`copy: ${await claudeRow.locator("button").getAttribute("title")}`);
  await phoneAudit();
  await screenshot("settings engines");

  // 6. Switching mode from the model menu: it keeps its place and size,
  // whatever mode is browsed and however many models it has.
  await page.keyboard.press("Escape");
  await settings.waitFor({ state: "detached" });
  await tap(model);
  await page.locator(".model-list .model-row").first().waitFor();
  await settled(page.locator("#chip-popover"));
  const box = () => page.locator("#chip-popover").evaluate(e => {
    const r = e.getBoundingClientRect();
    return [r.left, r.top, r.width, r.height].map(Math.round).join(",");
  });
  const switchTo = async (name) => {
    await tap(engineButton);
    await tap(engineMenu.locator(".model-access-item", { hasText: name }));
    await page.waitForFunction(n => document.querySelector(".model-engine .model-access-button")?.getAttribute("aria-label") === `Mode: ${n}`, name);
    await page.locator(".model-list .model-row").first().waitFor();
    await settled(page.locator("#chip-popover"));
  };
  const before = await box();
  await switchTo("Native");
  section("the model menu on another mode");
  console.log(`held: ${(await box()) === before}`);
  await screenshot("model menu native");
  await switchTo("Host");
  console.log(`held, back on Host: ${(await box()) === before}`);
} catch (e) {
  console.log(`failed: ${e.message.split("\n")[0]}`);
  await screenshot("failed");
  process.exitCode = 1;
}
section("errors");
console.log(errors.join("\n") || "none");
await end(() => browser.close(), () => server.kill(), () => rmSync(bin, { recursive: true, force: true }));
