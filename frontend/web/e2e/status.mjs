// Where the session's status sits (/status-style): each of the six places,
// idle, with its details open and while the agent works, in each theme, on a
// desktop and on a phone. For each style it prints what shows (each status
// element, by its accessible name, and whether anything spills out of the
// window), that the details are reachable by the keyboard, and that the
// context opens the session's details. Run by prigh_web.sh; prints
// normalised results (the expected output is in ENGINE.expected) and, with
// SHOTS, screenshots named STYLE-THEME-SIZE-STATE.png.
//
// The agent's bash call waits for a file the script creates, so that the
// working state holds while it is looked at.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN, and
// optionally SHOTS (and STATUS_THEMES, the themes to shoot, default all).
import { spawn } from "node:child_process";
import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: status.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "status");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
setTimeout(() => { console.error("status.mjs: timed out"); process.exit(2); }, 280000).unref();

const styles = ["island", "ring", "whisper", "border", "hub", "sidebar"];
// Screenshots in every theme when asked for; the checks need one.
const themes = process.env.SHOTS ? (process.env.STATUS_THEMES || "original dark light").split(" ") : ["dark"];
const sizes = { desktop: [1280, 800], phone: [390, 760] };

// One turn at work for each size and theme, each held by its own file (a
// foreground command is moved to the background after 30s).
const gates = Array.from({ length: 6 }, (_, i) => `go-${i}`);
const waitFor = file => `timeout 300 sh -c 'until [ -f ${file} ]; do sleep 0.1; done'; echo ${file}`;
const open = gate => writeFileSync(join(cwd, gate), "");
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    { text: "The port is read in `server.ml` from a literal.\n\nI changed `Server.start` to take it from `PORT`, falling back to **8080**.", usage: { input: 360000, output: 12000, cache_read: 300000 } },
    ...gates.flatMap((gate, i) => [
      { text: "Checking the tests.", usage: { input: 380000 + i, output: 14000, cache_read: 340000 },
        tool_calls: [{ id: `b${i}`, name: "bash", arguments: { command: waitFor(gate) } }] },
      { text: "All tests pass.", usage: { input: 390000 + i, output: 15000, cache_read: 350000 } },
    ]),
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
  if (message.type() === "error" && !/favicon\.ico|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
});
page.on("pageerror", error => errors.push(`page: ${error.message}`));

const section = label => console.log(`=== ${engineName}: status: ${label} ===`);
const clean = text => text.replace(/\$\d+\.\d+/g, "$<COST>").replace(/\b\d+(\.\d+)?k\b/g, "<N>k");
const screenshot = async (style, theme, size, state) => {
  if (!process.env.SHOTS) return;
  await page.waitForTimeout(350); // what just appeared fades in
  await page.screenshot({ path: join(process.env.SHOTS, `${style}-${theme}-${size}-${state}.png`) });
};
const editor = page.locator(".composer textarea");
const command = async text => {
  await editor.fill(text);
  await editor.press("Enter");
  await page.waitForFunction(() => document.querySelector(".composer textarea")?.value === "");
};
const setTheme = async name => {
  await command(`/theme ${name}`);
  await page.waitForFunction(t => document.documentElement.dataset.theme === t, name);
};
const setStyle = async name => {
  await command(`/status-style ${name}`);
  await page.waitForFunction(n => document.querySelector(".composer")?.classList.contains(`status-style-${n}`), name);
  await page.mouse.move(640, 300);
};

// What shows of the status: each element, by its accessible name or text,
// and whether anything of the status leaves the window.
const report = () => page.evaluate(() => {
  const shown = e => {
    const r = e.getBoundingClientRect();
    return r.width > 1 && r.height > 1 && getComputedStyle(e).visibility !== "hidden"
      && !e.closest(".sr-only, .status-details");
  };
  const where = {
    "chrome": ".chat-chrome",
    "island": ".status-island",
    "send ring": ".send-ring",
    "row chips": ".row-chips",
    "whisper": ".status-whisper",
    "border meter": ".border-meter",
    "border tag": ".border-tag",
    "hub badge": ".hub-badge",
    "sidebar card": ".status-card",
  };
  const lines = [];
  for (const [name, sel] of Object.entries(where)) {
    const el = [...document.querySelectorAll(sel)].find(shown);
    if (!el || name === "chrome") continue;
    const parts = [...el.querySelectorAll("button, .status-item, .percent")].filter(shown)
      .filter(e => !e.parentElement.closest("button, .status-item"))
      .map(e => (e.getAttribute("aria-label") || e.innerText).trim().replace(/\s+/g, " "))
      .filter(Boolean);
    lines.push(`${name}: ${parts.join(" | ") || "(drawn)"}`);
  }
  const chrome = document.querySelector(".chat-chrome");
  const buttons = chrome ? [...chrome.querySelectorAll("button")].filter(shown)
    .filter(b => !b.closest(".status-island"))
    .map(b => b.getAttribute("aria-label") || b.title).join(", ") : "(none)";
  lines.push(`chrome buttons: ${buttons}`);
  const spill = [...document.querySelectorAll(".chat-chrome *, .composer *, .status-card *")].filter(shown)
    .filter(e => { const r = e.getBoundingClientRect(); return r.left < -1 || r.right > innerWidth + 1; })
    .map(e => e.className || e.tagName);
  lines.push(spill.length ? `spills: ${[...new Set(spill)].join(", ")}` : "fits");
  const details = document.querySelectorAll(".status-details").length;
  lines.push(`details cards: ${details}; header bar: ${document.querySelector(".session-bar") ? "yes" : "none"}`);
  return lines.join("\n");
});
// The details open from the keyboard: Tab to the context's button.
const keyboardDetails = async () => {
  const target = page.locator("button.context:visible, .status-item.context:visible").first();
  if (!(await target.count())) {
    const hidden = page.locator("button.context").first();
    if (!(await hidden.count())) return "no context button";
    await hidden.focus();
  } else await target.focus();
  await page.keyboard.press("Shift+Tab");
  await page.keyboard.press("Tab");
  return page.evaluate(() => {
    const d = document.activeElement?.closest(".status-gauge")?.querySelector(".status-details");
    if (!d) return "the focus is not in a gauge";
    const shown = getComputedStyle(d).visibility !== "hidden";
    const described = document.activeElement.getAttribute("aria-describedby") === d.id;
    return `details by keyboard: ${shown ? "shown" : "hidden"}, ${described ? "described" : "not described"}`;
  });
};
// Hover what opens the details, in each style.
const hoverDetails = async style => {
  const sel = {
    island: ".status-island .status-gauge",
    ring: ".send-gauge",
    whisper: ".status-whisper .status-gauge",
    border: ".composer",
    hub: ".model-chip",
    sidebar: ".status-card .status-gauge, .status-whisper .status-gauge",
  }[style];
  const el = page.locator(sel).first();
  if (style === "hub") {
    await el.click();
    await page.locator("#chip-popover .hub-sections").waitFor();
    return;
  }
  if (style === "border") {
    // The tag shows on hover of the composer; then its gauge.
    await el.hover();
    await page.locator(".border-tag .status-item.context").hover();
  } else await el.hover();
  await page.waitForTimeout(400);
};

try {
  await page.goto(url);
  await page.locator("form input[type=password]").fill(token);
  await page.locator("form button[type=submit]").click();
  await editor.waitFor();
  // The toasts the commands leave would cover what is shot.
  if (process.env.SHOTS) await page.addStyleTag({ content: ".toasts, .toast { display: none !important; }" });
  await command("make the port configurable");
  await page.waitForFunction(() => document.querySelector(".chat")?.innerText.includes("falling back to"));
  await page.waitForFunction(() => document.title.includes("prigh"));
  section("the tab's title");
  console.log(await page.evaluate(() => document.title));

  for (const [size, [w, h]] of Object.entries(sizes)) {
    await page.setViewportSize({ width: w, height: h });
    for (const theme of themes) {
      await setTheme(theme);
      for (const style of styles) {
        await setStyle(style);
        if (theme === "dark") {
          section(`${style}, ${size}, idle`);
          console.log(clean(await report()));
          if (size === "desktop") console.log(await keyboardDetails());
          await page.evaluate(() => document.activeElement?.blur());
        }
        await screenshot(style, theme, size, "idle");
        await hoverDetails(style);
        await screenshot(style, theme, size, "detail");
        await page.keyboard.press("Escape");
        await page.mouse.move(w / 2, 200);
      }
    }
  }

  // Clicking the context opens the session's details, from the island.
  await page.setViewportSize({ width: 1280, height: 800 });
  await setStyle("island");
  await page.locator(".status-island button.context").click();
  await page.locator(".modal").waitFor();
  section("the context opens the session's details");
  console.log(await page.locator(".modal h2, .modal .dialog-title").first().innerText());
  await page.keyboard.press("Escape");
  await page.waitForSelector(".modal", { state: "detached" });

  // The hub's menu changes the thinking and the mode and stays open.
  await setStyle("hub");
  await page.locator(".model-chip").click();
  await page.locator("#chip-popover .hub-sections").waitFor();
  section("the hub's menu");
  console.log(await page.locator("#chip-popover .hub-title").evaluateAll(t => t.map(e => e.innerText).join(", ")));
  await page.locator("#chip-popover .hub-choice", { hasText: /^auto$/ }).click();
  await page.locator("#chip-popover .hub-choice.on", { hasText: /^auto$/ }).waitFor();
  console.log(`menu open after a choice: ${await page.locator("#chip-popover").count() === 1}`);
  await page.keyboard.press("Escape");
  await page.locator(".hub-badge .hub-mode.loud").waitFor();
  console.log("the badge shows the mode");
  // Alt+M still cycles the mode; then back to auto, which never asks.
  await page.keyboard.press("Alt+m");
  await page.locator(".hub-badge .hub-mode.loud").waitFor();
  await page.locator(".model-chip").click();
  await page.locator("#chip-popover .hub-choice", { hasText: /^auto$/ }).click();
  await page.locator("#chip-popover .hub-choice.on", { hasText: /^auto$/ }).waitFor();
  await page.keyboard.press("Escape");

  // While the agent works.
  let turn = 0;
  for (const [size, [w, h]] of Object.entries(sizes)) {
    await page.setViewportSize({ width: w, height: h });
    for (const theme of themes) {
      await setTheme(theme);
      await command("run the tests");
      await page.waitForFunction(() => document.querySelector(".composer.busy"));
      await page.locator(".tool-call, .chat").getByText(`go-${turn}`).first().waitFor();
      // The bash tool at work (not still writing its call): one activity to print.
      await page.waitForFunction(() => [...document.querySelectorAll(".status-item.activity")].some(e => e.textContent.includes("Running tools")));
      for (const style of styles) {
        await setStyle(style);
        if (theme === "dark" && size === "desktop") {
          section(`${style}, working`);
          console.log(clean(await report()));
        }
        await screenshot(style, theme, size, "working");
      }
      open(gates[turn++]);
      await page.waitForFunction(() => !document.querySelector(".composer.busy"));
    }
  }
  await setStyle("island");

  if (errors.length) {
    section("errors");
    console.log(errors.join("\n"));
  }
} catch (e) {
  if (process.env.SHOTS) await page.screenshot({ path: join(process.env.SHOTS, "failed.png") }).catch(() => {});
  console.log(`FAILED: ${e.stack}\n${errors.join("\n")}`);
  process.exitCode = 1;
} finally {
  for (const gate of gates) open(gate);
  await end(() => browser.close(), () => server.kill());
}
