// Themes and accents, and the user's preferences on the backend: the
// settings page and /theme and /accent change the page at once; the choice
// is in ~/.prigh/prefs.json, comes back before the first paint of a reload,
// reaches a browser that signs in afresh, and changes another open browser
// live, as does the prompt history. Run by prigh_web.sh; prints normalised
// snapshots (the expected output is in ENGINE.expected) and, with SHOTS,
// screenshots of a transcript (markdown, code, a diff, the terminal) in each
// theme.
//
// Environment: PLAYWRIGHT_MODULE, PRIGH_BACKEND, SITE, TEST_DIR, TOKEN and
// optionally SHOTS.
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { end } from "./end.mjs";

const { chromium, firefox, webkit } = await import(process.env.PLAYWRIGHT_MODULE);

const [engineName] = process.argv.slice(2);
const engine = { chromium, firefox, webkit }[engineName];
if (!engine) throw new Error("usage: themes.mjs chromium|firefox|webkit");
const { PRIGH_BACKEND: backend, SITE: site, TOKEN: token } = process.env;
const dir = join(process.env.TEST_DIR, "themes");
const home = join(dir, "home");
const cwd = join(dir, "cwd");
mkdirSync(home, { recursive: true });
mkdirSync(cwd, { recursive: true });
writeFileSync(
  join(cwd, "server.ml"),
  "let port = 8080\nlet greet name =\n  Printf.printf \"hello, %s\\n\" name\n",
);
const reply = [
  "The server reads its **port** from `server.ml`; here is the change:",
  "",
  "```ocaml",
  "(* Listen where the environment says. *)",
  "let port =",
  "  match Sys.getenv_opt \"PORT\" with",
  "  | Some p -> int_of_string p",
  "  | None -> 8080",
  "```",
  "",
  "- the default stays `8080`",
  "- [docs](https://example.com) describe the rest",
].join("\n");
writeFileSync(
  join(dir, "script.json"),
  JSON.stringify([
    {
      text: "Changing the port.",
      tool_calls: [
        { id: "e1", name: "edit", arguments: { path: "server.ml", edits: [{ old_text: "let port = 8080", new_text: "let port =\n  match Sys.getenv_opt \"PORT\" with\n  | Some p -> int_of_string p\n  | None -> 8080" }] } },
        { id: "b1", name: "bash", arguments: { command: "grep -n port server.ml" } },
      ],
    },
    { text: reply },
    { text: "noted" },
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
const open = async () => {
  const page = await (await browser.newContext({ viewport: { width: 1280, height: 800 } })).newPage();
  page.on("console", message => {
    if (message.type() === "error" && !/favicon\.ico|WebSocket/.test(message.text())) errors.push(`console: ${message.text()}`);
  });
  page.on("pageerror", error => errors.push(`page: ${error.message}`));
  // What <html> says when the document is parsed: before the backend has
  // answered anything.
  await page.addInitScript(() => {
    document.addEventListener("DOMContentLoaded", () => {
      window.__firstPaint = `${document.documentElement.dataset.theme ?? "-"} ${document.documentElement.dataset.accent ?? "-"}`;
    });
  });
  return page;
};
const page = await open();

const section = label => console.log(`=== ${engineName}: themes: ${label} ===`);
let shot = 0;
const screenshot = async (label, p = page) => {
  if (!process.env.SHOTS) return;
  shot++;
  await p.screenshot({
    path: join(process.env.SHOTS, `${engineName}-themes-${String(shot).padStart(2, "0")}-${label.replace(/\W+/g, "-")}.png`),
  });
};
const look = p => p.evaluate(() => {
  const root = document.documentElement;
  const css = getComputedStyle(root);
  return `${root.dataset.theme} ${root.dataset.accent} ${css.getPropertyValue("--sodium").trim()} on ${css.getPropertyValue("--graphite").trim()}`;
});
const lookIs = (p, theme, accent) => p.waitForFunction(([t, a]) =>
  document.documentElement.dataset.theme === t && document.documentElement.dataset.accent === a, [theme, accent]);
const composer = p => p.locator(".composer textarea");
const signIn = async p => {
  await p.goto(url);
  await p.locator("form input[type=password]").fill(token);
  await p.locator("form button[type=submit]").click();
  await composer(p).waitFor();
};
// Once no dialog owns the keyboard; done when the editor is empty again.
const command = async (p, text) => {
  await p.waitForSelector(".modal-backdrop", { state: "detached" });
  await composer(p).fill(text);
  // A bare command: as typed, not the completion of its argument.
  await composer(p).press(text.includes(" ") ? "Enter" : "Control+Enter");
  await p.waitForFunction(() => document.querySelector(".composer textarea").value === "");
};
const prefsFile = () => {
  try {
    const prefs = JSON.parse(readFileSync(join(home, ".prigh/prefs.json"), "utf8"));
    return Object.entries(prefs)
      .filter(([k]) => k !== "history")
      .map(([k, v]) => `${k}=${JSON.stringify(v)}`)
      .join(" ");
  } catch {
    return "(none)";
  }
};
const terminalOpen = async p => {
  if (!(await p.locator(".terminal-panel").count())) await p.keyboard.press("Control+`");
  await p.locator(".terminal-panel .xterm-rows").waitFor();
  await p.waitForTimeout(500);
  await composer(p).click();
};
// The transcript from its start, with everything open, the terminal too.
const showcase = async (label, p = page) => {
  await p.evaluate(() => {
    document.querySelectorAll("details:not(.image):not(.note)").forEach(d => { d.open = true; });
    document.getElementById("chat").scrollTop = 0;
  });
  await p.waitForTimeout(300);
  await screenshot(label, p);
};
// After a dialog's fade in.
const dialogShot = async label => {
  await page.waitForTimeout(250);
  await screenshot(label);
};

try {
  await signIn(page);
  section("first paint before choosing");
  console.log(`${await page.evaluate(() => window.__firstPaint)}; ${await look(page)}`);
  await command(page, "make the port configurable");
  await page.locator(".chat .md-part pre").waitFor();
  await page.waitForFunction(() => !document.querySelector(".streaming, .btn.stop"));
  await terminalOpen(page);
  await showcase("original orange");

  // The settings page: each theme shows at once, and stays open.
  await page.locator(".settings-toggle").click();
  await page.locator(".settings-dialog").waitFor();
  await page.waitForTimeout(200);
  section("settings");
  console.log(await page.locator(".settings-section").first().innerText());
  await dialogShot("settings original");
  await page.locator(".theme-choice", { hasText: "Dark" }).click();
  await lookIs(page, "dark", "orange");
  section("dark from the settings");
  console.log(`${await look(page)}; checked: ${await page.locator(".theme-choice[aria-checked=true]").innerText()}`);
  await dialogShot("settings dark");
  await page.locator(".accent-choice[aria-label=Blue]").click();
  await lookIs(page, "dark", "blue");
  await dialogShot("settings dark blue");
  await page.keyboard.press("Escape");
  await showcase("dark blue");
  section("saved on the backend");
  for (let i = 0; i < 50 && !prefsFile().includes("blue"); i++) await page.waitForTimeout(100);
  console.log(prefsFile());

  // A reload paints it before the backend answers.
  await page.reload();
  await composer(page).waitFor();
  section("reloaded");
  console.log(`first paint: ${await page.evaluate(() => window.__firstPaint)}; ${await look(page)}`);

  // Another browser: signing in brings the user's; a change in one shows
  // in the other at once, and so does a prompt sent.
  const other = await open();
  await signIn(other);
  await lookIs(other, "dark", "blue");
  section("another browser");
  console.log(`first paint: ${await other.evaluate(() => window.__firstPaint)}; ${await look(other)}`);
  await command(page, "/theme light");
  await lookIs(other, "light", "blue");
  await command(page, "/accent #c2410c");
  await lookIs(other, "light", "custom");
  section("changed live in the other");
  console.log(await look(other));
  await composer(other).click();
  await composer(other).press("ArrowUp");
  await other.waitForFunction(() => document.querySelector(".composer textarea").value !== "");
  console.log(`its history: ${await composer(other).inputValue()}`);
  await composer(other).fill("");
  await terminalOpen(page);
  await showcase("light custom");

  // The pickers preview what is highlighted; Esc puts it back.
  await command(page, "/theme");
  await page.locator(".picker-dialog").waitFor();
  await page.keyboard.press("ArrowUp");
  await lookIs(page, "dark", "custom");
  await dialogShot("theme picker previewing dark");
  await page.keyboard.press("Escape");
  await lookIs(page, "light", "custom");
  section("picker");
  console.log(`Esc: ${await look(page)}; saved: ${prefsFile()}`);
  await command(page, "/accent");
  await page.locator(".picker-dialog").waitFor();
  await page.keyboard.type("purp");
  await dialogShot("accent picker");
  await page.keyboard.press("Enter");
  await lookIs(page, "light", "purple");
  await command(page, "/theme original");
  await lookIs(page, "original", "purple");
  await showcase("original purple");
  await command(page, "/accent green");
  await command(page, "/theme dark");
  await lookIs(page, "dark", "green");
  await showcase("dark green");

  // Back to how it started, for the scripts after this one.
  await command(page, "/theme original");
  await command(page, "/accent orange");
  await lookIs(page, "original", "orange");
  await lookIs(other, "original", "orange");
  section("back");
  console.log(`${await look(page)}; the other: ${await look(other)}`);

  section("errors");
  console.log(errors.length ? errors.join("\n") : "none");
} catch (error) {
  process.exitCode = 1;
  await screenshot("failure");
  console.error(errors.join("\n"));
  throw error;
} finally {
  await end(() => server.kill(), () => browser.close());
}
