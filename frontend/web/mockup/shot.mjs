// Screenshots of the mockups: serves the built site itself and shoots each
// spec, `hash:WxH[:mobile][:light]` (e.g. `chat:1440x900`,
// `chat:390x844:mobile`), into OUT_DIR as <hash>-<W>x<H>[-mobile][-light].png
// (`:as=NAME` names it NAME-... instead of <hash>-...).
//
//   node web/mockup/shot.mjs /tmp/newui-shots chat:1440x900 chat:390x844:mobile
//
// Needs PLAYWRIGHT_MODULE, PLAYWRIGHT_BROWSERS_PATH and FONTCONFIG_FILE
// (shot.sh sets them); MOCKUP_DIR defaults to dune's build of this directory.
// A spec may end with `@js` to run before the shot:
// `chat:390x844:mobile:as=chat-sheet@document.querySelector('[data-sidebar=trigger]').click()`.
import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const site =
  process.env.MOCKUP_DIR ?? path.resolve(here, "../../_build/default/web/mockup");
const { chromium } = await import(process.env.PLAYWRIGHT_MODULE);
const [outDir, ...specs] = process.argv.slice(2);
if (!outDir || specs.length === 0) {
  console.error("usage: shot.mjs OUT_DIR hash:WxH[:mobile][:light][@js] ...");
  process.exit(2);
}
fs.mkdirSync(outDir, { recursive: true });

const types = { ".html": "text/html", ".js": "text/javascript", ".css": "text/css" };
const server = http.createServer((req, res) => {
  const file = path.join(site, decodeURIComponent(new URL(req.url, "http://x").pathname));
  const target = file.endsWith("/") ? path.join(file, "index.html") : file;
  fs.readFile(target, (err, data) => {
    if (err) { res.writeHead(404); res.end(); return; }
    res.writeHead(200, { "content-type": types[path.extname(target)] ?? "application/octet-stream" });
    res.end(data);
  });
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const base = `http://127.0.0.1:${server.address().port}/`;

const browser = await chromium.launch();
for (const spec of specs) {
  const [main, js] = spec.split(/@(.*)/s);
  const [hash, size, ...flags] = main.split(":");
  const [width, height] = size.split("x").map(Number);
  const mobile = flags.includes("mobile");
  const light = flags.includes("light");
  const context = await browser.newContext({
    viewport: { width, height },
    deviceScaleFactor: 2,
    isMobile: mobile,
    hasTouch: mobile,
  });
  const page = await context.newPage();
  page.on("pageerror", (e) => console.error(`${spec}: ${e.message}`));
  await page.goto(`${base}index.html${light ? "?theme=light" : ""}#${hash}`);
  await page.waitForFunction(() => document.querySelector("#app, [data-slot]")?.childElementCount !== 0
    && document.querySelector("[data-slot]") !== null);
  if (js) { await page.evaluate(js); }
  await page.evaluate(() => document.fonts.ready);
  await page.waitForTimeout(400); // animations (Presence, tooltips) settle
  const as = flags.find((f) => f.startsWith("as="))?.slice(3);
  const name = `${as ?? hash}-${width}x${height}${mobile ? "-mobile" : ""}${light ? "-light" : ""}.png`;
  await page.screenshot({ path: path.join(outDir, name) });
  console.log(path.join(outDir, name));
  await context.close();
}
await browser.close();
server.close();
