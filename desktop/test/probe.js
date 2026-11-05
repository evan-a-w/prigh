// Run by the shell (PRIGH_DESKTOP_TEST_SCRIPT) on each of the real `prigh
// desktop`'s pages; run.sh fills in @REPORT@, @ADDRESS@ and @MODE@. It says
// what the page shows by POSTing to the report server and drives it: in
// "close" mode it closes the window once the connect form shows, in "quit"
// mode it signs in with the form (or, on the backend's own prigh-web under
// -ui remote, with its sign-in) and then quits with /quit; "menu" mode
// first opens the thinking chip's menu on the new-session screen and
// chooses from it with the real pointer (run.sh's report server moves and
// clicks it with xdotool), hovering first. "window" mode sends a message,
// then Ctrl+clicks its session in the sidebar: the page opens it in another
// window (where this script runs too: it reports what that window shows and
// closes it), and once that one has closed the first quits.
(() => {
  // What it saw, for the report when the page is stuck: its requests and
  // errors.
  const requests = [];
  const errors = [];
  const open = XMLHttpRequest.prototype.open;
  XMLHttpRequest.prototype.open = function (method, url, ...rest) {
    requests.push([`${method} ${url}`, this]);
    return open.call(this, method, url, ...rest);
  };
  const consoleError = console.error;
  console.error = (...args) => {
    errors.push(args.map(String).join(" "));
    consoleError(...args);
  };
  window.addEventListener("error", e => errors.push(`error: ${e.message}`));
  window.addEventListener("unhandledrejection", e => errors.push(`rejection: ${e.reason}`));
  const seen = () =>
    `requests: ${requests.map(([what, x]) => `${what} ${x.readyState === 4 ? x.status : "pending"}`).join(", ")}; errors: ${errors.join(" | ") || "none"}`;
  const report = text => fetch("@REPORT@", { method: "POST", mode: "no-cors", body: text });
  // The real pointer to the middle of el ("move" or "click"), in the
  // window's pixels: CSS pixels times the zoom.
  const pointer = (what, el) => {
    const r = el.getBoundingClientRect();
    const x = Math.round((r.left + r.width / 2) * devicePixelRatio);
    const y = Math.round((r.top + r.height / 2) * devicePixelRatio);
    return fetch("@REPORT@pointer", { method: "POST", mode: "no-cors", body: `${what} ${x} ${y}` });
  };
  const until = (what, timeout = 5000) =>
    new Promise(resolve => {
      const end = Date.now() + timeout;
      const poll = () => {
        const v = what();
        if (v || Date.now() > end) resolve(v);
        else setTimeout(poll, 50);
      };
      poll();
    });
  const onScreen = el => {
    const r = el.getBoundingClientRect();
    return r.width > 0 && r.height > 0 && r.left >= 0 && r.top >= 0 && r.right <= innerWidth && r.bottom <= innerHeight;
  };
  // Whether the pointer at its middle reaches it (not something over it).
  const reachable = el => {
    const r = el.getBoundingClientRect();
    return el.contains(document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2));
  };
  // Where the last click landed, for a report of one that missed.
  let landed = "nowhere";
  document.addEventListener("pointerdown", e => {
    const r = e.target.getBoundingClientRect();
    landed = `(${Math.round(e.clientX)}, ${Math.round(e.clientY)}) on .${String(e.target.className).split(" ").join(".")} at (${Math.round(r.left)}, ${Math.round(r.top)})`;
  }, true);
  const chooseFromMenu = async () => {
    const fresh = await until(() => document.querySelector(".main.fresh .thinking-chip"), 10000);
    if (!fresh) return report("menu: no thinking chip on the new-session screen");
    const label = () => document.querySelector(".thinking-chip").textContent.trim();
    const before = label();
    await pointer("click", fresh);
    const menu = await until(() => document.querySelector("#chip-popover[data-chip=thinking]"));
    if (!menu) return report(`menu: the thinking chip's click opened nothing: it landed at ${landed}, the chip is at (${Math.round(fresh.getBoundingClientRect().left)}, ${Math.round(fresh.getBoundingClientRect().top)})`);
    await until(() => menu.getAnimations().every(a => a.playState !== "running"), 1000);
    const items = [...menu.querySelectorAll(".popover-item")];
    const name = i => i.querySelector(".menu-label").textContent.trim();
    const item = items.find(i => name(i) !== before);
    if (!item) return report(`menu: no level but ${before} in the menu`);
    await report(`menu: the thinking chip's menu shows (${items.length} levels), on screen: ${onScreen(menu)}; ${name(item)} reachable: ${reachable(item)}`);
    await pointer("move", item);
    const hovered = await until(() => item.matches(":hover"), 3000);
    await report(`menu: the pointer over ${name(item)}: hovered ${hovered}`);
    await pointer("click", item);
    const closed = await until(() => !document.querySelector("#chip-popover") && label() !== before);
    await report(`menu: clicked ${name(item)}: the menu ${document.querySelector("#chip-popover") ? "is still open" : "closed"}, the chip says ${closed ? label() : `${label()} (was ${before})`}`);
  };
  const type = (el, value) => {
    const proto = el instanceof HTMLTextAreaElement ? HTMLTextAreaElement : HTMLInputElement;
    Object.getOwnPropertyDescriptor(proto.prototype, "value").set.call(el, value);
    el.dispatchEvent(new Event("input", { bubbles: true }));
    el.dispatchEvent(new Event("change", { bubbles: true }));
  };
  // The window the first one opened (Open in new window), on ?session=.
  const opened = new URLSearchParams(location.search).get("session");
  const desktop = () =>
    ["prigh.desktop_key", "prigh.desktop_app"].some(k => sessionStorage.getItem(k) !== null);
  const openAnother = async composer => {
    localStorage.removeItem("probe-window");
    type(composer, "hello");
    composer.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", code: "Enter", bubbles: true }));
    const session = await until(() => document.querySelector(".sessions .session-open"), 15000);
    if (!session) return report("window: no session in the sidebar");
    session.dispatchEvent(new MouseEvent("click", { ctrlKey: true, bubbles: true, cancelable: true }));
    const seen = await until(() => localStorage.getItem("probe-window"), 30000);
    await report(`window: the other window: ${seen || "nothing within 30s"}`);
    // It has closed by now, or is closing: this window, and the app, go on.
    await until(() => false, 1000);
    await report(`window: the first window still works: ${document.querySelector(".composer textarea") !== null}`);
  };
  const anotherWindow = async () => {
    const composer = await until(() => document.querySelector(".composer textarea"), 15000);
    const seen = `on ?session=, desktop mode ${desktop()}, composer ${composer !== null}`;
    localStorage.setItem("probe-window", seen);
    location.href = "about:blank#prigh-desktop-test-close";
  };
  if (opened) {
    anotherWindow();
    return;
  }
  let done = false;
  let submitted = false;
  let signedIn = false;
  const started = Date.now();
  let stuck = false;
  const step = () => {
    if (done) return;
    if (!stuck && Date.now() - started > 30000) {
      stuck = true;
      report(`stuck at ${location.pathname}: ${seen()}; it shows: ${document.body.innerText.trim().replace(/\s+/g, " ").slice(0, 300)}`);
    }
    const form = document.querySelector("#desktop-form");
    const composer = document.querySelector(".composer textarea");
    if (form && !submitted) {
      done = true;
      submitted = true;
      const heading = document.querySelector(".signin h1, .signin h2");
      report(`connect form: ${heading ? heading.innerText.trim() : "(no heading)"}`).then(() => {
        if ("@MODE@" === "close") {
          location.href = "about:blank#prigh-desktop-test-close";
          return;
        }
        type(document.querySelector("#address"), "@ADDRESS@");
        type(document.querySelector("#password"), "sekrit");
        document.querySelector("#desktop-connect").click();
        done = false;
        setTimeout(step, 100);
      });
    } else if (form && document.querySelector(".signin-error")?.innerText.trim()) {
      done = true;
      report(`the form says: ${document.querySelector(".signin-error").innerText.trim()}`);
    } else if (!form && !composer && !signedIn && document.querySelector("form input[type=password]")) {
      // The backend's own prigh-web (-ui remote): its sign-in.
      done = true;
      signedIn = true;
      const where = location.host === "@ADDRESS@" ? "the backend" : location.host;
      report(`the backend's sign-in, on ${where}`).then(() => {
        type(document.querySelector("form input[type=password]"), "sekrit");
        document.querySelector("form button[type=submit]").click();
        done = false;
        setTimeout(step, 100);
      });
    } else if (composer) {
      done = true;
      // The window may close before anything sent after the /quit arrives.
      report("signed in").then(async () => {
        if ("@MODE@" === "menu") await chooseFromMenu();
        if ("@MODE@" === "window") await openAnother(composer);
        type(composer, "/quit");
        composer.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", code: "Enter", bubbles: true }));
      });
    }
    if (!done) setTimeout(step, 100);
  };
  step();
})();
