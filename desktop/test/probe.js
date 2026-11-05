// Run by the shell (PRIGH_DESKTOP_TEST_SCRIPT) on each of the real `prigh
// desktop`'s pages; run.sh fills in @REPORT@, @ADDRESS@ and @MODE@. It says
// what the page shows by POSTing to the report server and drives it: in
// "close" mode it closes the window once the connect form shows, in "quit"
// mode it signs in with the form and then quits with /quit.
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
  const type = (el, value) => {
    const proto = el instanceof HTMLTextAreaElement ? HTMLTextAreaElement : HTMLInputElement;
    Object.getOwnPropertyDescriptor(proto.prototype, "value").set.call(el, value);
    el.dispatchEvent(new Event("input", { bubbles: true }));
    el.dispatchEvent(new Event("change", { bubbles: true }));
  };
  let done = false;
  let submitted = false;
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
    } else if (composer) {
      done = true;
      report("signed in").then(() => {
        type(composer, "/quit");
        composer.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", code: "Enter", bubbles: true }));
        const quitting = () =>
          document.querySelector(".desktop-quitting")
            ? report("quitting")
            : setTimeout(quitting, 100);
        quitting();
      });
    }
    if (!done) setTimeout(step, 100);
  };
  step();
})();
