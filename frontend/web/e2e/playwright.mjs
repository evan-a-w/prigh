// Playwright for the e2e scripts: prigh_web.sh points PLAYWRIGHT_MODULE here
// (the real one in PRIGH_E2E_PLAYWRIGHT). Every browser context it makes
// blocks service workers unless the script asks for them ({ serviceWorkers:
// "allow" }, as pwa.mjs does): the worker caching every request is about a
// third of a page load's CPU, and only the script that tests it needs it.
// E2E_SERVICE_WORKERS changes the default. (Not reduced motion: the page
// behaves differently with it, e.g. how far the arrow keys scroll.)
const real = await import(process.env.PRIGH_E2E_PLAYWRIGHT);

const defaults = { serviceWorkers: process.env.E2E_SERVICE_WORKERS || "block" };

const patched = new WeakSet();
const patch = browser => {
  const proto = Object.getPrototypeOf(browser);
  if (!patched.has(proto)) {
    patched.add(proto);
    // Browser.newPage makes its context with newContext.
    const newContext = proto.newContext;
    proto.newContext = function (options = {}) {
      return newContext.call(this, { ...defaults, ...options });
    };
  }
  return browser;
};
for (const type of [real.chromium, real.firefox, real.webkit]) {
  const launch = type.launch.bind(type);
  type.launch = async (...args) => patch(await launch(...args));
}

export const { chromium, firefox, webkit, devices, errors, request, selectors } = real;
export default real;
