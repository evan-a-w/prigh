// prigh-web's service worker: the app shell offline, nothing else.
//
// The build (sw_gen/, site/dune) fills in VERSION, a hash of the shell's
// files, and SHELL, their names: a new build is a new worker, with a cache
// of its own, which waits until the page reloads into it (the page offers
// to). Only the shell's files are answered here; everything else (the RPC
// and terminal WebSockets, file transfers, the sign-in link, anything
// unknown) goes to the network untouched.
const VERSION = "%%VERSION%%";
const SHELL = %%SHELL%%;

const PREFIX = "prigh-web-";
const CACHE = PREFIX + VERSION;
const scope = new URL(self.registration.scope);
const shellUrls = new Set(SHELL.map(name => new URL(name, scope).href));
const index = new URL("index.html", scope).href;
// The page: what a navigation to the scope's root or index.html loads.
const pages = new Set([scope.href, index]);

self.addEventListener("install", event => {
  event.waitUntil(
    caches.open(CACHE).then(cache =>
      // Past the HTTP cache: these are this build's files.
      cache.addAll([...shellUrls].map(url => new Request(url, { cache: "reload" })))));
});

self.addEventListener("activate", event => {
  event.waitUntil((async () => {
    for (const key of await caches.keys())
      if (key.startsWith(PREFIX) && key !== CACHE) await caches.delete(key);
    await self.clients.claim();
  })());
});

// The page's Reload in its "new version" notice.
self.addEventListener("message", event => {
  if (event.data === "skip-waiting") self.skipWaiting();
});

// The page from the network, so that a new build's arrives; this build's
// when the network or the backend cannot be reached (an error page from a
// proxy, or its sign-in, is the network's answer and goes through).
const page = async request => {
  const timeout = new Promise((_, reject) => setTimeout(() => reject(new Error("timed out")), 10000));
  try {
    return await Promise.race([fetch(request), timeout]);
  } catch (e) {
    const cached = await caches.match(index, { cacheName: CACHE });
    if (cached) return cached;
    throw e;
  }
};

self.addEventListener("fetch", event => {
  const request = event.request;
  if (request.method !== "GET") return;
  const url = new URL(request.url);
  const bare = url.origin + url.pathname;
  if (request.mode === "navigate") {
    if (pages.has(bare)) event.respondWith(page(request));
    return;
  }
  if (url.search === "" && shellUrls.has(bare))
    event.respondWith(
      caches.match(request, { cacheName: CACHE }).then(cached => cached ?? fetch(request)));
});

// A notification shown through the worker (where pages cannot make their
// own: Android, an installed app on iOS): a click brings the page back.
self.addEventListener("notificationclick", event => {
  event.notification.close();
  event.waitUntil((async () => {
    const windows = await self.clients.matchAll({ type: "window", includeUncontrolled: true });
    const tab = event.notification.data?.url
      && windows.find(w => w.url === event.notification.data.url);
    const target = tab ?? windows[0];
    if (target) return target.focus();
    return self.clients.openWindow(event.notification.data?.url ?? scope.href);
  })());
});
