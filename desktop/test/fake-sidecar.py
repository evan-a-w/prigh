#!/usr/bin/env python3
"""A stand-in for `prigh desktop`, following its contract with the shell:
one JSON ready line on stdout, logs on stderr, exit 0 on stdin EOF.

$FAKE_MODE picks a behaviour:
  serve        serve a page until stdin closes (default)
  quit         exit 0 once the page has loaded, as the page's Quit does
  remote       once the page has loaded, a second page line: the backend's own
               site (another origin), which quits through the quit URL
  crash        exit 4 once the page has loaded
  fail         print an error and exit 3 before the ready line
  bad-ready    print something that is not a ready line
  ignore-eof   keep running after stdin closes (the shell terminates it)
  ignore-term  ... and after SIGTERM too (the shell must kill it)
  proxy        remote, the backend's site behind a sign-in proxy (as
               authentik: a 302 to its login site, with a new state each
               time; there a form posted, a redirect through a callback on
               the login site to one on the backend's site, which sets the
               proxy's cookie and goes back to the page); the page line is
               a plain remote one, as for a site that needs no sign-in
  proxy-browser  the same site; once the page has loaded, as if the user
               chose "Sign in with your browser": an external page line
               with a sign-in listener's URL, whose grant (posted by the
               default browser) gives the remote page line with the cookie.
               The site takes the cookie for one page only: reloaded, its
               page then sends the window back to the login, and the shell
               shows the sidecar's sign-in page, where the fake quits
With $FAKE_PAGE_LINKS set, the page, once, opens a window on another site
(http://localhost:PORT/new-window: another origin, the same server) and
then goes to another site itself (/away), as links in prigh-web would.
With $FAKE_PAGE_POPUP set, the page, once, opens a popup on its own site
(/?popup=1), as prigh-web's Open in new window does in a browser's app
window.
What happens is appended to $FAKE_LOG, one event per line.
"""

import http.server
import json
import os
import signal
import sys
import threading
import urllib.parse

mode = os.environ.get("FAKE_MODE", "serve")
log_path = os.environ["FAKE_LOG"]
log_lock = threading.Lock()


def log(event):
    with log_lock, open(log_path, "a") as f:
        f.write(event + "\n")


log("args " + " ".join(sys.argv[1:]))
log("cwd " + os.getcwd())

if mode == "fail":
    print("prigh desktop: -connect: cannot parse 'nowhere'", file=sys.stderr)
    print("Give HOST:PORT or a prigh-web URL.", file=sys.stderr)
    sys.exit(3)
if mode == "bad-ready":
    print("listening on 7790", flush=True)
    sys.stdin.read()
    sys.exit(0)

PAGE = b"""<!doctype html><html><head><title>fake prigh-web</title></head>
<body><p>fake prigh-web</p><img src="/pixel.gif" alt=""></body></html>"""
if os.environ.get("FAKE_PAGE_LINKS"):
    PAGE = PAGE.replace(b"</body>", b"""<script>
addEventListener("load", () => {
  if (location.hostname === "localhost" || sessionStorage.getItem("went")) return;
  sessionStorage.setItem("went", "1");
  const elsewhere = "http://localhost:" + location.port;
  window.open(elsewhere + "/new-window", "_blank");
  setTimeout(() => { location.href = elsewhere + "/away"; }, 1000);
});
</script></body>""")
if os.environ.get("FAKE_PAGE_POPUP"):
    PAGE = PAGE.replace(b"</body>", b"""<script>
addEventListener("load", () => {
  if (location.search) document.title = "fake prigh-web, another window";
  if (location.search || sessionStorage.getItem("opened")) return;
  sessionStorage.setItem("opened", "1");
  window.open("/?popup=1#shell=browser", "_blank", "popup,width=600,height=400");
});
</script></body>""")
if mode == "proxy":
    # In a browser's app window the page goes to the backend's site itself
    # (as prigh-web's does), once the shell has been told (it asks first).
    PAGE = PAGE.replace(b"</body>", b"""<script>
addEventListener("load", async () => {
  if (!location.hash.includes("shell=browser")) return;
  location.href = await (await fetch("/remote")).text();
});
</script></body>""")
if mode == "proxy-browser":
    PAGE = PAGE.replace(b"</body>", b"""<script>
fetch("/hash?" + encodeURIComponent(location.hash));
addEventListener("hashchange", () => fetch("/hash?" + encodeURIComponent(location.hash)));
</script></body>""")
# The smallest GIF: the page's load waits for it, so a load the shell sees
# means the webview fetched it.
PIXEL = bytes.fromhex(
    "47494638396101000100800000000000ffffff21f90401000000002c000000000100010000020144003b"
)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path.startswith("/hash?"):
            hash_ = urllib.parse.unquote(self.path[len("/hash?"):])
            self.send_response(204)
            self.end_headers()
            if "signin=1" in hash_:
                log(f"the page is at {hash_}")
                if not os.environ.get("FAKE_SIGNIN_STAY"):
                    log("exiting: the sign-in page")
                    os._exit(0)
            return
        if self.path == "/remote":
            remote_line_printed.wait()
            url = f"http://localhost:{site.server_address[1]}/#app=desktop&host=laptop".encode()
            return respond(self, 200, url)
        log(f"GET {self.path} webkit={'AppleWebKit' in self.headers.get('User-Agent', '')}")
        body, ctype = (PIXEL, "image/gif") if self.path == "/pixel.gif" else (PAGE, "text/html")
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)
        if self.path == "/pixel.gif" and mode in ("remote", "proxy") and not remote_shown.is_set():
            remote_shown.set()
            port = (site if mode == "proxy" else remote).server_address[1]
            print(json.dumps({"url": f"http://localhost:{port}/#app=desktop&host=laptop", "remote": True}), flush=True)
            remote_line_printed.set()
        if self.path == "/pixel.gif" and mode == "proxy-browser" and not remote_shown.is_set():
            remote_shown.set()
            browser_signin()
        if self.path == "/pixel.gif" and mode in ("quit", "crash"):
            self.wfile.flush()
            log("exiting " + mode)
            if mode == "crash":
                print("prigh desktop: the backend went away", file=sys.stderr, flush=True)
            os._exit(0 if mode == "quit" else 4)

    def log_message(self, fmt, *args):
        pass


REMOTE_PAGE = b"""<!doctype html><html><head><title>the backend's prigh-web</title></head>
<body><p>the backend's prigh-web</p><script>
addEventListener("load", () => { location.href = "about:blank#prigh-desktop-quit"; });
</script></body></html>"""
remote_shown = threading.Event()
remote_line_printed = threading.Event()


class RemoteHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        log(f"backend GET {self.path}")
        self.send_response(200 if self.path == "/" else 404)
        self.send_header("Content-Type", "text/html")
        self.send_header("Content-Length", str(len(REMOTE_PAGE)))
        self.end_headers()
        self.wfile.write(REMOTE_PAGE)

    def log_message(self, fmt, *args):
        pass


# The backend's site behind a sign-in proxy, and the proxy's login site
# (another origin: 127.0.0.1, the site is localhost).
SITE_PAGE = b"""<!doctype html><html><head><title>the backend's prigh-web</title></head>
<body><p>the backend's prigh-web, signed in</p><script>
addEventListener("load", () => {
  // (In a browser's app window the test closes it.)
  if (QUIT) { if (!/Chrome/.test(navigator.userAgent)) location.href = "about:blank#prigh-desktop-quit"; return; }
  if (!sessionStorage.getItem("reloaded")) { sessionStorage.setItem("reloaded", "1"); location.reload(); }
});
</script></body></html>""".replace(b"QUIT", b"true" if mode == "proxy" else b"false")
LOGIN_PAGE = b"""<!doctype html><html><head><title>sign in</title></head>
<body><form method="post" action="/login"><input name="user" value="alice"></form>
<script>addEventListener("load", () => document.forms[0].submit());</script></body></html>"""
signed_in_pages = [0]
states = [0]


def respond(h, code, body=b"", headers=()):
    h.send_response(code)
    for k, v in headers:
        h.send_header(k, v)
    h.send_header("Content-Type", "text/html")
    h.send_header("Content-Length", str(len(body)))
    h.send_header("Cache-Control", "no-store")
    h.end_headers()
    h.wfile.write(body)


class SiteHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/favicon.ico":
            return respond(self, 404)
        cookie = self.headers.get("Cookie") or ""
        if self.path.startswith("/outpost/callback?"):
            log(f"site: {self.path}: sets the cookie")
            return respond(self, 302, headers=[("Location", "/"), ("Set-Cookie", "proxy=ok; Path=/; HttpOnly")])
        good = "proxy=ok" in cookie and (mode == "proxy" or signed_in_pages[0] == 0)
        if good:
            signed_in_pages[0] += 1
            log(f"site: GET {self.path} with the cookie: the page")
            return respond(self, 200, SITE_PAGE)
        states[0] += 1
        log(f"site: GET {self.path}{' with an expired cookie' if 'proxy=ok' in cookie else ''}: to the login")
        login_url = f"http://127.0.0.1:{login.server_address[1]}/start?state={states[0]}&next=/authorize"
        respond(self, 302, headers=[("Location", login_url)])

    def log_message(self, fmt, *args):
        pass


class LoginHandler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path.startswith("/start?"):
            log(f"login: GET {self.path}")
            return respond(self, 200, LOGIN_PAGE)
        if self.path == "/callback":
            log("login: GET /callback: back to the site")
            back = f"http://localhost:{site.server_address[1]}/outpost/callback?code=c0de"
            return respond(self, 302, headers=[("Location", back)])
        respond(self, 404)

    def do_POST(self):
        self.rfile.read(int(self.headers.get("Content-Length", 0)))
        log(f"login: POST {self.path}")
        respond(self, 302, headers=[("Location", "/callback")])

    def log_message(self, fmt, *args):
        pass


class GrantHandler(http.server.BaseHTTPRequestHandler):
    """Browser_signin's listener: the default browser's Allow posts here."""

    def do_POST(self):
        form = urllib.parse.parse_qs(self.rfile.read(int(self.headers.get("Content-Length", 0))).decode())
        ok = form.get("state") == ["5747e"]
        log(f"grant: state {'right' if ok else 'wrong'}, cookie {form.get('proxy_cookie')}")
        respond(self, 200, b"signed in")
        if ok:
            local = f"http://127.0.0.1:{port}/#desktop=0123abcd"
            line = {
                "url": f"http://localhost:{site.server_address[1]}/#app=desktop&host=laptop",
                "remote": True,
                "cookie": form["proxy_cookie"][0],
                "signin": local + "&signin=1",
            }
            print(json.dumps(line), flush=True)

    def log_message(self, fmt, *args):
        pass


def browser_signin():
    grants = http.server.ThreadingHTTPServer(("127.0.0.1", 0), GrantHandler)
    threading.Thread(target=grants.serve_forever, daemon=True).start()
    url = f"http://localhost:{site.server_address[1]}/#cli-signin={grants.server_address[1]}.5747e&program=desktop"
    log("signing in through the browser")
    print(json.dumps({"url": url, "external": True}), flush=True)


site = http.server.ThreadingHTTPServer(("127.0.0.1", 0), SiteHandler)
threading.Thread(target=site.serve_forever, daemon=True).start()
login = http.server.ThreadingHTTPServer(("127.0.0.1", 0), LoginHandler)
threading.Thread(target=login.serve_forever, daemon=True).start()
remote = http.server.ThreadingHTTPServer(("127.0.0.1", 0), RemoteHandler)
threading.Thread(target=remote.serve_forever, daemon=True).start()
server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
threading.Thread(target=server.serve_forever, daemon=True).start()
port = server.server_address[1]
print(json.dumps({"url": f"http://127.0.0.1:{port}/#desktop=0123abcd"}), flush=True)
print(f"fake sidecar: serving on {port}", file=sys.stderr, flush=True)

sys.stdin.read()
log("eof")
if mode == "ignore-eof":
    signal.signal(signal.SIGTERM, lambda *_: (log("SIGTERM"), os._exit(0)))
if mode == "ignore-term":
    signal.signal(signal.SIGTERM, lambda *_: log("SIGTERM ignored"))
if mode in ("ignore-eof", "ignore-term"):
    threading.Event().wait()
sys.exit(0)
