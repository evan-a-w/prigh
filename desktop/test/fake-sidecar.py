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
# The smallest GIF: the page's load waits for it, so a load the shell sees
# means the webview fetched it.
PIXEL = bytes.fromhex(
    "47494638396101000100800000000000ffffff21f90401000000002c000000000100010000020144003b"
)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        log(f"GET {self.path} webkit={'AppleWebKit' in self.headers.get('User-Agent', '')}")
        body, ctype = (PIXEL, "image/gif") if self.path == "/pixel.gif" else (PAGE, "text/html")
        self.send_response(200)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)
        if self.path == "/pixel.gif" and mode == "remote" and not remote_shown.is_set():
            remote_shown.set()
            print(
                json.dumps({"url": f"http://localhost:{remote.server_address[1]}/#app=desktop&host=laptop", "remote": True}),
                flush=True,
            )
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
