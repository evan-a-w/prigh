#!/usr/bin/env python3
"""A stand-in for `prigh desktop`, following its contract with the shell:
one JSON ready line on stdout, logs on stderr, exit 0 on stdin EOF.

$FAKE_MODE picks a behaviour:
  serve        serve a page until stdin closes (default)
  quit         exit 0 once the page has loaded, as the page's Quit does
  crash        exit 4 once the page has loaded
  fail         print an error and exit 3 before the ready line
  bad-ready    print something that is not a ready line
  ignore-eof   keep running after stdin closes (the shell must kill it)
What happens is appended to $FAKE_LOG, one event per line.
"""

import http.server
import json
import os
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
        if self.path == "/pixel.gif" and mode in ("quit", "crash"):
            self.wfile.flush()
            log("exiting " + mode)
            if mode == "crash":
                print("prigh desktop: the backend went away", file=sys.stderr, flush=True)
            os._exit(0 if mode == "quit" else 4)

    def log_message(self, fmt, *args):
        pass


server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
threading.Thread(target=server.serve_forever, daemon=True).start()
port = server.server_address[1]
print(json.dumps({"url": f"http://127.0.0.1:{port}/#signin=one-time-code"}), flush=True)
print(f"fake sidecar: serving on {port}", file=sys.stderr, flush=True)

sys.stdin.read()
log("eof")
if mode == "ignore-eof":
    threading.Event().wait()
sys.exit(0)
