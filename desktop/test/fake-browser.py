#!/usr/bin/env python3
"""A stand-in for a Chromium-family browser's app window, as the desktop
shell runs it (`--app=URL --user-data-dir=PROFILE ...`). Like Chromium, a
launch on a profile that an instance holds (its SingletonLock, a symlink to
HOST-PID) hands the window to that instance and exits; otherwise it takes
the lock, "loads" the page (GETs it and its image, as a browser would), and
then, by $FAKE_BROWSER_MODE:
  close        its window is closed at once (exits 0) (default)
  stay         stays open until SIGTERM
  close-later  stays open until $FAKE_BROWSER_CLOSE exists
What happens is appended to $FAKE_BROWSER_LOG, one event per line.
"""

import os
import signal
import socket
import sys
import time
import urllib.request

mode = os.environ.get("FAKE_BROWSER_MODE", "close")
log_path = os.environ["FAKE_BROWSER_LOG"]


def log(event):
    with open(log_path, "a") as f:
        f.write("browser: " + event + "\n")


args = sys.argv[1:]
log("args " + " ".join(args))
url = next(a[len("--app="):] for a in args if a.startswith("--app="))
profile = next(a[len("--user-data-dir="):] for a in args if a.startswith("--user-data-dir="))
lock = os.path.join(profile, "SingletonLock")

try:
    pid = int(os.readlink(lock).rsplit("-", 1)[1])
    os.kill(pid, 0)
    log("handed the window to the instance on the profile")
    sys.exit(0)
except (OSError, ValueError, IndexError):
    pass
os.makedirs(profile, exist_ok=True)
if os.path.lexists(lock):
    os.remove(lock)
os.symlink(f"{socket.gethostname()}-{os.getpid()}", lock)


def leave(code):
    if os.path.lexists(lock) and os.readlink(lock).endswith(f"-{os.getpid()}"):
        os.remove(lock)
    sys.exit(code)


def on_term(*_):
    log("SIGTERM: closing")
    leave(0)


signal.signal(signal.SIGTERM, on_term)

base = url.split("#")[0]
log("fragment " + (url.split("#", 1)[1] if "#" in url else ""))
for path in ("", "pixel.gif"):
    try:
        urllib.request.urlopen(base + path, timeout=10).read()
    except OSError:  # the sidecar may exit as the image is answered
        pass
log("loaded the page")

if mode == "close":
    log("window closed")
    leave(0)
if mode == "close-later":
    while not os.path.exists(os.environ["FAKE_BROWSER_CLOSE"]):
        time.sleep(0.1)
    log("window closed")
    leave(0)
while True:
    signal.pause()
