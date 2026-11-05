#!/usr/bin/env bash
# The desktop shell against a fake `prigh desktop` (fake-sidecar.py), in a
# real window: prints what each scenario did, to diff with expected.txt.
#
#   desktop/test/run.sh PRIGH_DESKTOP_BINARY [PRIGH]
#
# REAL_CHROMIUM, if set, is a Chromium for the browser window scenarios
# (with curl). Needs a display (run it under xvfb-run), a D-Bus session bus (for the
# single-instance plugin: dbus-run-session) and python3. The flake's
# desktop-window check runs it that way.
set -uo pipefail
app=$1
real=${2:-}
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
export HOME="$work/home" XDG_CONFIG_HOME="$work/config" XDG_DATA_HOME="$work/data"
mkdir -p "$HOME"
fake="$work/bin/fake-prigh"
mkdir -p "$work/bin"
cp "$here/fake-sidecar.py" "$fake"
chmod +x "$fake"
mkdir -p "$work/browsers"
cp "$here/fake-browser.py" "$work/browsers/google-chrome-stable"
chmod +x "$work/browsers/google-chrome-stable"
export PRIGH_DESKTOP_NO_DIALOG=1
# dbus-run-session gives two addresses, each with its server's GUID; zbus
# (the single-instance plugin) pairs the first with the wrong GUID and goes
# without. A desktop session's bus has one.
export DBUS_SESSION_BUS_ADDRESS="${DBUS_SESSION_BUS_ADDRESS%%;*}"
# The app's own window unless a scenario says otherwise (`auto` would take
# a browser installed here).
export PRIGH_DESKTOP_WINDOW=webview

# scenario NAME [VAR=VALUE...]: runs the app with them, prints its exit
# code, its own log lines and the fake's events, ports and paths replaced.
scenario() {
	local name=$1
	shift
	: >"$work/log"
	: >"$work/browser-log"
	env FAKE_LOG="$work/log" FAKE_BROWSER_LOG="$work/browser-log" PRIGH_DESKTOP_BACKEND="$fake" "$@" \
		timeout 60 "$app" >"$work/stdout" 2>"$work/stderr"
	local code=$?
	echo "== $name: exit $code"
	# Without CLOSE_ON_LOAD the sidecar may end the app before the load is
	# logged.
	local unsure='^prigh-desktop: loaded '
	case " $* " in *" PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1 "*) unsure='/favicon.ico' ;; esac
	normalize <"$work/stderr" | grep -E '^prigh-desktop: ' | grep -v -e '/favicon.ico' -e "$unsure"
	normalize <"$work/log" | grep -v 'GET /favicon.ico' | sed 's/^/fake: /'
	normalize <"$work/browser-log"
	return 0
}
normalize() {
	sed -E -e "s#$work#WORK#g" -e "s#${real:-/nonexistent}#PRIGH#g" -e 's#127\.0\.0\.1:[0-9]+#127.0.0.1:PORT#g'  -e 's#localhost:[0-9]+#localhost:PORT#g' -e 's#serving on [0-9]+#serving on PORT#'
}

scenario "the window shows the page; closing it stops the sidecar" \
	PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1 PRIGH_DESKTOP_ARGS="-cwd '/a b' -name laptop"
scenario "the page's Quit: the sidecar exits 0, so does the app" FAKE_MODE=quit
scenario "the backend's own site (-ui remote), quitting from it" FAKE_MODE=remote
scenario "the sidecar fails after it was ready" FAKE_MODE=crash
scenario "the sidecar fails before it is ready" FAKE_MODE=fail
scenario "the sidecar prints something else" FAKE_MODE=bad-ready
scenario "the sidecar ignores its stdin closing: terminated" \
	FAKE_MODE=ignore-eof PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1
scenario "the sidecar ignores SIGTERM too: killed" \
	FAKE_MODE=ignore-term PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1
scenario "no such prigh" PRIGH_DESKTOP_BACKEND=prigh-not-installed
scenario "bad PRIGH_DESKTOP_ARGS" PRIGH_DESKTOP_ARGS="-cwd 'x"

# The page in a browser's app window (fake-browser.py, standing in for
# Chromium), not the app's own.
browser="$work/browsers/google-chrome-stable"
scenario "browser: auto takes one on the PATH; closing its window stops the sidecar" \
	PRIGH_DESKTOP_WINDOW= PATH="$work/browsers:$PATH"
scenario "browser: the page's Quit (the sidecar exits) closes the browser" \
	PRIGH_DESKTOP_WINDOW=browser PRIGH_DESKTOP_BROWSER="$browser" FAKE_MODE=quit FAKE_BROWSER_MODE=stay
scenario "browser: the sidecar fails: the browser closes, the failure is told" \
	PRIGH_DESKTOP_WINDOW=browser PRIGH_DESKTOP_BROWSER="$browser" FAKE_MODE=crash FAKE_BROWSER_MODE=stay
scenario "browser: none installed" PRIGH_DESKTOP_WINDOW=browser
scenario "browser: PRIGH_DESKTOP_BROWSER is not one" \
	PRIGH_DESKTOP_WINDOW=browser PRIGH_DESKTOP_BROWSER=/no/such/chrome
scenario "bad PRIGH_DESKTOP_WINDOW" PRIGH_DESKTOP_WINDOW=chrome

# wait_for FILE PATTERN
wait_for() {
	for _ in $(seq 300); do
		grep -q -- "$2" "$1" 2>/dev/null && return 0
		sleep 0.1
	done
	echo "timed out waiting for $2 in $1"
}
# browser_app [VAR=VALUE...]: the app, in the background, in browser mode.
browser_app() {
	env FAKE_LOG="$work/log" FAKE_BROWSER_LOG="$work/browser-log" PRIGH_DESKTOP_BACKEND="$fake" \
		PRIGH_DESKTOP_WINDOW=browser PRIGH_DESKTOP_BROWSER="$browser" "$@" \
		timeout 60 "$app" >"$work/stdout" 2>"$work/stderr" &
	app_pid=$!
}
report() {
	echo "== $1: exit $2"
	normalize <"$work/stderr" | grep -E '^prigh-desktop: ' | grep -v '/favicon.ico'
	normalize <"$work/log" | grep -v 'GET /favicon.ico' | sed 's/^/fake: /'
	normalize <"$work/browser-log"
}
profile="$XDG_DATA_HOME/io.github.evan-a-w.prigh/browser"

# An instance left on prigh's profile (by an app that was killed) takes the
# window: the app waits for that instance instead.
: >"$work/log"
: >"$work/browser-log"
FAKE_BROWSER_LOG="$work/browser-log" FAKE_BROWSER_MODE=stay "$browser" \
	--app=http://127.0.0.1:9/ --user-data-dir="$profile" &
stale=$!
wait_for "$work/browser-log" "loaded the page"
browser_app FAKE_BROWSER_MODE=close
wait_for "$work/stderr" "handed the window"
kill -TERM "$stale"
wait "$stale"
wait "$app_pid"
report "browser: an instance already on the profile; its window closing stops the sidecar" $?

# A second launch opens another window in the running browser, and its
# launcher exiting at once does not end the app.
: >"$work/log"
: >"$work/browser-log"
browser_app FAKE_BROWSER_MODE=close-later FAKE_BROWSER_CLOSE="$work/close"
wait_for "$work/browser-log" "loaded the page"
env FAKE_LOG="$work/log2" FAKE_BROWSER_LOG="$work/browser-log" PRIGH_DESKTOP_WINDOW=browser \
	PRIGH_DESKTOP_BROWSER="$browser" PRIGH_DESKTOP_BACKEND="$fake" timeout 30 "$app" >/dev/null 2>"$work/stderr2"
second=$?
wait_for "$work/browser-log" "handed the window"
sleep 0.5
running=$(kill -0 "$app_pid" 2>/dev/null && echo yes || echo no)
touch "$work/close"
wait "$app_pid"
code=$?
echo "second launch: exit $second; the first still running after it: $running"
report "browser: a second launch, then the first window closes" $code

# A real Chromium ($REAL_CHROMIUM; the check gives Playwright's), its windows
# closed through its DevTools as a user would close them (Xvfb has no window
# manager to ask).
if [ -n "${REAL_CHROMIUM:-}" ]; then
	printf '#!/bin/sh\nexec "%s" --remote-debugging-port=9333 --no-sandbox "$@"\n' "$REAL_CHROMIUM" >"$work/chromium"
	chmod +x "$work/chromium"
	rm -rf "$profile"
	pages() {
		curl -s http://127.0.0.1:9333/json/list |
			python3 -c 'import json, sys; print(" ".join(t["id"] for t in json.load(sys.stdin) if t["type"] == "page"))'
	}
	real_report() {
		echo "== $1: exit $2"
		normalize <"$work/stderr" | grep -E '^prigh-desktop: ' | grep -v '/favicon.ico'
		normalize <"$work/log" | grep -v -e 'GET /favicon.ico' -e '^args' -e '^cwd' | LC_ALL=C sort | uniq -c | sed -E 's/^ +/fake: /'
		echo "left running: $(ps -eo args= | grep -F -- "--user-data-dir=$profile" | grep -vc grep)"
	}
	: >"$work/log"
	browser_app PRIGH_DESKTOP_BROWSER="$work/chromium"
	wait_for "$work/log" "GET /pixel.gif"
	env FAKE_LOG="$work/log2" PRIGH_DESKTOP_WINDOW=browser PRIGH_DESKTOP_BROWSER="$work/chromium" \
		PRIGH_DESKTOP_BACKEND="$fake" timeout 30 "$app" >/dev/null 2>&1
	second=$?
	for _ in $(seq 100); do [ "$(pages | wc -w)" = 2 ] && break; sleep 0.1; done
	echo "second launch: exit $second; windows: $(pages | wc -w)"
	for id in $(pages); do curl -s -o /dev/null "http://127.0.0.1:9333/json/close/$id"; sleep 0.5; done
	wait "$app_pid"
	real_report "real Chromium: two windows; closing both stops the sidecar" $?
	: >"$work/log"
	env FAKE_LOG="$work/log" PRIGH_DESKTOP_BACKEND="$fake" PRIGH_DESKTOP_WINDOW=browser \
		PRIGH_DESKTOP_BROWSER="$work/chromium" FAKE_MODE=quit timeout 60 "$app" >"$work/stdout" 2>"$work/stderr"
	code=$?
	sleep 0.5
	real_report "real Chromium: the sidecar quits, Chromium closes" $code
fi

# The real `prigh desktop` (run.sh's second argument: a prigh with its
# prigh-web site, as the flake's wrapper or PRIGH_WEB_ROOT gives it) for a
# `prigh serve -web` backend with a scripted model. The window's page is
# driven by probe.js, which reports what it shows to a small server.

[ -n "$real" ] || exit 0
mkdir -p "$work/server-home" "$work/laptop-cwd"
HOME="$work/server-home" "$real" serve -web 127.0.0.1:0 -faux -token sekrit \
	-cwd "$work/server-home" 2>"$work/server.err" &
server=$!
report_port_file="$work/report-port"
python3 - "$work/reports" "$report_port_file" <<'PY' &
import http.server, sys
out, port_file = sys.argv[1], sys.argv[2]
class H(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get("Content-Length", 0))).decode()
        with open(out, "a") as f:
            f.write(body + "\n")
        self.send_response(204)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
    def log_message(self, *a):
        pass
s = http.server.HTTPServer(("127.0.0.1", 0), H)
open(port_file, "w").write(str(s.server_port))
s.serve_forever()
PY
reporter=$!
trap 'kill $server $reporter 2>/dev/null; rm -rf "$work"' EXIT
for _ in $(seq 300); do
	address=$(sed -nE 's#^prigh: prigh-web on http://([^/]+)/$#\1#p' "$work/server.err")
	[ -n "$address" ] && [ -s "$report_port_file" ] && break
	sleep 0.1
done
[ -n "$address" ] || { echo "the backend did not start:"; cat "$work/server.err"; exit 1; }

# real NAME MODE UI: a fresh laptop home (no saved connection) for MODE
# close, the same one again for quit.
real_scenario() {
	local name=$1 mode=$2 ui=$3
	: >"$work/reports"
	sed -e "s#@REPORT@#http://127.0.0.1:$(cat "$report_port_file")/#" \
		-e "s#@ADDRESS@#$address#" -e "s#@MODE@#$mode#" "$here/probe.js" >"$work/probe.js"
	env PRIGH_DESKTOP_BACKEND="$real" PRIGH_DESKTOP_TEST_SCRIPT="$work/probe.js" \
		PRIGH_DESKTOP_ARGS="-ui $ui -cwd '$work/laptop-cwd' -name laptop -host-id test-desktop-laptop -listen 127.0.0.1:0" \
		timeout 90 "$app" >"$work/stdout" 2>"$work/stderr"
	local code=$?
	echo "== $name: exit $code"
	normalize <"$work/stderr" | grep -E '^prigh-desktop: ' |
		grep -v -e '/favicon.ico' -e '^prigh-desktop: loaded ' |
		sed -E 's#(ready at http://127.0.0.1:PORT/).*#\1#; s#(prigh-web at http://127.0.0.1:PORT/).*#\1#'
	normalize <"$work/stderr" | grep -E '^prigh desktop: |^prigh: ' | grep -E 'quit|stdin|connected|tools run' |
		sed 's/^/sidecar: /'
	sed 's/^/page: /' "$work/reports"
	echo "saved connection: $([ -s "$HOME/.prigh/tui-connection.sexp" ] && echo yes || echo no)"
	# Nothing of it is left: not the sidecar, not its tool host.
	local left
	left=$(ps -eo args= | grep -F -- "$work/laptop-cwd" | grep -v -e grep -e '^timeout' || true)
	echo "left running: ${left:-nothing}"
}
real_scenario "the real sidecar: the connect form; closing the window stops it" close local
real_scenario "the real sidecar: signed in with the form; the page's /quit ends the app" quit local
# Saved by the one before: straight to the backend's own prigh-web.
real_scenario "the real sidecar, -ui remote: the backend's own site; its /quit ends the app" quit remote
