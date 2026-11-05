#!/usr/bin/env bash
# The desktop shell against a fake `prigh desktop` (fake-sidecar.py), in a
# real window: prints what each scenario did, to diff with expected.txt.
#
#   desktop/test/run.sh PRIGH_DESKTOP_BINARY [PRIGH]
#
# Needs a display (run it under xvfb-run), a D-Bus session bus (for the
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
export PRIGH_DESKTOP_NO_DIALOG=1

# scenario NAME [VAR=VALUE...]: runs the app with them, prints its exit
# code, its own log lines and the fake's events, ports and paths replaced.
scenario() {
	local name=$1
	shift
	: >"$work/log"
	env FAKE_LOG="$work/log" PRIGH_DESKTOP_BACKEND="$fake" "$@" \
		timeout 60 "$app" >"$work/stdout" 2>"$work/stderr"
	local code=$?
	echo "== $name: exit $code"
	# Without CLOSE_ON_LOAD the sidecar may end the app before the load is
	# logged.
	local unsure='^prigh-desktop: loaded '
	case " $* " in *" PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1 "*) unsure='/favicon.ico' ;; esac
	normalize <"$work/stderr" | grep -E '^prigh-desktop: ' | grep -v -e '/favicon.ico' -e "$unsure"
	normalize <"$work/log" | grep -v 'GET /favicon.ico' | sed 's/^/fake: /'
	return 0
}
normalize() {
	sed -E -e "s#$work#WORK#g" -e "s#${real:-/nonexistent}#PRIGH#g" -e 's#127\.0\.0\.1:[0-9]+#127.0.0.1:PORT#g' -e 's#serving on [0-9]+#serving on PORT#'
}

scenario "the window shows the page; closing it stops the sidecar" \
	PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1 PRIGH_DESKTOP_ARGS="-cwd '/a b' -name laptop"
scenario "the page's Quit: the sidecar exits 0, so does the app" FAKE_MODE=quit
scenario "the sidecar fails after it was ready" FAKE_MODE=crash
scenario "the sidecar fails before it is ready" FAKE_MODE=fail
scenario "the sidecar prints something else" FAKE_MODE=bad-ready
scenario "the sidecar ignores its stdin closing: terminated" \
	FAKE_MODE=ignore-eof PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1
scenario "the sidecar ignores SIGTERM too: killed" \
	FAKE_MODE=ignore-term PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1
scenario "no such prigh" PRIGH_DESKTOP_BACKEND=prigh-not-installed
scenario "bad PRIGH_DESKTOP_ARGS" PRIGH_DESKTOP_ARGS="-cwd 'x"

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

# real NAME MODE: a fresh laptop home (no saved connection) for MODE close,
# the same one again for quit.
real_scenario() {
	local name=$1 mode=$2
	: >"$work/reports"
	sed -e "s#@REPORT@#http://127.0.0.1:$(cat "$report_port_file")/#" \
		-e "s#@ADDRESS@#$address#" -e "s#@MODE@#$mode#" "$here/probe.js" >"$work/probe.js"
	env PRIGH_DESKTOP_BACKEND="$real" PRIGH_DESKTOP_TEST_SCRIPT="$work/probe.js" \
		PRIGH_DESKTOP_ARGS="-cwd '$work/laptop-cwd' -name laptop -host-id test-desktop-laptop -listen 127.0.0.1:0" \
		timeout 90 "$app" >"$work/stdout" 2>"$work/stderr"
	local code=$?
	echo "== $name: exit $code"
	normalize <"$work/stderr" | grep -E '^prigh-desktop: ' |
		grep -v -e '/favicon.ico' -e '^prigh-desktop: loaded ' |
		sed -E 's#(ready at http://127.0.0.1:PORT/).*#\1#'
	normalize <"$work/stderr" | grep -E '^prigh desktop: |^prigh: ' | grep -E 'quit|stdin|connected|tools run' |
		sed 's/^/sidecar: /'
	sed 's/^/page: /' "$work/reports"
	echo "saved connection: $([ -s "$HOME/.prigh/tui-connection.sexp" ] && echo yes || echo no)"
	# Nothing of it is left: not the sidecar, not its tool host.
	local left
	left=$(ps -eo args= | grep -F -- "$work/laptop-cwd" | grep -v -e grep -e '^timeout' || true)
	echo "left running: ${left:-nothing}"
}
real_scenario "the real sidecar: the connect form; closing the window stops it" close
real_scenario "the real sidecar: signed in with the form; the page's /quit ends the app" quit
