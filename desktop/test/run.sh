#!/usr/bin/env bash
# The desktop shell against a fake `prigh desktop` (fake-sidecar.py), in a
# real window: prints what each scenario did, to diff with expected.txt.
#
#   desktop/test/run.sh PRIGH_DESKTOP_BINARY [PRIGH]
#
# REAL_CHROMIUM, if set, is a Chromium for the browser window scenarios
# (with curl, and openbox: a window manager for its keys). Needs a display
# (run it under xvfb-run), a D-Bus session bus
# (for the single-instance plugin: dbus-run-session), python3 and xdotool.
# The flake's desktop-window check runs it that way.
set -uo pipefail
app=$1
real=${2:-}
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
export HOME="$work/home" XDG_CONFIG_HOME="$work/config" XDG_DATA_HOME="$work/data"
mkdir -p "$HOME"
# The package's desktop entry, installed in a profile (a link to it, as
# Nix's are).
ln -s "$(cd "$(dirname "$app")/.." && pwd)" "$work/profile"
export XDG_DATA_DIRS="$work/profile/share"
fake="$work/bin/fake-prigh"
mkdir -p "$work/bin"
cp "$here/fake-sidecar.py" "$fake"
chmod +x "$fake"
mkdir -p "$work/browsers"
# A wrapper, not a copy: in the Nix build only this directory's shebangs are
# patched, and /usr/bin/env is not there.
printf '#!%s\nexec %s %s "$@"\n' "$(command -v bash)" "$(command -v python3)" "$here/fake-browser.py" \
	>"$work/browsers/google-chrome-stable"
chmod +x "$work/browsers/google-chrome-stable"
export PRIGH_DESKTOP_NO_DIALOG=1
# The default browser (xdg-open, which the app's opener runs): a stand-in
# that says what it opened in $work/opened, and allows a sign-in there.
mkdir -p "$work/opener"
printf '#!%s\nexec %s %s %s "$@"\n' "$(command -v bash)" "$(command -v python3)" "$here/fake-default-browser.py" \
	"$work/opened" >"$work/opener/xdg-open"
chmod +x "$work/opener/xdg-open"
: >"$work/opened"
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
	sed -E -e "s#$work#WORK#g" -e "s#${real:-/nonexistent}#PRIGH#g" -e 's#127\.0\.0\.1:[0-9]+#127.0.0.1:PORT#g'  -e 's#localhost:[0-9]+#localhost:PORT#g' -e 's#serving on [0-9]+#serving on PORT#' -e 's#\(pid [0-9]+\)#(pid PID)#g' -e 's#session=[A-Za-z0-9_.-]+#session=ID#g' -e 's#cli-signin=[0-9]+#cli-signin=PORT#'
}

scenario "the window shows the page; closing it stops the sidecar" \
	PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1 PRIGH_DESKTOP_ARGS="-cwd '/a b' -name laptop"
scenario "the page's Quit: the sidecar exits 0, so does the app" FAKE_MODE=quit
scenario "the backend's own site (-ui remote), quitting from it" FAKE_MODE=remote
# The backend's site behind a sign-in proxy (authentik, as a user saw it
# loop: each visit a 302 to its login with a new state, which the window
# opened in the default browser, over and over).
signin_scenario() {
	: >"$work/opened"
	scenario "$@" PATH="$work/opener:$PATH"
	normalize <"$work/opened"
}
signin_scenario "a sign-in proxy: the window signs in there, and is back on the site" FAKE_MODE=proxy
signin_scenario "a sign-in proxy: signed in with the default browser, the cookie lands in the window; expired, the sidecar's sign-in page" \
	FAKE_MODE=proxy-browser
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
# Entries earlier versions named after the window's app_id (they go), and
# the user's own for a site (it stays).
mkdir -p "$XDG_DATA_HOME/applications"
old='[Desktop Entry]
Type=Application
Name=prigh
Comment=prigh'\''s window in the browser
Exec=/nix/store/x-prigh-desktop-unwrapped/bin/prigh-desktop
Icon=prigh-desktop
StartupWMClass=chrome-127.0.0.1__-Default
'
printf "$old" >"$XDG_DATA_HOME/applications/chrome-127.0.0.1__-Default.desktop"
printf "$old" >"$XDG_DATA_HOME/applications/brave-ai.example.com__-Default.desktop"
printf '[Desktop Entry]\nName=Mail\nExec=chromium --app=https://mail.example.com\n' \
	>"$XDG_DATA_HOME/applications/chrome-mail.example.com__-Default.desktop"
entries() {
	echo "applications: $(ls "$XDG_DATA_HOME/applications" | paste -sd ' ' -)"
}
scenario "browser: auto takes one on the PATH; closing its window stops the sidecar" \
	PRIGH_DESKTOP_WINDOW= PATH="$work/browsers:$PATH" XDG_SESSION_TYPE=wayland
# One entry for prigh: the installed one, shadowed by the user's with the
# window's Wayland app_id, run through the profile.
entries
normalize <"$XDG_DATA_HOME/applications/prigh.desktop" | sed 's/^/prigh.desktop: /'
scenario "the app's own window: the installed entry shows again" PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1
entries
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

# An instance left on prigh's profile by an app that died: closed, and the
# page opens in a new one (handed to it, the window would go with it). Found
# whether its command line is one string (Chrome rewrites its title) or does
# not name the profile at all (by the files it holds open there).
for title in 1 hidden; do
	: >"$work/log"
	: >"$work/browser-log"
	FAKE_BROWSER_LOG="$work/browser-log" FAKE_BROWSER_MODE=stay FAKE_BROWSER_TITLE=$title "$browser" \
		--app=http://127.0.0.1:9/ --user-data-dir="$profile" &
	stale=$!
	wait_for "$work/browser-log" "loaded the page"
	browser_app FAKE_BROWSER_MODE=close
	wait "$app_pid"
	code=$?
	wait "$stale"
	report "browser: an instance left on the profile (title $title) is closed; the page opens in a new one" $code
done

# Ctrl-C (SIGINT) in the terminal it runs in: the app stops as when its
# window closes, the browser with it (it is in a process group of its own).
: >"$work/log"
: >"$work/browser-log"
browser_app FAKE_BROWSER_MODE=stay
wait_for "$work/browser-log" "loaded the page"
kill -INT "$(pgrep -P "$app_pid")"
wait "$app_pid"
code=$?
echo "left on the profile: $(ps -eo args= | grep -F -- "--user-data-dir=$profile" | grep -vc grep)"
report "browser: SIGINT stops the sidecar and closes the browser" $code

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
	# No popup blocker: the links scenario's page opens a window as it loads.
	printf '#!%s\nexec "%s" --remote-debugging-port=9333 --no-sandbox --disable-popup-blocking "$@"\n' \
		"$(command -v bash)" "$REAL_CHROMIUM" >"$work/chromium"
	chmod +x "$work/chromium"
	rm -rf "$profile"
	pages() {
		curl -s http://127.0.0.1:9333/json/list |
			python3 -c 'import json, sys; print(" ".join(t["id"] for t in json.loads(sys.stdin.read() or "[]") if t["type"] == "page"))'
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

	# Links to other sites open in the default browser (xdg-open, here a
	# stand-in), not in Chromium on prigh's profile: a new window on one is
	# closed, and prigh's window going to one is sent back.
	: >"$work/log"
	: >"$work/opened"
	browser_app PRIGH_DESKTOP_BROWSER="$work/chromium" FAKE_PAGE_LINKS=1 PATH="$work/opener:$PATH"
	for _ in $(seq 300); do
		[ "$(wc -l <"$work/opened")" = 2 ] && [ "$(grep -c '^GET / ' "$work/log")" = 2 ] &&
			[ "$(pages | wc -w)" = 1 ] && break
		sleep 0.1
	done
	urls() {
		curl -s http://127.0.0.1:9333/json/list |
			python3 -c 'import json, sys; print(" ".join(sorted(t["url"].split("#")[0] for t in json.loads(sys.stdin.read() or "[]") if t["type"] == "page")))'
	}
	echo "== real Chromium: links to other sites open in the default browser"
	normalize <"$work/opened" | LC_ALL=C sort
	echo "prigh's page loaded: $(grep -c '^GET / ' "$work/log") times; Chromium's windows: $(urls | normalize)"
	for id in $(pages); do curl -s -o /dev/null "http://127.0.0.1:9333/json/close/$id"; done
	wait "$app_pid"
	echo "exit $?; left running: $(ps -eo args= | grep -F -- "--user-data-dir=$profile" | grep -vc grep)"

	# prigh's page hands a link to another site to the shell (its DevTools
	# binding): it opens in the default browser, and no window opens in
	# Chromium; a window the page opens itself still does, and is closed.
	: >"$work/log"
	: >"$work/opened"
	browser_app PRIGH_DESKTOP_BROWSER="$work/chromium" FAKE_PAGE_HANDOVER=1 PATH="$work/opener:$PATH"
	for _ in $(seq 300); do
		[ "$(wc -l <"$work/opened")" = 2 ] && [ "$(pages | wc -w)" = 1 ] && break
		sleep 0.1
	done
	echo "== real Chromium: a link handed over opens in the default browser, no window"
	normalize <"$work/opened"
	normalize <"$work/stderr" | grep -E '^prigh-desktop: (a window opened|ignored|refused)'
	echo "Chromium's windows: $(urls | normalize)"
	for id in $(pages); do curl -s -o /dev/null "http://127.0.0.1:9333/json/close/$id"; done
	wait "$app_pid"
	echo "exit $?; left running: $(ps -eo args= | grep -F -- "--user-data-dir=$profile" | grep -vc grep)"

	# The backend's site behind a sign-in proxy, in Chromium: the window
	# signs in there (the proxy's redirects are not links); signed in with
	# the default browser, the cookie lands in the window (through its
	# DevTools), and once it has expired the window is on the sidecar's
	# sign-in page (where the fake quits).
	: >"$work/log"
	: >"$work/opened"
	browser_app PRIGH_DESKTOP_BROWSER="$work/chromium" FAKE_MODE=proxy PATH="$work/opener:$PATH"
	wait_for "$work/log" "with the cookie: the page"
	for id in $(pages); do curl -s -o /dev/null "http://127.0.0.1:9333/json/close/$id"; done
	wait "$app_pid"
	real_report "real Chromium: a sign-in proxy: the window signs in there, and is back on the site" $?
	normalize <"$work/opened"
	: >"$work/log"
	: >"$work/opened"
	browser_app PRIGH_DESKTOP_BROWSER="$work/chromium" FAKE_MODE=proxy-browser FAKE_SIGNIN_STAY=1 \
		PATH="$work/opener:$PATH"
	wait_for "$work/stderr" "is back from"
	for id in $(pages); do curl -s -o /dev/null "http://127.0.0.1:9333/json/close/$id"; done
	wait "$app_pid"
	real_report "real Chromium: a sign-in proxy: signed in with the default browser; expired, the sign-in page" $?
	normalize <"$work/opened"

	# The window is an app's: a popup on prigh's site (Open in new window)
	# becomes another app window, and the browser's own windows (a new tab
	# or window, incognito, history, DevTools), opened with its keys in
	# prigh's window, are closed, the focus back in prigh's window. Chromium
	# takes its keys only in an active window: a window manager (openbox) for
	# this one.
	# SHOTS=DIR keeps screenshots (with ImageMagick's import).
	page_list() {
		curl -s http://127.0.0.1:9333/json/list | python3 -c '
import json, sys
for t in sorted(json.loads(sys.stdin.read() or "[]"), key=lambda t: t["url"]):
    if t["type"] == "page":
        print("  " + t["url"].split("#")[0] + (", a popup" if t.get("openerId") else ""))'
	}
	shot() {
		[ -n "${SHOTS:-}" ] && mkdir -p "$SHOTS" && import -window root "$SHOTS/$1.png"
		return 0
	}
	: >"$work/log"
	openbox --sm-disable >/dev/null 2>&1 &
	wm_pid=$!
	browser_app PRIGH_DESKTOP_BROWSER="$work/chromium" FAKE_PAGE_POPUP=1
	for _ in $(seq 300); do
		[ "$(page_list | normalize)" = "$(printf '  http://127.0.0.1:PORT/\n  http://127.0.0.1:PORT/?popup=1')" ] &&
			xdotool search --onlyvisible --name '^fake prigh-web, another window$' >/dev/null && break
		sleep 0.1
	done
	echo "== real Chromium: the window is an app's"
	echo "Open in new window: Chromium's windows:"
	page_list | normalize
	shot popup
	main=$(xdotool search --onlyvisible --name '^fake prigh-web$' | head -1)
	closed() { grep -c '^prigh-desktop: closed ' "$work/stderr"; }
	for key in ctrl+t ctrl+n ctrl+shift+n ctrl+h F12; do
		xdotool windowactivate --sync "$main"
		before=$(closed)
		xdotool key "$key"
		# The app closed what it opened; then the page is gone, the focus back.
		for _ in $(seq 100); do [ "$(closed)" != "$before" ] && break; sleep 0.1; done
		for _ in $(seq 100); do
			[ "$(page_list | wc -l)" = 2 ] && [ "$(xdotool getactivewindow)" = "$main" ] && break
			sleep 0.1
		done
		shot "$key"
		echo "$key: $(normalize <"$work/stderr" | grep '^prigh-desktop: closed ' | tail -n +$((before + 1)) |
			sed 's/^prigh-desktop: //' | paste -sd ';' -); Chromium's windows:"
		page_list | normalize
		echo "  the focus in prigh's window: $([ "$(xdotool getactivewindow)" = "$main" ] && echo yes || echo no)"
	done
	for id in $(pages); do curl -s -o /dev/null "http://127.0.0.1:9333/json/close/$id"; done
	wait "$app_pid"
	echo "exit $?"
	kill "$wm_pid"
	wait "$wm_pid" 2>/dev/null
	normalize <"$work/stderr" | grep -E '^prigh-desktop: ' | grep -v '/favicon.ico'
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
# POST /pointer "move|click|ctrlclick X Y" (the window's pixels) moves the real
# pointer there, and clicks, with xdotool; anything else is a report line.
python3 - "$work/reports" "$report_port_file" <<'PY' &
import http.server, subprocess, sys
out, port_file = sys.argv[1], sys.argv[2]
def window():
    # The app's window: the largest visible one called prigh.
    ids = subprocess.run(["xdotool", "search", "--onlyvisible", "--name", "^prigh$"],
                         capture_output=True, text=True).stdout.split()
    def geometry(i):
        g = dict(l.split("=") for l in subprocess.run(
            ["xdotool", "getwindowgeometry", "--shell", i], capture_output=True, text=True).stdout.split())
        return int(g["X"]), int(g["Y"]), int(g["WIDTH"]) * int(g["HEIGHT"])
    return max(map(geometry, ids), key=lambda g: g[2])
def pointer(what, x, y):
    wx, wy, _ = window()
    move = ["xdotool", "mousemove", "--sync", str(wx + int(x)), str(wy + int(y))]
    act = {"click": ["click", "1"], "ctrlclick": ["keydown", "ctrl", "click", "1", "keyup", "ctrl"]}
    subprocess.run(move + act.get(what, []), check=True)
class H(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get("Content-Length", 0))).decode()
        if self.path == "/pointer":
            try:
                pointer(*body.split())
            except Exception as e:
                body = f"pointer {body}: {e}"
            else:
                body = None
        if body is not None:
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
# Saved by the one before: signed in at once.
real_scenario "the real sidecar: a chip's menu, chosen with the real pointer" menu local
zoom_file="$XDG_CONFIG_HOME/io.github.evan-a-w.prigh/zoom"
mkdir -p "$(dirname "$zoom_file")"
echo 1.25 >"$zoom_file"
real_scenario "the real sidecar at 125% zoom: a chip's menu, chosen with the real pointer" menu local
rm "$zoom_file"
real_scenario "the real sidecar, -ui remote: the backend's own site; its /quit ends the app" quit remote
real_scenario "the real sidecar: Open in new window opens another app window; one sidecar" window local
real_scenario "the real sidecar, -ui remote: Open in new window on the backend's site" window remote
