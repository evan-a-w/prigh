#!/usr/bin/env bash
# The desktop shell against a fake `prigh desktop` (fake-sidecar.py), in a
# real window: prints what each scenario did, to diff with expected.txt.
#
#   desktop/test/run.sh PRIGH_DESKTOP_BINARY
#
# Needs a display (run it under xvfb-run), a D-Bus session bus (for the
# single-instance plugin: dbus-run-session) and python3. The flake's
# desktop-window check runs it that way.
set -uo pipefail
app=$1
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
	sed -E -e "s#$work#WORK#g" -e 's#127\.0\.0\.1:[0-9]+#127.0.0.1:PORT#g' -e 's#serving on [0-9]+#serving on PORT#'
}

scenario "the window shows the page; closing it stops the sidecar" \
	PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1 PRIGH_DESKTOP_ARGS="-cwd '/a b' -name laptop"
scenario "the page's Quit: the sidecar exits 0, so does the app" FAKE_MODE=quit
scenario "the sidecar fails after it was ready" FAKE_MODE=crash
scenario "the sidecar fails before it is ready" FAKE_MODE=fail
scenario "the sidecar prints something else" FAKE_MODE=bad-ready
scenario "the sidecar ignores its stdin closing: killed" \
	FAKE_MODE=ignore-eof PRIGH_DESKTOP_TEST_CLOSE_ON_LOAD=1
scenario "no such prigh" PRIGH_DESKTOP_BACKEND=prigh-not-installed
scenario "bad PRIGH_DESKTOP_ARGS" PRIGH_DESKTOP_ARGS="-cwd 'x"
