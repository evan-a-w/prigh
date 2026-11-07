#!/usr/bin/env bash
# prigh-computer on real desktops: a headless sway (wlroots) and Xvfb (X11),
# each with an app that prints the input it gets (wev, xev). The helper is
# driven with JSON lines (a click, typed text, a key combo, a scroll, a zoom);
# its replies (without the image data) and what the app received are
# printed, for expected.txt.
#
#   bash run.sh PRIGH_COMPUTER [PRIGH]
#
# With PRIGH, an exec also goes through `prigh tool-host -computer` (the
# stdio worker), as a backend sends it. Needs sway, wev, Xvfb and
# xev on PATH. KEEP=1 keeps the logs.
set -euo pipefail
helper=$1
prigh=${2:-}
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
pids=()
# kill_tree PID: it and its descendants (sway's apps).
kill_tree() {
	local c
	for c in $(pgrep -P "$1"); do kill_tree "$c"; done
	kill -9 "$1" 2>/dev/null || true
}
cleanup() {
	for p in "${pids[@]}"; do kill_tree "$p"; done
	wait 2>/dev/null || true
	if [ -n "${KEEP:-}" ]; then chmod -R a+rX "$work"; echo "kept $work" >&2; else rm -rf "$work"; fi
}
trap cleanup EXIT

# wait_for SECONDS COMMAND...: until it succeeds.
wait_for() {
	local n=$(($1 * 10))
	shift
	until "$@" >/dev/null 2>&1; do
		n=$((n - 1))
		if [ "$n" -le 0 ]; then
			echo "timed out waiting for: $*" >&2
			return 1
		fi
		sleep 0.1
	done
}
# has FILE PATTERN: the app printed it.
has() { grep -q "$2" "$1"; }

requests='{"id":1,"op":"info"}
{"id":2,"op":"act","action":{"action":"click","x":320,"y":180}}
{"id":3,"op":"act","action":{"action":"type","text":"hello é→"}}
{"id":4,"op":"act","action":{"action":"key","combo":"ctrl+a"}}
{"id":5,"op":"act","action":{"action":"scroll","x":100,"y":50,"dy":2}}
{"id":6,"op":"act","action":{"action":"zoom","region":[0,0,64,36]}}'
drive() {
	printf '%s\n' "$requests" | "$helper" --max-long-side 640 --settle-ms 100 \
		| sed -E 's/"data":"[^"]*"/"data":"…"/; s/"detail":"[^"]*"/"detail":"…"/'
}

echo "== wlroots (headless sway)"
export XDG_RUNTIME_DIR=$work/run
mkdir -m 700 "$XDG_RUNTIME_DIR"
cat >"$work/sway.conf" <<CONF
output HEADLESS-1 resolution 1280x720 scale 1
default_border none
exec WAYLAND_DEBUG=1 stdbuf -oL wev >$work/wev.log 2>$work/wev.debug
CONF
WLR_BACKENDS=headless WLR_LIBINPUT_NO_DEVICES=1 WLR_RENDERER=pixman \
	sway -c "$work/sway.conf" >"$work/sway.log" 2>&1 &
pids+=($!)
wait_for 20 bash -c "ls $XDG_RUNTIME_DIR/wayland-? && ls $XDG_RUNTIME_DIR/sway-ipc.*.sock" || {
	cat "$work/sway.log" >&2
	exit 1
}
WAYLAND_DISPLAY=$(basename "$(ls "$XDG_RUNTIME_DIR"/wayland-? | head -1)")
export WAYLAND_DISPLAY
wait_for 20 has "$work/wev.log" "configure: width: 1280"
drive
wait_for 10 has "$work/wev.debug" "axis_value120"
wait_for 10 has "$work/wev.log" "sym: a "
echo "-- wev received:"
awk -f "$here/wev.awk" "$work/wev.log" "$work/wev.debug"
unset WAYLAND_DISPLAY

echo "== x11 (Xvfb)"
exec 3>"$work/display"
Xvfb -displayfd 3 -screen 0 1280x720x24 -nolisten tcp >"$work/xvfb.log" 2>&1 &
pids+=($!)
exec 3>&-
wait_for 20 test -s "$work/display"
DISPLAY=":$(cat "$work/display")"
export DISPLAY
stdbuf -oL xev -geometry 1280x720+0+0 -event keyboard -event button -event structure >"$work/xev.log" 2>&1 &
pids+=($!)
wait_for 10 has "$work/xev.log" "^MapNotify"
# Xlib loads the keymap and asks for its changes at the first key: until
# that has reached the server, xev misses a remapping (é read NoSymbol).
# Apps (GTK, Qt) do that when they start.
printf '%s\n' '{"id":0,"op":"act","action":{"action":"key","combo":"shift"}}' \
	| "$helper" --settle-ms 0 >/dev/null
wait_for 10 has "$work/xev.log" "Shift_L"
drive
wait_for 10 has "$work/xev.log" "keysym 0x61, a"
echo "-- xev received:"
awk -f "$here/xev.awk" "$work/xev.log"

if [ -n "$prigh" ]; then
	echo "== prigh tool-host -computer (X11)"
	# stdin stays open until the result is in: at its end the worker cancels
	# what still runs.
	coproc TH { PRIGH_COMPUTER=$helper PRIGH_COMPUTER_ARGS="--max-long-side 640" timeout 30 "$prigh" tool-host -computer 2>/dev/null; }
	printf '%s\n' '{"type":"exec","exec_id":"e1","name":"computer","arguments":{"action":"click","x":10,"y":10},"cwd":"/"}' >&"${TH[1]}"
	while IFS= read -r line <&"${TH[0]}"; do
		case $line in *'"type":"result"'*) break ;; esac
	done
	exec {TH[1]}>&-
	wait "$TH_PID" || true
	printf '%s\n' "$line" | sed -E 's/"data":"[^"]*"/"data":"…"/g'
fi
