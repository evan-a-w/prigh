#!/usr/bin/env bash
# prigh-computer on Hyprland, which matches binds through its configured
# layout whatever keymap a virtual keyboard uploads: a key sent at the wrong
# keycode fires another bind (the helper's `d` was Super+1, wtype's
# Super+Escape). Hyprland runs nested in a virtual KWin (aquamarine wants
# xdg_wm_base 6, which sway and weston lack); it needs a GPU render node
# (/dev/dri/renderD*), so this is not a flake check: the Nix sandbox has
# none. With binds that write files and wev as the focused app, the helper
# presses super+d, types, and presses a plain d; what fired and what wev
# received are printed, for hyprland.expected.
#
#   nix run .#computer-use-hyprland [-- PRIGH_COMPUTER]
#
# KEEP=1 keeps the logs.
set -euo pipefail
helper=${1:-prigh-computer}
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
pids=()
kill_tree() {
	local c
	for c in $(pgrep -P "$1"); do kill_tree "$c"; done
	kill -9 "$1" 2>/dev/null || true
}
cleanup() {
	for p in "${pids[@]}"; do kill_tree "$p"; done
	wait 2>/dev/null || true
	if [ -n "${KEEP:-}" ]; then echo "kept $work" >&2; else rm -rf "$work"; fi
}
trap cleanup EXIT
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
has() { grep -q "$2" "$1"; }
# fired: the binds that ran since the last call. The helper has waited for
# Hyprland to take its keys, so their execs started before this one.
fired() {
	local f out=
	rm -f "$work/sync"
	hyprctl dispatch "hl.dsp.exec_cmd(\"touch $work/sync\")" >/dev/null
	wait_for 10 test -e "$work/sync"
	for f in d escape 1; do [ -e "$work/$f" ] && out="$out super+$f"; done
	[ -e "$work/alt-d" ] && out="$out alt+d"
	echo "binds fired:${out:- none}"
	rm -f "$work/d" "$work/escape" "$work/1" "$work/alt-d"
}

ls /dev/dri/renderD* >/dev/null 2>&1 || {
	echo "no GPU render node (/dev/dri/renderD*): Hyprland cannot run here" >&2
	exit 1
}
# Unset the user's session: everything here is nested and private.
unset WAYLAND_DISPLAY DISPLAY HYPRLAND_INSTANCE_SIGNATURE SWAYSOCK WAYLAND_SOCKET
export XDG_RUNTIME_DIR=$work/run
mkdir -m 700 "$XDG_RUNTIME_DIR"

QT_QPA_PLATFORM=offscreen kwin_wayland --virtual --width 1920 --height 1080 --socket kwin-0 \
	--no-lockscreen --no-global-shortcuts >"$work/kwin.log" 2>&1 &
pids+=($!)
wait_for 20 test -e "$XDG_RUNTIME_DIR/kwin-0"

cat >"$work/hyprland.lua" <<CONF
hl.monitor({ output = "", mode = "1920x1080@60", position = "0x0", scale = "1" })
hl.config({ misc = { disable_hyprland_logo = true, disable_splash_rendering = true } })
hl.bind("SUPER + D", hl.dsp.exec_cmd("touch $work/d"))
hl.bind("SUPER + Escape", hl.dsp.exec_cmd("touch $work/escape"))
hl.bind("SUPER + 1", hl.dsp.exec_cmd("touch $work/1"))
hl.bind("ALT + D", hl.dsp.exec_cmd("touch $work/alt-d"))
CONF
# A seat that cannot open: Hyprland then leaves the machine's input devices
# and DRM alone and runs on the Wayland backend only. Now and then it starts
# with no monitor (the nested output never comes up): then it starts again.
started=
for attempt in 1 2 3; do
	rm -rf "$XDG_RUNTIME_DIR/hypr"
	WAYLAND_DISPLAY=kwin-0 LIBSEAT_BACKEND=seatd SEATD_SOCK=$work/no-seatd HYPRLAND_NO_CRASHREPORTER=1 \
		Hyprland --config "$work/hyprland.lua" >"$work/hyprland.log" 2>&1 &
	hypr=$!
	pids+=($hypr)
	if wait_for 30 bash -c "ls $XDG_RUNTIME_DIR/hypr/*/.socket.sock"; then
		HYPRLAND_INSTANCE_SIGNATURE=$(basename "$(dirname "$(ls "$XDG_RUNTIME_DIR"/hypr/*/.socket.sock)")")
		export HYPRLAND_INSTANCE_SIGNATURE
		if wait_for 15 bash -c 'hyprctl -j monitors | grep -q "\"width\": 1920"' 2>/dev/null; then
			started=1
			break
		fi
	fi
	echo "Hyprland attempt $attempt came up without a monitor" >"$work/attempt-$attempt"
	kill_tree "$hypr"
done
[ -n "$started" ] || {
	tail -20 "$work/hyprland.log" >&2
	exit 1
}
WAYLAND_DISPLAY=$(hyprctl -j instances | sed -nE 's/.*"wl_socket": "([^"]*)".*/\1/p' | head -1)
export WAYLAND_DISPLAY
stdbuf -oL wev >"$work/wev.log" 2>&1 &
pids+=($!)
wait_for 20 bash -c 'hyprctl -j activewindow | grep -q wev'

ask() {
	printf '%s\n' "$1" | "$helper" --max-long-side 640 --settle-ms 200 \
		| sed -E 's/"data":"[^"]*"/"data":"…"/'
}
echo "== super+d"
ask '{"id":1,"op":"act","action":{"action":"key","combo":"super+d"}}'
fired
echo "== type"
ask '{"id":2,"op":"act","action":{"action":"type","text":"hello é"}}'
fired
echo "== d alone (no Super left down)"
ask '{"id":3,"op":"act","action":{"action":"key","combo":"d"}}'
fired
# A compositor may keep a stopped client's keys down (Hyprland does unless
# release_pressed_on_close): on SIGTERM the helper lets go first.
echo "== SIGTERM while holding alt, then d alone"
mkfifo "$work/in"
"$helper" --settle-ms 0 <"$work/in" >"$work/held.out" &
held=$!
exec 4>"$work/in"
echo '{"id":4,"op":"act","action":{"action":"hold_key","combo":"alt","duration":10}}' >&4
wait_for 10 has "$work/wev.log" "depressed: 00000008"
kill -TERM "$held"
wait "$held" || true
exec 4>&-
sed -E 's/"data":"[^"]*"/"data":"…"/' "$work/held.out"
ask '{"id":5,"op":"act","action":{"action":"key","combo":"d"}}'
fired
echo "-- wev received:"
awk -f "$here/wev.awk" "$work/wev.log" /dev/null
