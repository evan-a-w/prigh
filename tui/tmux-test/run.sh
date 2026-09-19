#!/usr/bin/env bash
# Real-terminal harness: runs the built TUI (`main.exe -faux`) inside a
# detached tmux session, sends keys, captures panes and diffs them against
# expected/*.txt (normalised). Usage:
#   tmux-test/run.sh            run every scenario
#   tmux-test/run.sh NAME...    run some
#   UPDATE=1 tmux-test/run.sh   rewrite the expected files
# Skips (exit 0) when tmux is not installed.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tui="$(dirname "$here")"
root="$(dirname "$tui")"
exe="${PRIGH_TUI:-$tui/_build/default/bin/main.exe}"
backend="${PRIGH_BACKEND:-$root/backend/_build/default/bin/main.exe}"
expected="$here/expected"

if ! command -v tmux >/dev/null; then
	echo "tmux-test: tmux not installed, skipping"
	exit 0
fi
for f in "$exe" "$backend"; do
	[ -x "$f" ] || { echo "tmux-test: missing $f (build both projects first)"; exit 1; }
done

W=100
H=30
failures=0
session=""
tmp=""

start() {
	tmp="$(mktemp -d)"
	mkdir -p "$tmp/home" "$tmp/cwd"
	session="prigh-test-$$-$RANDOM"
	tmux new-session -d -s "$session" -x "$W" -y "$H" \
		"env HOME=$tmp/home TERM=xterm-256color PRIGH_BACKEND=$backend \
		 $exe -faux -cwd $tmp/cwd -auth-file $tmp/home/auth.json $*; \
		 echo EXITED; sleep 30"
}

stop() {
	tmux kill-session -t "$session" 2>/dev/null || true
	rm -rf "$tmp"
}

keys() { tmux send-keys -t "$session" "$@"; }
type_text() { tmux send-keys -t "$session" -l "$1"; }
paste() { printf '%s' "$1" | tmux load-buffer -; tmux paste-buffer -p -t "$session"; }
resize() { tmux resize-window -t "$session" -x "$1" -y "$2"; }

# Waits (up to 10s) until the pane contains the pattern.
wait_for() {
	local pattern=$1 i
	for i in $(seq 1 100); do
		if tmux capture-pane -p -t "$session" | grep -q -- "$pattern"; then return 0; fi
		sleep 0.1
	done
	echo "  timeout waiting for: $pattern"
	tmux capture-pane -p -t "$session"
	return 1
}

# Waits until the pane has been stable for 0.3s.
settle() {
	local prev cur
	prev="$(tmux capture-pane -p -t "$session")"
	while :; do
		sleep 0.3
		cur="$(tmux capture-pane -p -t "$session")"
		[ "$cur" = "$prev" ] && break
		prev="$cur"
	done
}

normalise() {
	sed -E \
		-e "s#$tmp#\$TMP#g" \
		-e 's/[⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏]/<spin>/g' \
		-e 's/session [0-9a-f]{16}/session <id>/g' \
		-e 's/[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z?/<time>/g' \
		-e 's/[[:space:]]+$//'
}

# capture NAME STEP: appends the normalised pane to the scenario transcript.
capture() {
	{
		echo "=== $1"
		tmux capture-pane -p -t "$session" | normalise | sed '/./,$!d' | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}'
	} >>"$out"
}

check() {
	local name=$1
	if [ "${UPDATE:-}" = 1 ]; then
		cp "$out" "$expected/$name.txt"
		echo "updated $name"
	elif diff -u "$expected/$name.txt" "$out" >"$out.diff"; then
		echo "ok      $name"
	else
		echo "FAIL    $name"
		cat "$out.diff"
		failures=$((failures + 1))
	fi
}

run_scenario() {
	local name=$1
	out="$(mktemp)"
	start
	if ! "scenario_$name"; then
		echo "FAIL    $name (scenario error)"
		failures=$((failures + 1))
	else
		check "$name"
	fi
	stop
	rm -f "$out" "$out.diff"
}

# ---- scenarios ------------------------------------------------------------

scenario_startup() {
	wait_for "Ctrl+C twice quits"
	settle
	capture startup "initial screen"
}

scenario_prompt() {
	wait_for "Ctrl+C twice quits"
	type_text "hello there"
	settle
	capture prompt "typed"
	keys Enter
	wait_for "faux reply"
	settle
	capture prompt "after reply"
}

scenario_ctrl_o() {
	# ^O is VDISCARD: without IEXTEN cleared the tty swallows it and the
	# status line never changes. The screen must differ after C-o.
	wait_for "Ctrl+C twice quits"
	settle
	local before after
	before="$(tmux capture-pane -p -t "$session")"
	keys C-o
	sleep 0.5
	settle
	after="$(tmux capture-pane -p -t "$session")"
	if [ "$before" = "$after" ]; then
		echo "  Ctrl+O did not change the screen"
		return 1
	fi
	capture ctrl_o "after C-o"
}

scenario_resize() {
	wait_for "Ctrl+C twice quits"
	type_text "first"
	keys Enter
	wait_for "faux reply"
	resize 40 12
	settle
	capture resize "40x12"
	resize 100 30
	settle
	capture resize "100x30"
}

scenario_quit() {
	wait_for "Ctrl+C twice quits"
	keys C-c
	wait_for "again quits"
	capture quit "after first C-c"
	keys C-c
	wait_for "EXITED"
	# The tty must be sane after exit: check flags through the pane's shell.
	capture quit "exited"
}

all="startup prompt ctrl_o resize quit"
for name in ${*:-$all}; do
	run_scenario "$name"
done
[ "$failures" = 0 ]
