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
	mkdir -p "$tmp/home" "$tmp/cwd"
	session="prigh-test-$$-$RANDOM"
	tmux new-session -d -s "$session" -x "$W" -y "$H" \
		"env HOME=$tmp/home TERM=xterm-256color PRIGH_BACKEND=$backend \
		 $exe -faux -cwd $tmp/cwd -auth-file $tmp/home/auth.json $*; \
		 echo EXITED; stty -a | tr ' ;' '\n\n' | grep -E '^-?(icanon|echo|iexten|isig|ixon)$' | tr '\n' ' '; echo; sleep 30"
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
	tmp="$(mktemp -d)"
	extra_args=""
	if declare -F "setup_$name" >/dev/null; then "setup_$name"; fi
	start $extra_args
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
	wait_for "Ctrl+C twice"
	settle
	capture startup "initial screen"
}

scenario_prompt() {
	wait_for "Ctrl+C twice"
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
	wait_for "Ctrl+C twice"
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
	wait_for "Ctrl+C twice"
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
	wait_for "Ctrl+C twice"
	keys C-c
	wait_for "Ctrl+C again"
	capture quit "after first C-c"
	keys C-c
	wait_for "EXITED"
	sleep 0.3
	# The tty must be sane after exit: the shell prints the termios flags.
	capture quit "exited (tty flags)"
}

# A scripted provider run: a bash tool call, then two subagents in one turn.
setup_tools() {
	extra_args="-- -faux-script $tmp/script.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "let me look", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "printf 'one\\ntwo\\nthree\\n'"}}]},
  {"text": "three lines. now delegating", "tool_calls": [
     {"id": "s1", "name": "subagent", "arguments": {"task": "count files", "tools": ["ls"]}},
     {"id": "s2", "name": "subagent", "arguments": {"task": "say hello", "tools": ["ls"]}}]},
  {"text": "child one reporting: 0 files"},
  {"text": "child two reporting: hello"},
  {"text": "all done"}
]
JSON
}

scenario_tools() {
	wait_for "Ctrl+C twice"
	type_text "go"
	keys Enter
	wait_for "all done"
	settle
	capture tools "normal"
	keys C-o
	settle
	capture tools "verbose"
	keys C-o
	settle
	capture tools "quiet"
	keys BTab
	settle
	capture tools "agent 1"
	keys BTab
	settle
	capture tools "agent 2"
	keys BTab
	settle
	capture tools "main again"
}

all="startup prompt ctrl_o resize quit tools"
for name in ${*:-$all}; do
	run_scenario "$name"
done
[ "$failures" = 0 ]
