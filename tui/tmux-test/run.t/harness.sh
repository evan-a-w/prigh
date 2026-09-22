#!/usr/bin/env bash
# Real-terminal harness behind the cram test in run.t: runs the built TUI
# (`prigh-tui -faux`) inside a detached tmux session, sends keys, and prints
# the captured panes (normalised) for the cram expectation. Usage:
#   harness.sh NAME     run one scenario (see the bottom of this file)
# Outside dune, PRIGH_TUI and PRIGH_BACKEND locate the executables.
set -euo pipefail
# Under dune the locale is C; the spinner and rules are multi-byte.
export LC_ALL=C.UTF-8
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [ -n "${DUNE_SOURCEROOT:-}" ]; then
	tui="$DUNE_SOURCEROOT"
else
	tui="$(dirname "$(dirname "$here")")"
fi
root="$(dirname "$tui")"
exe="${PRIGH_TUI:-$(command -v prigh-tui || echo "$tui/_build/default/bin/main.exe")}"
backend="${PRIGH_BACKEND:-$root/backend/_build/default/bin/main.exe}"

if ! command -v tmux >/dev/null; then
	echo "tmux-test: tmux not installed"
	exit 1
fi
for f in "$exe" "$backend"; do
	[ -x "$f" ] || { echo "tmux-test: missing $f (build both projects first)"; exit 1; }
done

W=100
H=30
session=""
tmp=""

start() {
	mkdir -p "$tmp/home" "$tmp/cwd"
	session="prigh-test-$$-$RANDOM"
	{
		echo "export HOME=$tmp/home TERM=xterm-256color EDITOR=$tmp/editor.sh PRIGH_BACKEND=$backend"
		echo "$exe -faux -cwd $tmp/cwd -auth-file $tmp/home/auth.json $*"
		echo "echo EXITED"
		echo "stty -a | tr ' ;' '\\n\\n' | grep -E '^-?(icanon|echo|iexten|isig|ixon)\$' | tr '\\n' ' '; echo"
	} >"$tmp/run.sh"
	if [ "${interactive:-}" = 1 ]; then
		# Job control (Ctrl+Z / fg) needs an interactive shell.
		tmux new-session -d -s "$session" -x "$W" -y "$H" "env -i PATH=$PATH TERM=xterm-256color bash --norc --noprofile -i"
		sleep 0.5
		tmux send-keys -t "$session" -l "clear; sh $tmp/run.sh"
		tmux send-keys -t "$session" Enter
	else
		tmux new-session -d -s "$session" -x "$W" -y "$H" "sh $tmp/run.sh; sleep 30"
	fi
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

# capture NAME STEP: prints the normalised pane.
capture() {
	echo "=== $2"
	tmux capture-pane -p -t "$session" | normalise | sed '/./,$!d' | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}'
}

run_scenario() {
	local name=$1 status=0
	tmp="$(mktemp -d)"
	extra_args=""
	interactive=""
	if declare -F "setup_$name" >/dev/null; then "setup_$name"; fi
	start $extra_args
	"scenario_$name" || status=$?
	stop
	return $status
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

setup_suspend() { interactive=1; }

scenario_suspend() {
	wait_for "Ctrl+C twice"
	type_text "before"
	keys Enter
	wait_for "faux reply"
	keys C-z
	wait_for "Stopped"
	settle
	capture suspend "suspended (shell visible)"
	type_text "fg"
	keys Enter
	wait_for "faux reply"
	settle
	capture suspend "after fg (repainted)"
	type_text "still typing"
	settle
	capture suspend "editor works"
}

setup_editor() {
	interactive=1
	# A fake $EDITOR that appends a line to the prompt file.
	printf '#!/bin/sh\nprintf "edited by editor\\n" >> "$1"\n' >"$tmp/editor.sh"
	chmod +x "$tmp/editor.sh"
}

scenario_editor() {
	wait_for "Ctrl+C twice"
	type_text "draft"
	keys C-g
	wait_for "edited by editor"
	settle
	capture editor "after Ctrl+G round trip"
}

setup_confirm() {
	extra_args="-- -faux-script $tmp/script.json"
	mkdir -p "$tmp/home/.prigh"
	echo '{"scoped_models": [], "confirm_tools": true}' >"$tmp/home/.prigh/config.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "running", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "echo ran-it"}}]},
  {"text": "first done"},
  {"text": "again", "tool_calls": [{"id": "c2", "name": "bash", "arguments": {"command": "echo never"}}]},
  {"text": "second done"}
]
JSON
}

scenario_confirm() {
	wait_for "Ctrl+C twice"
	type_text "go"
	keys Enter
	wait_for "Run bash"
	settle
	capture confirm "dialog"
	keys y
	wait_for "first done"
	settle
	capture confirm "allowed"
	type_text "go again"
	keys Enter
	wait_for "Run bash"
	keys n
	wait_for "second done"
	settle
	capture confirm "denied"
}

scenario_paste() {
	wait_for "Ctrl+C twice"
	# Bracketed paste: Enter inside the paste must not submit.
	paste "$(printf 'line one\nline two\nline three\nline four\nline five')"
	settle
	capture paste "chip after a 5-line paste"
	keys Left
	settle
	capture paste "cursor inside expands the chip"
	keys Enter
	wait_for "faux reply"
	settle
	capture paste "submitted as one message"
}

# The spawned backend dies; the TUI reconnects (respawning it) and rejoins
# the same session.
scenario_reconnect() {
	wait_for "Ctrl+C twice"
	type_text "before"
	keys Enter
	wait_for "faux reply"
	pkill -f "serve -faux -cwd $tmp/cwd"
	wait_for "reconnected to the backend"
	settle
	capture reconnect "after the backend was killed"
	type_text "after"
	keys Enter
	wait_for "> after"
	settle
	capture reconnect "prompt works again"
}

run_scenario "$1"
