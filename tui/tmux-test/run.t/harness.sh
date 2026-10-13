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

# A private tmux server: the scenarios never see (or kill) the user's
# sessions, even when run from inside tmux.
socket="prigh-test-$$"
unset TMUX
tmux() { command tmux -L "$socket" "$@"; }

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
	tmux kill-server 2>/dev/null || true
	rm -f "${TMUX_TMPDIR:-/tmp}/tmux-$(id -u)/$socket"
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
		-e 's/[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}/<time>/g' \
		-e 's/^(\[[0-9]+\]\+ +Stopped) +/\1 /' \
		-e 's/^bash-[0-9.]+\$/bash$/' \
		-e 's/[[:space:]]+$//'
}

# capture NAME STEP: prints the normalised pane.
capture() {
	echo "=== $2"
	tmux capture-pane -p -t "$session" | normalise | sed '/./,$!d' | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}'
}

run_scenario() {
	local name=$1 status=0
	# Fixed length, so that lines wrap the same everywhere.
	tmp="$(mktemp -d /tmp/prigh-tmux.XXXXXX)"
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
	wait_for "Ctrl+C"
	settle
	capture startup "initial screen"
}

scenario_prompt() {
	wait_for "Ctrl+C"
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
	wait_for "Ctrl+C"
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
	wait_for "Ctrl+C"
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
	wait_for "Ctrl+C"
	keys C-c
	wait_for "Ctrl+C again"
	capture quit "after first C-c"
	keys C-c
	wait_for "EXITED"
	# The tty must be sane after exit: the shell prints the termios flags
	# (wait for them rather than for a fixed time; stty can lag on a slow box).
	wait_for "ixon"
	capture quit "exited (tty flags)"
}

# A scripted provider run: a bash tool call, then two background subagents in
# one turn and a subagent_wait for both. The replies are one shared script:
# each subagent takes its first reply as it starts, before the main agent asks
# for its next one, and both get the same reply so their order does not
# matter.
setup_tools() {
	extra_args="-- -faux-script $tmp/script.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "let me look", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "printf 'one\\ntwo\\nthree\\n'"}}]},
  {"text": "three lines. now delegating", "tool_calls": [
     {"id": "s1", "name": "subagent", "arguments": {"task": "count files", "tools": ["ls"]}},
     {"id": "s2", "name": "subagent", "arguments": {"task": "say hello", "tools": ["ls"]}}]},
  {"text": "child reporting: done"},
  {"text": "child reporting: done"},
  {"text": "waiting for both", "tool_calls": [{"id": "w1", "name": "subagent_wait", "arguments": {}}]},
  {"text": "all done"}
]
JSON
}

scenario_tools() {
	wait_for "Ctrl+C"
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
	wait_for "Ctrl+C"
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
	wait_for "Ctrl+C"
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
	wait_for "Ctrl+C"
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

# Approval modes and answers: always allow (the next matching command runs
# without asking), deny with a reason, Alt+M cycling the mode.
setup_approvals() {
	extra_args="-- -faux-script $tmp/script.json"
	mkdir -p "$tmp/home/.prigh"
	echo '{"approval_mode": "ask"}' >"$tmp/home/.prigh/config.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "one", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "echo first-run"}}]},
  {"text": "first done"},
  {"text": "two", "tool_calls": [{"id": "c2", "name": "bash", "arguments": {"command": "echo second-run"}}]},
  {"text": "second done"},
  {"text": "three", "tool_calls": [{"id": "c3", "name": "bash", "arguments": {"command": "make deploy"}}]},
  {"text": "third done"}
]
JSON
}

scenario_approvals() {
	wait_for "Ctrl+C"
	type_text "go"
	keys Enter
	wait_for "always allow"
	settle
	capture approvals "dialog with always"
	keys a
	wait_for "first done"
	type_text "again"
	keys Enter
	wait_for "second done"
	settle
	capture approvals "the second echo ran without asking"
	type_text "deploy"
	keys Enter
	wait_for "Run bash: make deploy"
	keys r
	wait_for "Why not"
	type_text "not on a Friday"
	keys Enter
	wait_for "third done"
	settle
	capture approvals "denied with a reason"
	keys M-m
	settle
	capture approvals "Alt+M: auto-edit"
}

# Plan mode: Alt+M into it, an edit refused, the plan approved, then
# implemented.
setup_plan() {
	extra_args="-- -faux-script $tmp/script.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "looking", "tool_calls": [{"id": "c1", "name": "write", "arguments": {"path": "a.txt", "content": "x"}}]},
  {"text": "planned", "tool_calls": [{"id": "c2", "name": "exit_plan_mode", "arguments": {"plan": "1. Write a.txt\n2. Check it"}}]},
  {"text": "implementing", "tool_calls": [{"id": "c3", "name": "write", "arguments": {"path": "a.txt", "content": "x"}}]},
  {"text": "all done"}
]
JSON
}

scenario_plan() {
	wait_for "Ctrl+C"
	keys M-m
	wait_for "mode:plan"
	type_text "add a.txt"
	keys Enter
	wait_for "Approve the plan"
	settle
	capture plan "the plan to approve"
	keys y
	wait_for "all done"
	settle
	capture plan "approved and implemented"
}

# The model asks: a dialog per question, answered with the keys.
setup_question() {
	extra_args="-- -faux-script $tmp/script.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "asking", "tool_calls": [{"id": "q1", "name": "ask_user", "arguments": {"questions": [{"question": "Which database?", "header": "Storage", "options": [{"label": "Postgres", "description": "what prod uses"}, {"label": "SQLite"}]}, {"question": "Anything else?"}]}}]},
  {"text": "thanks"}
]
JSON
}

scenario_question() {
	wait_for "Ctrl+C"
	type_text "go"
	keys Enter
	wait_for "Which database"
	settle
	capture question "first question"
	keys Down Enter
	wait_for "Anything else"
	type_text "keep it small"
	settle
	capture question "second question"
	keys Enter
	wait_for "thanks"
	settle
	capture question "answered"
}

scenario_paste() {
	wait_for "Ctrl+C"
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
	wait_for "Ctrl+C"
	type_text "before"
	keys Enter
	wait_for "faux reply"
	pkill -f "serve -faux -cwd $tmp/cwd"
	wait_for "reconnected to the backend"
	settle
	capture reconnect "after the backend was killed"
	type_text "after"
	keys Enter
	wait_for "▌ after"
	settle
	capture reconnect "prompt works again"
}

# /fallback and /default-dir through the real TUI, then a run whose model's
# usage runs out hands over to the next model of the chain.
setup_fallback() {
	extra_args="-- -faux-script $tmp/script.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "", "stop_reason": "error", "error": "HTTP 429: The usage limit has been reached (usage limit reached)"},
  {"text": "carried on"}
]
JSON
}

scenario_fallback() {
	wait_for "Ctrl+C"
	type_text "/fallback deepseek/deepseek-flash anthropic/claude-fable-5-1"
	# Esc closes the completion, so Enter runs the line as typed.
	keys Escape
	keys Enter
	wait_for "fallback: "
	type_text "/default-dir $tmp/cwd"
	keys Escape
	keys Enter
	wait_for "default directory: "
	settle
	capture fallback "chain and default directory set"
	type_text "go"
	keys Enter
	wait_for "carried on"
	settle
	capture fallback "handed over"
}

# Alt+S sends the line as typed while the completion highlights a
# subdirectory.
scenario_send_raw() {
	mkdir -p "$tmp/cwd/sub/deeper"
	wait_for "Ctrl+C"
	type_text "/cd $tmp/cwd/sub/"
	wait_for "deeper/"
	settle
	capture send_raw "the popup highlights a directory"
	keys M-s
	wait_for "cwd changed"
	settle
	capture send_raw "Alt+S: /cd to what was typed"
}

# A confirmation that arrives while the sessions picker is open belongs to
# that session: switching away drops it, switching back brings it back
# (get_pending).
setup_switch_pending() {
	extra_args="-- -faux-script $tmp/script.json"
	mkdir -p "$tmp/home/.prigh"
	echo '{"approval_mode": "ask", "permissions": {"allow": ["bash(sleep:*)"]}}' >"$tmp/home/.prigh/config.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "in the first session"},
  {"text": "waiting", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "sleep 2"}}]},
  {"text": "now", "tool_calls": [{"id": "c2", "name": "bash", "arguments": {"command": "rm -rf build"}}]},
  {"text": "all done"}
]
JSON
}

scenario_switch_pending() {
	wait_for "Ctrl+C"
	type_text "hello"
	keys Enter
	wait_for "in the first session"
	type_text "/new"
	keys Enter
	wait_for "new session"
	type_text "go"
	keys Enter
	wait_for "sleep 2"
	type_text "/sessions"
	keys Enter
	wait_for "Sessions"
	# The confirmation arrives behind the picker.
	wait_for "to answer:1"
	keys Down
	settle
	capture switch_pending "the picker, a confirmation waiting behind it"
	keys Enter
	wait_for "▌ hello"
	settle
	capture switch_pending "switched: no dialog from the other session"
	type_text "/sessions"
	keys Enter
	wait_for "Sessions"
	keys Up Enter
	wait_for "Run bash: rm -rf build"
	settle
	capture switch_pending "back: its confirmation again"
	keys y
	wait_for "all done"
	settle
	capture switch_pending "answered"
}

# A dialog open when the backend dies closes (the reason's draft comes
# back); the respawned backend has no run waiting, so none reopens.
setup_reconnect_dialog() {
	extra_args="-- -faux-script $tmp/script.json"
	mkdir -p "$tmp/home/.prigh"
	echo '{"approval_mode": "ask"}' >"$tmp/home/.prigh/config.json"
	cat >"$tmp/script.json" <<'JSON'
[
  {"text": "asking", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "make deploy"}}]},
  {"text": "never"}
]
JSON
}

scenario_reconnect_dialog() {
	wait_for "Ctrl+C"
	type_text "go"
	keys Enter
	wait_for "Run bash: make deploy"
	keys r
	wait_for "Why not"
	type_text "not on a Fri"
	settle
	capture reconnect_dialog "typing the reason"
	pkill -f "serve -faux -cwd $tmp/cwd"
	wait_for "reconnected to the backend"
	settle
	capture reconnect_dialog "reconnected: the dialog closed"
}

run_scenario "$1"
