#!/usr/bin/env bash
# Runs the browser e2e (pi_web.mjs) against a freshly started backend and
# diffs the output with pi_web.expected. Needs PLAYWRIGHT_MODULE (the
# playwright package's index.mjs) and PLAYWRIGHT_BROWSERS_PATH, plus:
#   PRIGH_BACKEND      the backend binary (default: the dune build)
#   PRIGH_PI_WEB_ROOT  the built site (default: ../dist)
#   ENGINES            browsers to run (default: chromium firefox)
#   UPDATE=1           rewrite pi_web.expected instead of diffing
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
backend="${PRIGH_BACKEND:-$here/../../backend/_build/default/bin/main.exe}"
root="${PRIGH_PI_WEB_ROOT:-$here/../dist}"
tmp="$(mktemp -d)"
actual="$tmp/actual"
: >"$actual"
server_pid=
cleanup() {
	status=$?
	if [ -n "$server_pid" ]; then kill "$server_pid" 2>/dev/null || true; fi
	if [ "$status" -ne 0 ]; then cat "$tmp/server.err" "$actual" 2>/dev/null || true; fi
	rm -rf "$tmp"
	exit "$status"
}
trap cleanup EXIT

cat >"$tmp/script.json" <<'JSON'
[
  {"text": "let me look", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "printf 'one\\ntwo\\nthree\\n'"}}]},
  {"text": "three lines. writing a note", "tool_calls": [{"id": "c2", "name": "write", "arguments": {"path": "note.txt", "content": "hello\nworld\n"}}]},
  {"text": "now delegating", "tool_calls": [{"id": "s1", "name": "subagent", "arguments": {"task": "count files", "tools": ["ls"]}}]},
  {"text": "child reporting: 1 file"},
  {"text": "all done"}
]
JSON

for engine in ${ENGINES:-chromium firefox}; do
	mkdir -p "$tmp/home-$engine" "$tmp/cwd-$engine"
	HOME="$tmp/home-$engine" "$backend" serve -pi-web 127.0.0.1:0 -pi-web-root "$root" \
		-faux-script "$tmp/script.json" -token 'sekrit&x=y' -cwd "$tmp/cwd-$engine" \
		>"$tmp/server.out" 2>"$tmp/server.err" &
	server_pid=$!
	for _ in $(seq 1 200); do
		grep -q "pi-web on" "$tmp/server.err" 2>/dev/null && break
		sleep 0.05
	done
	url="$(sed -n 's/^prigh: pi-web on //p' "$tmp/server.err" | head -1)"
	test -n "$url"
	curl --fail --silent "${url}theme/dark.json" >/dev/null
	TEST_CWD="$tmp/cwd-$engine" node "$here/pi_web.mjs" "$url" 'sekrit&x=y' "$engine" >>"$actual"
	kill "$server_pid"
	wait "$server_pid" 2>/dev/null || true
	server_pid=
done

if [ "${UPDATE:-}" = 1 ]; then
	cp "$actual" "$here/pi_web.expected"
	echo "updated $here/pi_web.expected"
else
	diff -u "$here/pi_web.expected" "$actual"
fi
