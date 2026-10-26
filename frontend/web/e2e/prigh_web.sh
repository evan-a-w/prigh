#!/usr/bin/env bash
# Runs the browser e2e (prigh_web.mjs, which starts and restarts the backend,
# then accounts.mjs, the account switcher on a backend with users, then
# reconnect.mjs, losing the connection on a phone, then paging.mjs, a long
# session loaded a page at a time, then resilience.mjs, a long session on a
# connection that cannot keep up, then stability.mjs, nothing redrawn or
# shifted that did not change, then themes.mjs, the themes and the user's
# preferences on the backend, then worktrees.mjs, the sidebar's worktrees and
# /worktree in a git repository, then review.mjs, the review page and the
# file browser's diffs and comments, then remote.mjs, the page `prigh web
# -connect` serves for a backend elsewhere) in each
# browser and diffs the output with ENGINE.expected (chromium.expected,
# firefox.expected). The scripts all run at once (JOBS at a time, default:
# the number of CPUs), each with its own backend, browser and directory;
# their outputs are joined in the order above. Needs node,
# PLAYWRIGHT_MODULE (the playwright package's index.mjs) and
# PLAYWRIGHT_BROWSERS_PATH, tmux (on the PATH or $PRIGH_TMUX) for the
# terminal panel, plus:
#   PRIGH_BACKEND        the backend binary (default: the dune build)
#   PRIGH_WEB_ROOT       the built site (default: the dune build)
#   ENGINES              browsers to run (default: chromium firefox)
#   SHOTS                a directory to save screenshots of each step in
#   JOBS                 how many scripts run at once
#   SCRIPT_TIMEOUT       seconds a script may take (default 300)
#   TIMES=1              print how long each script took
#   UPDATE=1             rewrite ENGINE.expected instead of diffing
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export PRIGH_BACKEND="${PRIGH_BACKEND:-$here/../../../backend/_build/default/bin/main.exe}"
export SITE="${PRIGH_WEB_ROOT:-$here/../../_build/default/web/bin/site}"
export TOKEN='sekrit&x=y'
tmp="$(mktemp -d)"
cleanup() {
	status=$?
	rm -rf "$tmp"
	exit "$status"
}
trap cleanup EXIT
test -x "$PRIGH_BACKEND" || { echo "no backend at $PRIGH_BACKEND: build it (cd backend && dune build)" >&2; exit 1; }
test -f "$SITE/main.bc.js" || { echo "no site at $SITE: build it (cd frontend && dune build ./web/bin/site)" >&2; exit 1; }
if [ -n "${SHOTS:-}" ]; then mkdir -p "$SHOTS"; fi

scripts="prigh_web accounts reconnect paging resilience:slow resilience:drops stability themes worktrees review review_git remote"
engines="${ENGINES:-chromium firefox}"
jobs="${JOBS:-$(nproc)}"
run_one() {
	# script:part runs script.mjs ENGINE part
	local engine=$1 script=$2 start=$EPOCHREALTIME status=0
	local file=${script%%:*} part=${script#*:}
	[ "$part" != "$script" ] || part=""
	mkdir -p "$tmp/$engine/$script"
	# A script that hangs fails, with what it printed, instead of hanging the run.
	TEST_DIR="$tmp/$engine/$script" timeout -k 10 "${SCRIPT_TIMEOUT:-300}" \
		node "$here/$file.mjs" "$engine" $part \
		>"$tmp/$engine-$script.out" 2>"$tmp/$engine-$script.err" || status=$?
	# The terminal panel's tmux server outlives a backend that was killed.
	find "$tmp/$engine/$script" -type s -path '*/tmux-*/*' 2>/dev/null | while read -r socket; do
		"${PRIGH_TMUX:-tmux}" -S "$socket" kill-server 2>/dev/null || true
	done
	if [ -n "${TIMES:-}" ]; then
		printf '%-9s %-11s %6.1fs\n' "$engine" "$script" "$(awk "BEGIN { print $EPOCHREALTIME - $start }")" >&2
	fi
	echo "$status" >"$tmp/$engine-$script.status"
}
for engine in $engines; do
	for script in $scripts; do
		while [ "$(jobs -rp | wc -l)" -ge "$jobs" ]; do wait -n || true; done
		run_one "$engine" "$script" &
	done
done
wait
failed=""
for engine in $engines; do
	: >"$tmp/$engine.actual"
	ok=1
	for script in $scripts; do
		cat "$tmp/$engine-$script.out" >>"$tmp/$engine.actual"
		if [ "$(cat "$tmp/$engine-$script.status")" != 0 ]; then
			failed="$failed $engine/$script"
			ok=""
			echo "--- $engine $script failed (status $(cat "$tmp/$engine-$script.status")); its output and stderr:" >&2
			tail -20 "$tmp/$engine-$script.out" >&2
			cat "$tmp/$engine-$script.err" >&2
		else
			sed -n "s|^e2e: |$engine $script: |p" "$tmp/$engine-$script.err" >&2
		fi
	done
	if [ "${UPDATE:-}" = 1 ] && [ -n "$ok" ]; then
		cp "$tmp/$engine.actual" "$here/$engine.expected"
		echo "updated $here/$engine.expected"
	else
		diff -u "$here/$engine.expected" "$tmp/$engine.actual" || failed="$failed $engine/diff"
	fi
done
if [ -n "$failed" ]; then
	echo "failed:$failed" >&2
	exit 1
fi
