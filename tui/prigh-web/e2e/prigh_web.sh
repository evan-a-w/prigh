#!/usr/bin/env bash
# Runs the browser e2e (prigh_web.mjs, which starts and restarts the backend,
# then accounts.mjs, the account switcher on a backend with users) in each
# browser and diffs the output with prigh_web.expected. Needs node,
# PLAYWRIGHT_MODULE (the playwright package's index.mjs) and
# PLAYWRIGHT_BROWSERS_PATH, tmux (on the PATH or $PRIGH_TMUX) for the
# terminal panel, plus:
#   PRIGH_BACKEND        the backend binary (default: the dune build)
#   PRIGH_PRIGH_WEB_ROOT the built site (default: the dune build)
#   ENGINES              browsers to run (default: chromium firefox)
#   SHOTS                a directory to save screenshots of each step in
#   UPDATE=1             rewrite prigh_web.expected instead of diffing
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export PRIGH_BACKEND="${PRIGH_BACKEND:-$here/../../../backend/_build/default/bin/main.exe}"
export SITE="${PRIGH_PRIGH_WEB_ROOT:-$here/../../_build/default/prigh-web/bin/site}"
export TOKEN='sekrit&x=y'
tmp="$(mktemp -d)"
actual="$tmp/actual"
: >"$actual"
cleanup() {
	status=$?
	if [ "$status" -ne 0 ]; then cat "$actual"; fi
	rm -rf "$tmp"
	exit "$status"
}
trap cleanup EXIT
test -x "$PRIGH_BACKEND" || { echo "no backend at $PRIGH_BACKEND: build it (cd backend && dune build)" >&2; exit 1; }
test -f "$SITE/main.bc.js" || { echo "no site at $SITE: build it (cd tui && dune build ./prigh-web/bin/site)" >&2; exit 1; }
if [ -n "${SHOTS:-}" ]; then mkdir -p "$SHOTS"; fi

for engine in ${ENGINES:-chromium firefox}; do
	TEST_DIR="$tmp/$engine" node "$here/prigh_web.mjs" "$engine" >>"$actual"
	TEST_DIR="$tmp/$engine" node "$here/accounts.mjs" "$engine" >>"$actual"
done

if [ "${UPDATE:-}" = 1 ]; then
	cp "$actual" "$here/prigh_web.expected"
	echo "updated $here/prigh_web.expected"
else
	diff -u "$here/prigh_web.expected" "$actual"
fi
