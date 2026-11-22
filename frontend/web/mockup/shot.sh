#!/usr/bin/env bash
# Screenshots of the built mockups (see shot.mjs): sets up Playwright and the
# fonts from nixpkgs, then runs shot.mjs with the arguments.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
paths=$(nix build --no-link --print-out-paths --inputs-from "$here" \
  nixpkgs#playwright-test nixpkgs#playwright-driver.browsers \
  nixpkgs#dejavu_fonts nixpkgs#geist-font nixpkgs#inter nixpkgs#nodejs 2>/dev/null)
get() { grep -- "$1" <<<"$paths" | head -1; }
conf=$(mktemp -d)/fonts.conf
cat >"$conf" <<CONF
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <dir>$(get dejavu)/share/fonts</dir>
  <dir>$(get geist-font)/share/fonts</dir>
  <dir>$(get inter)/share/fonts</dir>
  <cachedir>$(dirname "$conf")/cache</cachedir>
</fontconfig>
CONF
export FONTCONFIG_FILE=$conf
export PLAYWRIGHT_MODULE=$(get playwright-test)/lib/node_modules/playwright/index.mjs
export PLAYWRIGHT_BROWSERS_PATH=$(get playwright-browsers)
exec "$(get nodejs)/bin/node" "$here/shot.mjs" "$@"
