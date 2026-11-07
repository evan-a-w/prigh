#!/usr/bin/env bash
# The hicolor sizes the Nix package installs, from icon.svg (prigh-web's icon,
# frontend/web/bin/icon.svg). Run from this directory.
set -euo pipefail
cp ../../../frontend/web/bin/icon.svg icon.svg
nix shell --accept-flake-config nixpkgs#resvg -c sh -c '
  for s in 16 24 48 96 256 512; do resvg -w $s -h $s icon.svg ${s}x${s}.png; done'
