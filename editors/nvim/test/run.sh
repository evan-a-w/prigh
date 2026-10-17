#!/usr/bin/env bash
# Runs prigh.nvim's headless tests. PROMOTE=1 re-records the .expected files.
set -euo pipefail
cd "$(dirname "$0")"
exec "${NVIM:-nvim}" --headless -u NONE -i NONE -n -l run.lua "$@"
