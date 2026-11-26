#!/usr/bin/env bash
# Puts the coding-agent CLIs named in $AGENT_CLIS (comma-separated names of
# the flake's agentClis, or none) and their closures into $PRIGH_OUT/store
# (default /seed-agents), for the Dockerfile's nix-build stage: they become
# a layer of the image's Nix seed of their own, which changes only with the
# flake's nixpkgs-agents. $PRIGH_OUT/clis lists the program of each, which
# the image puts on PATH; $PRIGH_OUT/roots their store paths.
set -euo pipefail

src=${PRIGH_SRC:-/deps}
dest=${PRIGH_OUT:-/seed-agents}
mkdir -p "$dest/store"
: >"$dest/clis"
: >"$dest/roots"

system=$(nix eval --raw --impure --expr builtins.currentSystem)
# An expression, not path:$src#agentClis...: nix would first look for that
# under legacyPackages, which needs the sources $src lacks.
clis="(builtins.getFlake \"path:$src\").agentClis.$system"
known=$(nix eval --impure --raw --expr "builtins.concatStringsSep \",\" (builtins.attrNames $clis)")

list=${AGENT_CLIS:-}
IFS=, read -ra given <<<"${list// /}"
names=()
for name in "${given[@]}"; do
	case "$name" in "" | none) continue ;; esac
	if [[ ",$known," != *",$name,"* ]]; then
		echo "prigh: AGENT_CLIS has $name; the agent CLIs are $known (or none)" >&2
		exit 1
	fi
	names+=("$name")
done

for name in "${names[@]}"; do
	pkg=$(nix build --impure --no-link --print-out-paths --expr "$clis.$name")
	exe=$pkg/bin/$(nix eval --impure --raw --expr "$clis.$name.meta.mainProgram")
	home=$(mktemp -d)
	version=$(HOME=$home "$exe" --version 2>/dev/null | tail -1)
	echo "prigh: agent CLI $name: $exe ($version)"
	rm -rf "$home"
	echo "$pkg" >>"$dest/roots"
	echo "$exe" >>"$dest/clis"
done

[ -s "$dest/roots" ] || exit 0
# shellcheck disable=SC2046
cp -a $(nix-store -qR $(cat "$dest/roots")) "$dest/store/"
