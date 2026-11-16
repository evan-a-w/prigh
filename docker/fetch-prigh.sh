#!/usr/bin/env bash
# Puts the flake's backend and frontend (the flake in $PRIGH_SRC, default
# /src) into $PRIGH_OUT/backend and $PRIGH_OUT/frontend (default /out), for
# the Dockerfile's nix-build stage.
#
# When a binary cache in $NIX_SUBSTITUTER (space-separated) has one, signed
# by a trusted key, it downloads just that store path and unpacks it (a few
# hundred MB), not its closure: that holds the whole OxCaml toolchain
# (several GB), which the image does not need. What no cache has is built,
# after printing what that builds and fetches, unless REQUIRE_CACHED=1.
#
# Before any cache, $PRIGH_PREBUILT (default /prebuilt: the Dockerfile's
# `prebuilt` build context, empty unless given) may hold one as
# <store path's basename>.nar (nix-store --dump) with its narHash in
# <basename>.narhash: what whoever builds the image built (CI's own store),
# so trusted like the rest of the build context. Not a binary cache: one
# refuses a path without its references, the whole toolchain.
set -euo pipefail

src=${PRIGH_SRC:-/src}
dest=${PRIGH_OUT:-/out}
prebuilt=${PRIGH_PREBUILT:-/prebuilt}
mkdir -p "$dest"
declare -A out from
missing=()

for name in backend frontend; do
	out[$name]=$(nix eval --raw "path:$src#$name.outPath")
	base=$(basename "${out[$name]}")
	if [ -f "$prebuilt/$base.nar" ] && [ -f "$prebuilt/$base.narhash" ]; then
		from[$name]=prebuilt
		continue
	fi
	for s in ${NIX_SUBSTITUTER:-}; do
		# Checks that s has the path and that its signature is trusted.
		if nix store verify --store "$s" --no-contents "${out[$name]}" >/dev/null 2>&1; then
			from[$name]=$s
			break
		fi
	done
	[ -n "${from[$name]:-}" ] || missing+=("$name")
done

for name in backend frontend; do
	[ -n "${from[$name]:-}" ] || continue
	s=${from[$name]} p=${out[$name]}
	if [ "$s" = prebuilt ]; then
		nar=$prebuilt/$(basename "$p").nar
		want=$(cat "$prebuilt/$(basename "$p").narhash")
		echo "prigh: taking $name $p ($(($(stat -c %s "$nar") / 1024 / 1024)) MiB) from $prebuilt"
	else
		info=$(nix path-info --store "$s" --json --json-format 1 "$p")
		want=$(printf '%s' "$info" | grep -o '"narHash":"[^"]*"' | cut -d'"' -f4)
		size=$(printf '%s' "$info" | grep -o '"narSize":[0-9]*' | cut -d: -f2)
		echo "prigh: fetching $name $p ($((size / 1024 / 1024)) MiB) from $s, without its build closure"
		nar=$dest/$name.nar
		nix store dump-path --store "$s" "$p" >"$nar"
	fi
	got=$(nix hash file --sri --type sha256 "$nar")
	if [ "$got" != "$want" ]; then
		echo "prigh: $p from $s has hash $got, expected $want" >&2
		exit 1
	fi
	nix-store --restore "$dest/$name" <"$nar"
	rm -f "$dest/$name.nar"
done

[ ${#missing[@]} -gt 0 ] || exit 0

targets=()
for name in "${missing[@]}"; do
	targets+=("path:$src#$name")
	echo "prigh: WARNING: no binary cache (${NIX_SUBSTITUTER:-none}) has $name (${out[$name]}): compiling it from source, and the toolchain too where the cache lacks it (an hour or more; PREFETCH_DEPS=1 keeps the toolchain in a layer of its own)" >&2
done
plan=$(nix build --dry-run "${targets[@]}" 2>&1) || {
	printf '%s\n' "$plan" >&2
	exit 1
}
printf '%s\n' "$plan"
if [ "${REQUIRE_CACHED:-0}" = 1 ]; then
	echo "prigh: REQUIRE_CACHED=1: stopping instead of compiling" >&2
	exit 1
fi
for name in "${missing[@]}"; do
	nix build --out-link "$dest/$name" "path:$src#$name"
done
