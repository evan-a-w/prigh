#!/usr/bin/env bash
# Why pushes to Attic are slow (the attic workflow's last step): how this
# runner reaches the Attic API's NetBird peer (P2P or through a relay, the
# ICE candidates, the latency), then the same NAR downloaded over NetBird
# from the API ($ATTIC_ENDPOINT) and over the internet from the public
# endpoint, 20 s at most each. NetBird much slower than public: the tunnel
# (a relay); both slow: atticd or its storage. $TARGETS (flake attributes):
# the first one Attic has is the NAR downloaded.
set -uo pipefail

public=${ATTIC_PUBLIC:-https://attic.caduc.co/prigh}
endpoint=${ATTIC_ENDPOINT%/}
host=$(printf '%s' "$endpoint" | sed -E 's#^[a-z]+://##; s#[:/].*$##')

echo "== NetBird peer of the Attic API ($host)"
netbird status -d 2>/dev/null |
	awk -v ip="NetBird IP: $host" '
		/^ [^ ].*:$/ { if (inside) exit; block = $0 "\n"; next }
		index($0, ip) { inside = 1 }
		{ block = block $0 "\n" }
		END { if (inside) printf "%s", block }' |
	grep -E 'netbird|Status|Connection type|ICE candidate|Relay server|Latency|Transfer' ||
	echo "(no NetBird peer with IP $host: is ATTIC_ENDPOINT a NetBird address?)"

rate() { # url
	local out status=0
	out=$(curl -sSf -o /dev/null --max-time 20 -w '%{speed_download} %{size_download} %{time_total}' "$1" 2>&1) || status=$?
	# 28: the 20 s ran out, which still measured the rate.
	if [ "$status" != 0 ] && [ "$status" != 28 ]; then
		echo "failed: $(printf "%s\n" "$out" | grep -m1 "curl:")"
		return
	fi
	printf '%s\n' "$out" | tail -1 | awk '{ printf "%.2f MiB/s (%.1f MiB in %.1fs)\n", $1 / 1048576, $2 / 1048576, $3 }'
}

for t in ${TARGETS:-}; do
	p=$(nix eval --raw ".#$t.outPath" 2>/dev/null) || continue
	h=$(basename "$p" | cut -d- -f1)
	nar=$(curl -sf --max-time 10 "$public/$h.narinfo" | sed -n 's/^URL: //p')
	[ -n "$nar" ] || continue
	echo "== downloading $(basename "$p")'s NAR"
	echo "over NetBird (the API): $(rate "$endpoint/prigh/$nar")"
	echo "over the internet:      $(rate "$public/$nar")"
	exit 0
done
echo "== no target is in the cache yet: nothing to download"
