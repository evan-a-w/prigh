#!/usr/bin/env bash
# Runs docker/prigh-docker against fake roots, with its privileged steps
# printed instead of run (PRIGH_DOCKER_DRY_RUN), and compares the output with
# test-prigh-docker.expected. `-promote` accepts the new output.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
entry="$here/prigh-docker"
expected="$here/test-prigh-docker.expected"
work=$(mktemp -d "$here/../.prigh-docker-test.XXXXXX")
trap 'rm -rf "$work"' EXIT
r="$work/root"

# A fresh fake root: the image's accounts and skeleton, empty volumes.
fake_root() {
	rm -rf "$r"
	mkdir -p "$r/etc/skel" "$r/home" "$r/workspace" "$r/run"
	cat >"$r/etc/passwd" <<'EOF'
root:x:0:0:root:/root:/bin/bash
daemon:x:1:1:daemon:/usr/sbin:/usr/sbin/nologin
bin:x:2:2:bin:/bin:/usr/sbin/nologin
www-data:x:33:33:www-data:/var/www:/usr/sbin/nologin
_apt:x:42:65534::/nonexistent:/usr/sbin/nologin
nobody:x:65534:65534:nobody:/nonexistent:/usr/sbin/nologin
prigh:x:1000:1000::/home/prigh:/bin/bash
EOF
	cat >"$r/etc/group" <<'EOF'
root:x:0:
daemon:x:1:
staff:x:50:
users:x:100:
nogroup:x:65534:
prigh:x:1000:
EOF
	echo '# skeleton' >"$r/etc/skel/.bashrc"
	: >"$r/owners"
	nix_seed 1 aaaa-glibc-2.40 bbbb-nix-2.35 cccc-curl-8.9
}

# nix_seed ID PATH...: the image's Nix seed: store paths with some
# content, the first two of them GC roots.
nix_seed() {
	local s="$r/opt/nix-seed" id="$1" p
	shift
	rm -rf "$s"
	mkdir -p "$s/store"
	for p in "$@"; do
		mkdir -p "$s/store/$p/bin"
		echo "$p" >"$s/store/$p/bin/tool"
	done
	printf '/nix/store/%s\n' "$1" "$2" >"$s/roots"
	echo "seed-$id" >"$s/id"
	printf '%s\n' "$@" >"$s/registration"
}

# The /nix volume: store entries, valid paths (fake database), GC roots, stamp.
nix_volume() {
	local p
	echo "--- /nix/store"
	(cd "$r/nix/store" && find . -mindepth 1 -maxdepth 1 | sed 's|^\./||' | LC_ALL=C sort)
	files /nix/var/nix/db/valid
	echo "--- $(basename "$r/nix/var/nix/gcroots/prigh")/ GC roots"
	for p in "$r"/nix/var/nix/gcroots/prigh/*; do
		echo "${p##*/} -> $(readlink "$p")"
	done
	files /nix/var/prigh/seed
}

# owned PATH UID: PATH (created as a directory if missing) is owned by UID.
owned() {
	mkdir -p "$r$1"
	echo "$1 $2" >>"$r/owners"
}

section() {
	printf '\n=== %s\n' "$*"
}

# pd [VAR=VALUE]... [ARGS]: the entrypoint in a clean environment.
pd() {
	local vars=() out status
	while [[ ${1:-} =~ ^[A-Z_]+= ]]; do
		vars+=("$1")
		shift
	done
	echo "\$ ${vars[*]}${vars[*]:+ }prigh-docker $*"
	out=$(env -i PATH="$PATH" LANG=C.UTF-8 PRIGH_DOCKER_ROOT="$r" PRIGH_DOCKER_DRY_RUN=1 \
		"${vars[@]}" bash "$entry" "$@" 2>&1) && status=0 || status=$?
	printf '%s\n' "$out" | sed -e "s|$work|WORK|g" -e "s|${PATH//|/\\|}|\$PATH|g"
	[ "$status" = 0 ] || echo "[exit $status]"
}

files() {
	local f
	for f in "$@"; do
		echo "--- $f"
		cat "$r$f"
	done
}

# Directories under /home and /workspace with their mode and (fake) owner.
dirs() {
	local p owner
	echo "--- dirs (mode owner path)"
	while read -r p; do
		p="${p#"$r"}"
		owner=$(awk -v p="$p" '$1 == p { u = $2 } END { print (u == "" ? 0 : u) }' "$r/owners")
		printf '%s %s %s\n' "$(stat -c %a "$r$p")" "$owner" "$p"
	done < <(find "$r/home" "$r/workspace" -maxdepth 1 \( -type d -o -type l \) | sort)
}

run_tests() {
	section "fresh volumes: two users, alice a superuser, per-user and shared GitHub tokens"
	fake_root
	owned /home/prigh 1000
	pd PRIGH_TOKENS=" alice=tok-a , bob=tok-b" PRIGH_SUPERUSERS=alice PRIGH_GH_TOKENS=bob=gh-b \
		GH_TOKEN=gh-shared PRIGH_GIT_NAME="Ada L" PRIGH_GIT_EMAIL=ada@example.com PRIGH_ARGS="-model m"
	files /run/prigh/extrausers/passwd /run/prigh/extrausers/group /run/prigh/extrausers/shadow
	dirs
	nix_volume

	section "commands in the running container (docker compose exec)"
	pd users
	pd as alice env
	pd TERM=xterm as alice env
	pd GH_TOKEN=gh-shared PRIGH_GH_TOKENS=bob=gh-b as bob git push
	pd as prigh bash
	pd as root bash
	pd as mallory bash
	pd as alice
	pd PRIGH_DOCKER_UID=10001 as bob id
	pd PRIGH_DOCKER_UID=10001 as alice id
	pd ssh-key
	pd ssh-key bob
	pd login alice anthropic -method api_key
	pd login default openai
	pd login alice
	pd PRIGH_TOKENS=alice=tok-a tui
	pd PRIGH_MODE=web,server tui -session s1
	pd PRIGH_MODE=pi-web tui
	pd tui -tools remote
	pd tui -tools local
	pd PRIGH_TOKEN=tok tui
	pd prigh sessions list
	pd PRIGH_DOCKER_UID=1000 prigh sessions list

	section "restart: tokens reordered, a new user, a removed user's home still there"
	owned /home/zed 10002
	pd PRIGH_TOKENS=carol=tok-c,bob=tok-b,alice=tok-a PRIGH_SUPERUSERS="alice, bob" PRIGH_MODE=server,web
	files /run/prigh/extrausers/group
	pd users

	section "Nix: restart with the same image, then an upgrade"
	pd PRIGH_TOKEN=tok
	nix_seed 2 dddd-glibc-2.41 bbbb-nix-2.35 eeee-openssl-3
	pd PRIGH_TOKEN=tok
	nix_volume

	section "Nix: a store path left unregistered (interrupted copy) is replaced"
	rm "$r/nix/var/prigh/seed"
	sed -i '/eeee-openssl-3/d' "$r/nix/var/nix/db/valid"
	echo partial >"$r/nix/store/eeee-openssl-3/bin/tool"
	mkdir "$r/nix/store/.prigh-seed-dddd-glibc-2.41"
	pd PRIGH_TOKEN=tok
	cat "$r/nix/store/eeee-openssl-3/bin/tool"
	nix_volume

	section "Nix: other commands seed a fresh volume first (docker compose run)"
	fake_root
	owned /home/prigh 1000
	pd prigh sessions list
	nix_volume
	pd prigh sessions list
	pd PRIGH_DOCKER_UID=1000 prigh sessions list

	section "uids from existing directories"
	fake_root
	sed -i '$a svc:x:10000:10000::/:/bin/false' "$r/etc/passwd"
	owned /home/prigh 1000
	owned /home/alice 1000
	owned /workspace/bob 10007
	owned /home/carol 65534
	owned /home/dave 10003
	owned /workspace/dave 10003
	owned /home/erin 10003
	pd PRIGH_TOKENS=alice=a,bob=b,carol=c,dave=d,erin=e PRIGH_NO_BACKEND_HOST=1
	files /run/prigh/users
	dirs

	section "old layout: the volume was prigh's home; single PRIGH_TOKEN; pi-web only"
	fake_root
	owned /home 1000
	mkdir -p "$r/home/.prigh/sessions" "$r/home/.config/prigh" "$r/home/.ssh"
	touch "$r/home/.bashrc" "$r/home/.config/prigh/auth.json" "$r/home/.ssh/id_ed25519"
	owned /workspace/proj 1000
	owned /workspace/default 1000
	pd PRIGH_TOKEN=tok PRIGH_MODE=pi-web
	(cd "$r/home" && find . | LC_ALL=C sort)
	dirs

	section "old layout and /home/prigh both present"
	fake_root
	owned /home/prigh 1000
	mkdir -p "$r/home/.config"
	pd PRIGH_TOKEN=tok

	section "prigh-web: alone (tool hosts connect to it), and with pi-web"
	fake_root
	owned /home/prigh 1000
	pd PRIGH_TOKEN=tok PRIGH_MODE=prigh-web
	pd PRIGH_TOKEN=tok PRIGH_MODE=pi-web,prigh-web
	pd PRIGH_TOKEN=tok PRIGH_MODE=prigh-web healthcheck

	section "no token: no auth, one tool host as default without a token"
	fake_root
	owned /home/prigh 1000
	pd PRIGH_ALLOW_NO_TOKEN=1 PRIGH_MODE=server PRIGH_SUPERUSERS=
	files /run/prigh/extrausers/passwd

	section "PRIGH_MODE=tui: single user, as prigh"
	fake_root
	owned /home/prigh 1000
	pd PRIGH_MODE=tui PRIGH_TUI_ARGS="-model m" PRIGH_GIT_EMAIL=me@example.com GH_TOKEN=gh
	pd PRIGH_MODE=tui PRIGH_CWD=/workspace/proj
	pd PRIGH_MODE=tui tui
	pd PRIGH_MODE=tui ssh-key
	pd PRIGH_MODE=tui login default anthropic
	pd PRIGH_MODE=tui login alice anthropic
	dirs

	section "errors"
	fake_root
	pd users
	pd as alice id
	pd ssh-key
	pd PRIGH_DOCKER_UID=1000 PRIGH_TOKEN=tok
	pd
	pd PRIGH_TOKENS=root=a
	pd PRIGH_TOKENS=prigh=a
	pd PRIGH_TOKENS=nogroup=a
	pd PRIGH_TOKENS=1abc=a
	pd PRIGH_TOKENS=-x=a
	pd PRIGH_TOKENS=a.b=a
	pd PRIGH_TOKENS=abcdefghijabcdefghijabcdefghijabc=a
	pd PRIGH_TOKENS=alice=a,alice=b
	pd PRIGH_TOKENS=alice=a,secret-without-name
	pd PRIGH_TOKENS=alice=
	pd PRIGH_TOKENS=,
	pd PRIGH_TOKENS=alice=a PRIGH_SUPERUSERS=bob
	pd PRIGH_ALLOW_NO_TOKEN=1 PRIGH_SUPERUSERS=default
	pd PRIGH_TOKENS=alice=a PRIGH_GH_TOKENS=bob=gh
	pd PRIGH_TOKEN=tok PRIGH_MODE=tui,web
	pd PRIGH_TOKEN=tok PRIGH_MODE=bogus
	pd PRIGH_TOKEN=tok PRIGH_MODE=,
	ln -s /home/prigh "$r/workspace/default"
	pd PRIGH_TOKEN=tok
	rm "$r/workspace/default"
	chmod 750 "$r/workspace"
	pd PRIGH_TOKEN=tok
}

actual="$work/actual"
run_tests >"$actual"
if [ "${1:-}" = -promote ]; then
	cp "$actual" "$expected"
	echo "promoted $expected"
elif diff -u "$expected" "$actual"; then
	echo "ok"
else
	echo "FAILED: output differs from $expected (rerun with -promote to accept)" >&2
	exit 1
fi
