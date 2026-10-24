# Nix environment notes

## Known OxCaml issues

Bugs and gaps in OxCaml (`5.2.0+ox`, all still present on OxCaml `main`
when found) that we work around. When upgrading OxCaml, recheck each one
and drop the workaround once it is fixed; none has been reported upstream
yet.

| Issue | Kind | Symptom | Workaround | Details |
|---|---|---|---|---|
| `Ifloatarithmem` drops its second addressing register (`backend/amd64/cfg_selection.ml`) | compiler (amd64 codegen, non-AVX) | `Invalid_argument("index out of bounds")` compiling `r.(i) +. r.(j)` on a `float array` | `fix-floatarithmem.patch` on the compiler | below |
| A signal arriving while `caml_enter_blocking_section` releases the domain lock is only handled when the blocking section ends (upstream re-checks after the release) | runtime | event loops woken by signal handlers sleep forever: eio_posix never reaps an exited child (zombie, scheduler idle in `ppoll`); seen as backend tests hanging in `test_git_review.ml` | `iomux 0.4+prigh`; regression test "eio_posix reaps every child promptly" in `backend/test/test_process.ml` | `opam-repository/` below |
| `eio 1.3+ox`'s `url` is a commit of `oxcaml/eio` on no branch | packaging | Nix `fetchGit` cannot fetch it | plain `eio 1.5` | `prigh-ox-pins.nix` below |
| `zarith 1.12+ox3` lacks the `*_unsigned` conversions | packaging (old zarith) | `mirage-crypto-pk 2.x` does not compile | `mirage-crypto-pk 2.4.1+prigh` | `opam-repository/` below |

To check the runtime one on a new OxCaml: run the backend tests with
`EIO_BACKEND=posix` and the iomux patch removed; `test_process.ml` fails
(unreaped children) while the bug is present. The io_uring backend
(eio_linux, the default on Linux) is not affected, so default runs hide it.

## `fix-floatarithmem.patch`

OxCaml `5.2.0+ox` (and `main`) has a native-codegen bug in
`backend/amd64/cfg_selection.ml`: `pseudoregs_for_operation` groups
`Ifloatarithmem` with the two-address `Floatop` case and returns
`[| res.(0); arg.(1) |]`. When the memory operand uses a two-register
addressing mode (`Iindexed2scaled`, produced by a plain `r.(i)` on a
`float array`), the second addressing register `arg.(2)` is dropped and the
emitter raises `Invalid_argument("index out of bounds")`.

Minimal repro:

```ocaml
let f (r : float array) (i : int) (j : int) = r.(i) +. r.(j)
```

It only triggers on a non-AVX baseline target; `-favx` hides it but would
emit AVX instructions. The patch makes the `Ifloatarithmem` case copy the
whole arg array like upstream OCaml does. `flake.nix` applies it with an
`overrideAttrs` overlay; the opam switch applies it via the `oxpatched`
overlay repo.

## `prigh-ox-pins.nix`

The pin set exists because opam-nix's resolver runs in an IFD derivation
with a fixed 60 s `OPAMSOLVERTIMEOUT`. Searching the whole merged
`oxcaml/opam-repository` + `ocaml/opam-repository` against a query for the
full toolchain times out. Pinning the exact versions from the working opam
switch (plus the OxCaml preview releases we need) makes the solve trivial.

Regenerate it after changing the `prigh-ox` switch:

```sh
export PATH=~/.local/bin:$PATH
opam list --switch=prigh-ox --installed --columns=name,version \
  | awk 'NR>1 && $2 != "guard" && $2 != "enabled" { print "  " $1 " = \"" $2 "\";" }' \
  > /tmp/pins-body
# then wrap in the file header and drop the `oxcaml-*` guard/static packages.
```

The file was generated this way from the complete switch; it includes
`bonsai_term`/`bonsai_test`, `menhir 20260209`, etc. `oxcaml-*`
guard/static packages are omitted because their "versions" are the
`guard`/`enabled` markers, not real versions.

Both the TUI and the backend resolve against the same pins, so the
backend's dependencies are in it too, edited by hand since: the TLS/crypto
stack the backend uses (`tls-eio 2.1.3`, `mirage-crypto 2.4.1`, `ca-certs
1.0.3`, `kdf`, `digestif`) and `eio 1.5`, the version the backend was
written against. The OxCaml repository's `eio 1.3+ox` is not usable from
Nix: its `url` is a commit of `oxcaml/eio` that is on no branch, so
`fetchGit` cannot fetch it; plain `eio 1.5` builds with OxCaml unchanged.
The `+prigh` versions come from `opam-repository/` (below).

## `opam-repository/`

A small opam repository, first in the flake's `repos` so that its packages
shadow the OxCaml and upstream ones. Each package is the upstream release
plus a patch in its `files/` (listed in `patches` and `extra-files`, with
the patch's sha256), versioned `<upstream>+prigh` and pinned in
`prigh-ox-pins.nix`:

- `digestif 1.3.1+prigh`: OxCaml's `Bytes.blit` takes `local` arguments,
  which does not unify with the plain function type `feed ~blit` expects;
  the patch eta-expands it.
- `mirage-crypto-pk 2.4.1+prigh`: the OxCaml `zarith 1.12+ox3` lacks the
  `*_unsigned` conversions; the values converted are below 2^56, so the
  signed ones give the same result.
- `iomux 0.4+prigh`: a signal that arrives while
  `caml_enter_blocking_section` releases the domain lock is, under the
  OxCaml runtime, only handled when the blocking section ends (upstream
  OCaml re-checks after releasing the lock and handles it before
  blocking). eio_posix learns that a child exited from its SIGCHLD
  handler, so when SIGCHLD landed in that window (typically during the
  lock release's futex syscall) its scheduler slept in `ppoll` forever
  with the child a zombie. The patch makes the stubs not block when the
  domain has been interrupted since (`young_limit` is `UINTNAT_MAX`) and,
  for `ppoll`, keeps signals blocked until `ppoll` atomically unblocks
  them, so a signal can only interrupt the wait. eio_linux waits for
  children on a pidfd and is not affected. `backend/test/test_process.ml`
  ("eio_posix reaps every child promptly") catches a regression.

After changing a patch, update its sha256 in the package's `opam` file
(`sha256sum files/<patch>`); new files must be `git add`ed before the
flake sees them. An opam switch uses it with `opam repository add prigh
nix/opam-repository`.

## Materialization (optional)

opam-nix turns every package's opam file into a derivation via an IFD
`opam.json`, so the first `nix eval` of the scope takes a long time. The
recommended fix is to materialize:

```sh
nix eval --raw .#lib.x86_64-linux \
  --apply '(m: m.materializeOpamProject { } ./frontend { /* query */ })'
```

and commit the resulting JSON, then build the scope with
`materializedDefsToScope`. Do this after the pins are final.
