# Nix environment notes

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
# then wrap in the file header, drop `oxcaml-*` guards and the menhir
# runtime libraries (they are resolved consistently with `menhir` itself).
```

The current file was generated this way; `menhir`, `menhirCST`, `menhirLib`
and `menhirSdk` are intentionally left unpinned so opam picks a matching set,
and `oxcaml-*` guard/static packages are omitted (their versions are the
`guard`/`enabled` markers, not real versions).

Once the switch contains `bonsai_term`/`bonsai_test`, replace the `"*"`
entries in `flake.nix` with their pinned versions (or drop the `// { ... }`
override entirely).

## Materialization (TODO)

opam-nix turns every package's opam file into a derivation via an IFD
`opam.json`, so the first `nix eval` of the scope takes a long time. The
recommended fix is to materialize:

```sh
nix eval --raw .#lib.x86_64-linux \
  --apply '(m: m.materializeOpamProject { } ./backend { /* query */ })'
```

and commit the resulting JSON, then build the scope with
`materializedDefsToScope`. Do this after the pins are final.
