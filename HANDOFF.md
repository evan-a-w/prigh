# HANDOFF — state of the prigh frontend-on-Bonsai spike

Written mid-task for another agent to pick up. Read `ARCHITECTURE.md` (current
system), `UX_PLAN.md` (UI principles/tests we must uphold) and this file.

## What is done and committed

- Backend (OCaml, vanilla 5.3 switch `prigh`, Eio) has full provider login:
  Anthropic (Claude Pro/Max OAuth + API key), OpenAI (API key), OpenAI Codex
  (ChatGPT OAuth), DeepSeek (API key); Anthropic Messages + OpenAI Responses
  providers; pi-compatible `~/.config/prigh/auth.json`; `login/logout/auth`
  CLI and RPC methods. All in `backend/`, tests pass (`dune build @runtest`;
  two pre-existing flaky ordering diffs in `test_session`/`test_rpc`
  "list sessions", same-millisecond session stamps — ignore or fix by making
  stamps strictly increasing in `Session.create`).
- `Model.all` now mirrors pi's full catalog for the four providers (63 models;
  copied from `~/dev/pi/packages/ai/src/providers/data/{anthropic,openai,openai-codex,deepseek}.json`).
  Uncommitted: `backend/lib/model.ml`, `backend/bin/main.ml` (error lists
  keys), `prigh` launcher script, README tweak, `UX_PLAN.md`. Commit them.
- User's pi credentials were copied to `~/.config/prigh/auth.json` (anthropic
  + openai-codex OAuth). Never read/print that file.
- `./prigh` at repo root builds backend + TS frontend and runs the TUI.

## Decision taken

Replace the TypeScript TUI with an OCaml frontend on **bonsai_term**, written
so the logic is platform-agnostic (later: bonsai_web). Design agreed with the
user (see the "headless components" discussion, summarised here):

```
prigh_protocol   wire types + jsonaf, extracted from backend Rpc_json (shared lib)
prigh_client     Async RPC client over abstract Transport (stdio now, websocket later)
prigh_ui         Bonsai components with NO platform code:
                   Intent.t (input vocabulary), Keymap (tested once, used by both platforms),
                   Mode.t state machine (Editor | Picker | Login_prompt | Confirm),
                   headless Editor / Picker(fuzzy) / Transcript / Login_flow / Status
                   exposing state + inject, and Content.t (styled text blocks) for
                   things that are the same on both platforms
prigh_ui_term    Bonsai_term views + Event.t -> Intent + spawns backend
prigh_ui_web     later: Bonsai_web views + websocket
```

Testing: `Bonsai_test.Handle` expect tests on headless components (inject
intents, print state/diffs), term screen snapshots via notty dumb renderer
(or bonsai_term test support if it exists), keymap coverage test, protocol
round-trips, e2e against `main.exe serve -faux` with an isolated `-auth-file`.

## Spike findings so far (steps 1–4 from the plan)

1. **bonsai_term requires OxCaml.** Only branch is `oxcaml`; depends on
   `unboxed_datatypes`, conflicts with `oxcaml-compiler < 5.2.0minus39`. Not on
   default opam. Available from the `ox` opam repo
   (`git+https://github.com/oxcaml/opam-repository.git`, already added to the
   user's opam as repo `ox`), version `v0.18~preview.130.106+341` (latest).
   API: app is `dimensions:Dimensions.t Bonsai.t -> local_ Bonsai.graph ->
   View.With_handler.t Bonsai.t`; `View.t` is notty-style (`text`, `vcat`,
   `hcat`, `zcat`, `pad`, `crop`, `center`, colours, `uchar_tty_width`); no
   focus model (app routes `Event.t` itself); Async + notty_async; fullscreen.
2. **Eio exists on OxCaml** (`eio 1.3+ox` installed in the user's `oxsat`
   switch), and `cohttp-eio`, `tls-eio`, `ca-certs`, `jsonaf`,
   `ppx_jsonaf_conv`, `digestif`, `base64`, `ohex`, `mirage-crypto-rng`,
   `expect_test_helpers_core` all resolve in the ox repo. So a **single OxCaml
   switch for backend + frontend is plausible** — not yet verified by building
   the backend there.
3. Created switch **`prigh-ox`** (`opam switch create prigh-ox --repos
   ox,default --packages ocaml-variants.5.2.0+ox,ocaml-options-vanilla`).
   Compiler build succeeded (~25 min on 3 cores).
4. **Root cause found for the menhir failure — it is an OxCaml codegen bug,
   not a menhir problem.** In `backend/amd64/cfg_selection.ml`,
   `pseudoregs_for_operation` grouped `Ifloatarithmem` with the two-address
   `Floatop` case and returned `[| res.(0); arg.(1) |]`. When the memory
   operand uses a two-register addressing mode (`Iindexed2scaled`, which is
   exactly what a plain `r.(i)` on a `float array` produces), the second
   addressing register `arg.(2)` is dropped and the emitter raises
   `Invalid_argument("index out of bounds")`. Minimal repro:
   `let f (r:float array) (i:int) (j:int) = r.(i) +. r.(j)`.
   - Only bites on a **non-AVX baseline target**. `-favx` hides it, but this
     machine is a QEMU CPU with **no AVX** (`sse4_2` only), so AVX binaries
     would SIGILL. The correct fix is to copy the whole arg array like
     upstream OCaml (see the local overlay below).
   - Present in `minus39`, `minus40`, `5.4.0-ox2` and still on `main`, so
     bumping the compiler does not help. It affects *any* package doing
     float-array arithmetic at baseline; menhir was just first.
5. Repaired the compiler with a **local opam overlay repo `oxpatched`**
   (`~/.opam/repo/ox-patched`, rank 1 for switch `prigh-ox`). It adds
   `packages/oxcaml-compiler/oxcaml-compiler.5.2.0minus39/files/fix-floatarithmem.patch`
   and lists it in `patches`, so `opam reinstall oxcaml-compiler` rebuilds the
   patched compiler. Rebuild is running now (~25 min; log
   `/tmp/oxcaml-reinstall.log`). After it lands, re-run the package install:
   `OPAMSOLVERTIMEOUT=900 opam install --switch=prigh-ox -y
   bonsai_term.v0.18~preview.130.106+341 bonsai_test notty_async
   expect_test_helpers_core ppx_jsonaf_conv` (`/tmp/prigh-ox-install.log`).
   Use ≥600 s solver timeout. ~150 packages from the aborted first pass are
   already installed (`core`, `async`, `jsonaf`, `ppx_jsonaf_conv`,
   `notty-community 0.2.4+ox2`, `expect_test_helpers_core`); `bonsai`,
   `bonsai_term`, `js_of_ocaml*` are not yet.
   - Still to check: whether `bonsai.ppx_bonsai` + `ppx_jane` coexist in one
     stanza (their `src/dune` does this, so yes), and whether the public
     bonsai_term ships a test handle (`src/` has `driver.ml`, `loop.ml`,
     `frame_outcome.ml`; there is a `demos/` dir — look for
     `bonsai_term_test` or a `For_testing` in `driver.mli`).

## Status (latest)

Steps 1–3 below are done and the frontend is implemented: see
`FRONTEND_PLAN.md` and `ARCHITECTURE.md` ("Frontend (`tui/`)"). `frontend/`
(TypeScript) and `spike/` are deleted; `./prigh` builds both switches and
runs `tui/_build/default/bin/main.exe`; `flake.nix` builds `tui/`
(`packages.default = prigh_tui`). Test entry points: `cd tui && dune build
@runtest` (unit + Bonsai handle tests) and `dune build @e2e` (real backend).
`ocamlformat 0.26.2+ox2` is installed in `prigh-ox` and pinned for Nix.
Known quirk: the `v0.18~preview` ppx_expect runtime resolves corrected
files against `-source-tree-root`, so `tui/test/dune` passes
`(flags (-source-tree-root .))` to override dune's `..`.

## Remaining work, in order

1. **DONE — `prigh-ox` compiles Bonsai_term.** After the patched compiler
   installed (which triggered a full-switch rebuild — see Environment notes),
   `opam install bonsai_term.v0.18~preview.130.106+341 bonsai_test notty_async
   expect_test_helpers_core ppx_jsonaf_conv` succeeded, including `menhir
   20260209` (the package that originally exposed the bug). A smoke test was
   added at `spike/bonsai_term_hello/` (the upstream hello-world, with a
   `bonsai_term_hello.opam`); `dune build` and `main.exe --help` both work.
2. **DONE — decision: keep two switches.** `backend/` does **not** build under
   OxCaml, for dependency reasons unrelated to the compiler patch:
   - All available `digestif` versions (1.1.2, 1.3.0, 1.3.1) fail to compile
     under OxCaml with modes errors in `src-ocaml/baijiu_*.ml` (e.g.
     `By.blit` has type `bytes @ local -> int -> bytes @ local -> ...` but
     `feed` expects a differently-moded `blit`). There is no `oxcaml-digestif`
     guard in the `ox` repo.
   - Replacing the one `Digestif.SHA256` use (in `lib/pkce.ml`) with
     `Mirage_crypto.Hash.SHA256` gets past that, but `mirage-crypto-rng` in
     `prigh-ox` is `0.11.3`, whose `Mirage_crypto_rng_unix` has no
     `use_default` (the backend uses the 2.x API; the vanilla switch has
     `2.4.0`). Upgrading to `mirage-crypto-rng 2.4.0` pulls in `digestif`
     again via `tls 2.1.2`; the resolver instead picks `tls 0.17.5` +
     `mirage-crypto 0.11.3`, which is self-consistent but not what the backend
     is written against.
   - The temporary backend edits were reverted. Keep `prigh` (vanilla 5.3.0)
     for `backend/`; `prigh_protocol` needs to be a directory vendored into
     both dune projects as planned.
3. **Nix environment (user requirement):** nix is **installed** (multi-user
   Determinate Nix 3.22.5, daemon active, flakes enabled; `/etc/nix/nix.conf`;
   profile at `/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh`).
   `flake.nix`, `nix/prigh-ox-pins.nix`, `nix/fix-floatarithmem.patch` and
   `spike/bonsai_term_hello/` are written and committed. What the flake does:
   - inputs: `nixpkgs`, `flake-utils`, `opam-nix`,
     `ocaml/opam-repository`, `oxcaml/opam-repository` (both `flake = false`).
   - `repos = [ ox-opam-repository opam-repository ]` (ox first for
     precedence); `on.buildOpamProject' { inherit repos; resolveArgs = {
     depopts = false; dev = false; }; } ./spike/bonsai_term_hello query`.
   - the compiler fix is applied as an overlay:
     `oxcaml-compiler.overrideAttrs (oa: { patches = oa.patches ++ [ ./nix/fix-floatarithmem.patch ]; })`
     (opam-nix already has an `oxcaml-compiler` override in
     `src/overlays/ocaml.nix` adding `rsync` + a Makefile patch; ours composes
     with it).
   - `query` pins the ~230 exact versions now installed in the `prigh-ox`
     switch (`nix/prigh-ox-pins.nix`, including `bonsai_term`/`bonsai_test` and
     `eio 1.3+ox`) so opam's solver finishes inside opam-nix's fixed 60 s IFD
     timeout.
   - `packages.default = scope.bonsai_term_hello` (the OxCaml smoke test).
     `devShells.default` inherits that package's inputs plus `bonsai_test`,
     `notty_async`, `expect_test_helpers_core`, `nodejs`, `ripgrep`.
   - **Verified end to end:** the patched `oxcaml-compiler` built in Nix
     (`...-oxcaml-compiler-5.2.0minus39`), `menhir 20260209` built (the
     original failure), `nix build .#packages.x86_64-linux.default` produced
     `result/bin/bonsai_term_hello` and `--help` runs, and `nix develop`
     gives `ocamlopt 5.2.0+ox` + `dune 3.22.2` and compiles
     `let f (r:float array) (i:int) (j:int) = r.(i) +. r.(j)`
     (`NIX_COMPILER_PATCH_OK`). The full first build took ~1 h on 3 cores
     (mostly the compiler + ~175 opam packages); it is all in the Nix store
     now. It needs no `prigh` (backend) build, so it does not hit the
     digestif/mirage-crypto issue.
   - **Known issue / next:** opam-nix evaluates each `opam.json` with an IFD
     derivation, so the *first* evaluation of this scope took ~15 min (cached
     afterwards); the resolver itself succeeds. The robust fix is opam-nix
     materialization (`materializeOpamProject'` + `materializedDefsToScope`)
     with a committed `package-defs.json`, which removes all IFD. Do that once
     the pins are final. `nix eval
     .#packages.x86_64-linux.default.drvPath` is the quick check. `flake.lock`
     is not generated yet (run `nix flake lock`).
   - A `ocaml-lsp-server`/`ocamlformat` for the OxCaml shell is still to be
     tested (the `ox` repo ships `oxcaml-ocamlformat` guards; the plain
     `ocamlformat` may resolve).
4. Write the frontend plan (`FRONTEND_PLAN.md`) with the user: milestones
   protocol extraction → Async client → Mode/Keymap/Editor/Transcript headless
   + term views (parity with today's TUI) → Picker and `UX_PLAN.md` P0
   (`/model` picker, display-name/prefix model matching + "did you mean" in
   backend `set_model`) → delete `frontend/` (TypeScript).
5. Implement it.

## Environment notes

- opam is at `~/.local/bin/opam`; `export PATH=~/.local/bin:$PATH; eval
  $(opam env --switch=<name> --set-switch)`.
- Switches: `prigh` (vanilla 5.3.0, backend builds/tests here), `oxsat`
  (user's OxCaml switch — do not modify), `prigh-ox` (new, for the spike).
- `prigh-ox` carries the `oxpatched` opam repo (rank 1) with the
  `fix-floatarithmem.patch` for `oxcaml-compiler`. Keep it in sync with the
  Nix setup. To rebuild the compiler: `opam reinstall --switch=prigh-ox
  oxcaml-compiler` (25 min).
- Rebuilding the compiler in opam is a **full-switch operation**: `opam
  reinstall oxcaml-compiler` removes every package that depends on the
  compiler and reinstalls the whole world (~150 packages recompile). Budget
  an hour, not 25 min. The patched compiler itself builds fine and now
  compiles `let f (r:float array) (i:int) (j:int) = r.(i) +. r.(j)`.
- Nix: source `/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh`
  (or use `/nix/var/nix/profiles/default/bin/nix`); sudo is available with the
  user-provided password (prefix commands with `printf 'ubu\n' | sudo -S -p ''`).
  Flake entry points: `nix build .#packages.x86_64-linux.default` (the
  bonsai_term hello smoke test) and `nix develop` (OxCaml toolchain +
  bonsai_term); `nix eval .#packages.x86_64-linux.default.drvPath` checks
  evaluation. This host is a QEMU CPU **without AVX** (`sse4_2` only) — never
  enable `-favx`/AVX for things that will run here.
- `backend/AGENTS.md` has the coding conventions (mli for every module,
  expect tests, ocamlformat after edits, one dune process at a time).
- pi source for reference: `~/dev/pi` (TS). Its TUI components
  (`packages/coding-agent/src/modes/interactive/components/`, `packages/tui`)
  are the UX reference for pickers/dialogs.
