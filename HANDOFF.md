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
4. Package install into `prigh-ox` is **in progress / partially failed**:
   - Command: `OPAMSOLVERTIMEOUT=900 opam install --switch=prigh-ox -y
     bonsai_term.v0.18~preview.130.106+341 bonsai_test notty_async
     expect_test_helpers_core ppx_jsonaf_conv` (log: `/tmp/prigh-ox-install.log`).
     The default 60 s solver timeout is too short for this cone; use ≥600.
   - `menhir.20260209` **fails to compile under OxCaml** (compiler
     `Invalid_argument "index out of bounds"` in emit for `base/MCOP.ml`).
     opam kept installing unaffected packages; ~150 installed. `core`, `jsonaf`,
     `ppx_jsonaf_conv`, `notty-community 0.2.4+ox2`, `expect_test_helpers_core`
     are in. `bonsai`, `bonsai_term`, `async` were NOT yet installed when I
     last looked (blocked on menhir → ppx_css/sexp_grammar chain, presumably).
   - Next step: `opam install --switch=prigh-ox menhir.20240715` (older
     versions available: 20240715, 20250903, 20250912 — try oldest first; I
     started this but it was aborted), then re-run the bonsai_term install.
     If menhir can't build at all, check what actually depends on it
     (`opam list --switch=prigh-ox --depends-on menhir`) and whether a
     `+ox`-patched version exists.
   - Also check: whether `bonsai.ppx_bonsai` + `ppx_jane` coexist in one
     stanza (their `src/dune` does this, so yes), and whether the public
     bonsai_term ships a test handle (`src/` has `driver.ml`, `loop.ml`,
     `frame_outcome.ml`; there is a `demos/` dir — look for
     `bonsai_term_test` or a `For_testing` in `driver.mli`).

## Remaining work, in order

1. Finish the `prigh-ox` install (menhir workaround above); build the
   bonsai_term hello-world demo from `janestreet/bonsai_term` `demos/`.
2. Try building `backend/` in `prigh-ox` (`dune build`, `dune build
   @runtest`). If green: one switch for everything, and `prigh_protocol` is a
   plain shared library. If not: keep two switches; `prigh_protocol` as a
   directory vendored into both dune projects.
3. **Nix environment (user requirement):** make the whole toolchain
   reproducible with Nix, using https://github.com/tweag/opam-nix. `nix` is
   *not* installed on this machine yet (`which nix` → nothing) — install it
   first (multi-user Determinate/official installer). Plan: `flake.nix` with
   opam-nix `buildOpamProject'`/`queryToScope` over both the `ox` and default
   opam repositories (opam-nix accepts extra repos via `repos = [ ... ]`
   fetched as flake inputs: `https://github.com/oxcaml/opam-repository` and
   `opam-repository`), pinning `ocaml-variants.5.2.0+ox`, `bonsai_term`,
   `bonsai_test`, `notty_async`, `eio`, `cohttp-eio`, `tls-eio`, `jsonaf`,
   `ppx_jsonaf_conv`, `expect_test_helpers_core`, `ocamlformat`, and
   `menhir` at a version that compiles (see above). Expose a `devShell`
   (dune, ocamlformat, node for the legacy TS frontend until deleted, `rg`
   which the tools need) and packages for the backend and frontend. Expect
   the OxCaml compiler build in Nix to be slow; the `menhir` and any other
   `+ox`-patch packages are the likely friction points. Also record the exact
   opam-nix input revision in `flake.lock`.
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
- `backend/AGENTS.md` has the coding conventions (mli for every module,
  expect tests, ocamlformat after edits, one dune process at a time).
- pi source for reference: `~/dev/pi` (TS). Its TUI components
  (`packages/coding-agent/src/modes/interactive/components/`, `packages/tui`)
  are the UX reference for pickers/dialogs.
