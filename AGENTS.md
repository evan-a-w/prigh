prigh is a coding agent. Read `README.md` for what it does and
`ARCHITECTURE.md` for how it fits together; `DOCKER.md` covers the container.

## Layout and builds

| Directory | What | Toolchain | Build and test |
|---|---|---|---|
| `backend/` | the agent, providers, RPC/web servers (`prigh`) | OCaml 5.3, Eio | `nix develop .#backend`, then `cd backend && dune build @runtest` |
| `tui/` | terminal and Bonsai web frontends (`prigh-tui`) | OxCaml 5.2, Bonsai | `nix develop`, then `cd tui && dune build @runtest` |
| `tui/prigh-web/` | prigh-web, the DOM browser frontend (part of `tui/`) | OxCaml 5.2, Bonsai_web | `cd tui && dune build @prigh-web/test/runtest ./prigh-web/bin/site`; e2e: `bash tui/prigh-web/e2e/prigh_web.sh` (backend built, `PLAYWRIGHT_MODULE`/`PLAYWRIGHT_BROWSERS_PATH` set) |
| `pi-web/` | pi's web UI (Preact/TypeScript) on the backend | node 22 | `npm ci && npm run check && npm test && npm run build` |
| `docker/` | the image's entrypoint (`prigh-docker`) | bash | `bash docker/test-prigh-docker.sh` (`-promote` re-records) |
| `nix/` | pins and patches for the flake | | see `nix/README.md` |

- The backend and the TUI need different compilers: build each in its own
  dev shell (the default shell's OxCaml `dune` cannot build the backend).
- `tui/`'s `@runtest` includes the e2e and tmux tests, which run
  `backend/_build/default/bin/main.exe`: build the backend first. They need
  a terminal multiplexer (`tmux`) and can be slow; `dune build @test/runtest`
  runs only the unit tests. `$PRIGH_BACKEND` overrides that path: unset it
  (e.g. when working inside prigh, which sets it) to test your build.
- The browser e2es (`tui/e2e-web`, `tui/prigh-web/e2e`, `pi-web/e2e`) are
  Playwright scripts run with `node`, not dune; the flakes' Linux checks run
  them (`nix build .#checks.x86_64-linux.prigh-web-e2e`). Locally, point
  `PLAYWRIGHT_MODULE` at the playwright package's `index.mjs` and
  `PLAYWRIGHT_BROWSERS_PATH` at its browsers (both in the Nix store), and
  look at the screenshots (`SHOTS=dir`) as well as the text diff.
- The flakes use the binary cache `prigh.cachix.org`; pass
  `--accept-flake-config` to Nix in non-interactive use.
- Only have one dune process running at a time, otherwise it hangs. E.g. if
  you are running tests, don't also `dune exec`.
- Every new RPC field or method must be mirrored by the frontends that use it
  (`tui/protocol/`, `pi-web/src/protocol.ts` via `backend/lib/pi_rpc.ml`);
  the e2e tests guard the contract.

## OCaml conventions

Prefer defining types as a [type t] inside a module with the name of the
type. E.g. [type undo_entry = ...] should be [module Undo_entry = struct type
t = ... end]. Types should typically be [Type.t], not [type type_].

Write explicit interfaces for new files in the mli rather than just creating
an ml file. If the mli and the ml duplicate definitions of module types (e.g.
for functors), use a \*_intf.ml file defining the module types and the
resulting mli module type, include that from the ml, and make the mli simply
[include X_intf.X].

Prefer to modularize code where possible: self-contained functionality goes
in its own module.ml with a [type t] inside, exposing only the minimal
necessary functionality in the .mli.

For common modules in Core, like [Table], [Set] etc., refer to the
re-exported/functor applied modules, like [Int.Table.t]. [Int.Table] (and
equivalent for [Set], [Map], etc.) has the functions specific to [Int] tables,
like [create] and [of_list], but not the ones that are the same for all tables
(e.g. find: use [Hashtbl.find], [Map.find] etc.). Don't write
[Hashtbl.create (module Int)] or [(int, int) Hashtbl.t].

Use the import.ml module to import things.

Run ocamlformat on the files you changed when you finish (`dune fmt` in the
project, or `ocamlformat --inplace FILE`). The dev shells have upstream
ocamlformat, which cannot parse OxCaml syntax (e.g. `local_`): format such
files in `tui/` by hand, in the same style.

## Tests

Test comprehensively, such that we can be fully confident in correctness, and
don't consider work done until tests are present and working.

Prefer expect tests (let%expect_test ...) with human readable output: print
sexps or screens demonstrating state rather than asserting equality with a
hand-written value (e.g. don't just [assert_true] some condition). Look at the
output of `dune build @runtest`, and if the diffs are right, `dune promote`.

- Backend: drive the agent with `Faux_provider` and print event streams; no
  network.
- TUI: drive `App.update` with keys and events and print the screen the user
  sees (the `H` harness in `tui/test/test_app.ml`); `tui/e2e` and
  `tui/tmux-test` check the real binaries.
- pi-web: vitest with happy-dom (`pi-web/test`), against a fake backend where
  state is involved.

## UI principles

The UX reference is pi (https://github.com/evan-a-w/pi). In every frontend:

- Never make the user retype what we just showed them: lists are pickers with
  fuzzy filtering, or the argument accepts what was displayed.
- Every error tells you what to do next (closest matches, the command to run).
- State is always visible: the status line shows model, thinking, context,
  cost, user, queued messages, agents, and any mode the UI is in.
- Dialogs own the keyboard until closed; Esc closes without side effects,
  Enter accepts. Every binding is in the keymap, `/help` and a test.
- Every action is acknowledged within one frame.
- Switching sessions or users resets everything that belongs to the old one.

## General

Eagerly remove dead code, unless it has a good chance of being useful in
future and doesn't add complexity, cost performance or prevent optimisations.

Only write comments for things that are truly hard to understand without
them. Don't restate what can be intuited from context, names or code.

If you see that I have changed a module, do not revert those changes, but
apply the ideas elsewhere. For instance, I might change the API slightly to
make it cleaner, or remove/reword comments, and you should respect that.

Keep docs current: update `README.md`, `ARCHITECTURE.md`, `DOCKER.md` or
`pi-web/README.md` when behaviour they describe changes, and this file when
the conventions or the workflow change. Don't add plan or handoff documents
to the repository.
