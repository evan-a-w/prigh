prigh is a coding agent. Read `README.md` for what it does and
`ARCHITECTURE.md` for how it fits together; `DOCKER.md` covers the container.

## Layout and builds

| Directory | What | Toolchain | Build and test |
|---|---|---|---|
| `backend/` | the agent, providers, RPC/web servers (`prigh`); `lib/` is grouped in subdirectories (see `backend/AGENTS.md`) | OxCaml 5.2, Eio | `nix develop`, then `cd backend && dune build @runtest` |
| `frontend/` | the frontends' dune project: `protocol/`, `client/` (+ `client/unix/`) and `ui/` shared, `tui/` and `web/` | OxCaml 5.2, Bonsai | `nix develop`, then `cd frontend && dune build @runtest` |
| `frontend/tui/` | the terminal frontend (`prigh-tui`): `lib/`, `bin/`, `test/`, `e2e/`, `tmux-test/` | OxCaml 5.2, Bonsai_term | `cd frontend && dune build @tui/test/runtest` (unit tests only) |
| `frontend/web/` | prigh-web, the browser frontend: `lib/`, `app/`, `bin/`, `test/`, `e2e/` | OxCaml 5.2, Bonsai_web | `cd frontend && dune build @web/test/runtest ./web/bin/site`; e2e: `bash frontend/web/e2e/prigh_web.sh` (backend built, `PLAYWRIGHT_MODULE`/`PLAYWRIGHT_BROWSERS_PATH` set) |
| `docker/` | the image's entrypoint (`prigh-docker`) | bash | `bash docker/test-prigh-docker.sh` (`-promote` re-records) |
| `nix/` | pins and patches for the flake | | see `nix/README.md` |

- The backend and the TUI share one OxCaml toolchain and one dev shell
  (`nix develop`; `.#backend` is an alias of it). They are separate dune
  projects: run dune in `backend/` or `frontend/`.
- Backend tests use Eio's io_uring backend where the kernel allows it;
  `EIO_BACKEND=posix` selects the poll-based one (the only one on macOS).
- OxCaml has bugs we work around (a codegen bug, a signal race that hangs
  eio_posix, packaging gaps): "Known OxCaml issues" in `nix/README.md`
  lists them. Check it when something misbehaves only under OxCaml or on
  the posix backend, add new ones there, and recheck them when upgrading
  OxCaml.
- `frontend/`'s `@runtest` includes the TUI's e2e and tmux tests, which run
  `backend/_build/default/bin/main.exe`: build the backend first (`dune
  build`, not just `@runtest`, which does not rebuild `main.exe`). Dune does
  not see the backend binary as a dependency, so after changing only the
  backend, clear the stale results (`rm -rf
  frontend/_build/default/frontend/tui/tmux-test/.cram.run.t`) or the tmux tests report the
  old run. They need
  a terminal multiplexer (`tmux`) and can be slow; `dune build
  @tui/test/runtest` runs only the TUI's unit tests. `$PRIGH_BACKEND` overrides that path: unset it
  (e.g. when working inside prigh, which sets it) to test your build.
- prigh-web's browser e2e (`frontend/web/e2e`) is Playwright scripts run
  with `node`, not dune; the flake's Linux checks run it (`nix build .#checks.x86_64-linux.web-e2e`). Locally, point
  `PLAYWRIGHT_MODULE` at the playwright package's `index.mjs` and
  `PLAYWRIGHT_BROWSERS_PATH` at its browsers (both in the Nix store), and
  look at the screenshots (`SHOTS=dir`) as well as the text diff.
- The flake uses the binary cache `prigh.cachix.org`; pass
  `--accept-flake-config` to Nix in non-interactive use.
- Only have one dune process running at a time, otherwise it hangs. E.g. if
  you are running tests, don't also `dune exec`.
- Every new RPC field or method must be mirrored by the frontends that use it
  (`frontend/protocol/`, used by the TUI and prigh-web); the e2e tests guard the
  contract.

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
files in `frontend/` by hand, in the same style.

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
  sees (the `H` harness in `frontend/tui/test/test_app.ml`); `frontend/tui/e2e` and
  `frontend/tui/tmux-test` check the real binaries.
- prigh-web: drive `App.update` through the `Harness` in
  `frontend/web/test` and print the rendered HTML/text; the Playwright e2e
  (`frontend/web/e2e`) checks real browsers.

## UI principles

The UX reference is pi (https://github.com/evan-a-w/pi). In every frontend:

- Never make the user retype what we just showed them: lists are pickers with
  fuzzy filtering, or the argument accepts what was displayed.
- Every error tells you what to do next (closest matches, the command to run).
- State is always visible: the status line shows model, thinking, context,
  cost, user, queued messages, agents, and any mode the UI is in (prigh-web
  spreads these over the composer's chips and the status card at the foot
  of the sidebar, or a slim bar over the chat when the sidebar is closed or
  on a phone, and shows the user under that card, with the settings gear,
  and on its settings page).
- Dialogs own the keyboard until closed; Esc closes without side effects,
  Enter accepts. Every binding is in the keymap, `/help` and a test.
- Every action is acknowledged within one frame.
- Switching sessions or users resets everything that belongs to the old one.

## Pull requests

All changes go through pull requests on GitHub (`evan-a-w/prigh`): never
commit or push to `main` directly. Work on a branch (`feat/...`,
`fix/...`, `docs/...`), ideally in its own `git worktree` so parallel work
does not collide, push it and open a PR with `gh pr create` (`nix run
nixpkgs#gh -- pr create` if `gh` is not installed). One PR per coherent
change; its description says what changed and why, how it was tested
(which suites ran and passed), and anything left for later. Keep PRs
up to date with `main` (rebase or merge) and the suites passing; only
merge when asked.

## Shared machine

You run on the user's own machine, next to their work. Never kill, stop or
reset anything you did not start: no `tmux kill-server` or `kill-session`
on the default socket (tests use a private one: `tmux -L <name>`), no
`pkill`/`killall` by name, no killing prigh backends, tool hosts or editors
you did not launch. Kill only processes you started, by their pid or
private socket.

## General

Eagerly remove dead code, unless it has a good chance of being useful in
future and doesn't add complexity, cost performance or prevent optimisations.

Only write comments for things that are truly hard to understand without
them. Don't restate what can be intuited from context, names or code.

If you see that I have changed a module, do not revert those changes, but
apply the ideas elsewhere. For instance, I might change the API slightly to
make it cleaner, or remove/reword comments, and you should respect that.

Keep docs current: update `README.md`, `ARCHITECTURE.md`, `DOCKER.md` or
`nix/README.md` when behaviour they describe changes, and this file when
the conventions or the workflow change. Don't add plan or handoff documents
to the repository.
