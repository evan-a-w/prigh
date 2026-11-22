prigh is a coding agent. Read `README.md` for what it does and
`ARCHITECTURE.md` for how it fits together; `DOCKER.md` covers the container.

## Layout and builds

`./test` is the test suite: run it, and only it, before pushing and before
merging. It builds both projects, then runs the dune tests (`@runtest` in
each) and, alongside them, prigh-web's browser smoke (`smoke.mjs`, one
script, about 3 CPU-seconds) in Chromium; the desktop's `cargo test` too. It
is meant to take under a minute after a change, also on a machine busy with
other work, and repeats nothing it has a result for: dune keeps its test
results, and the browser smoke is skipped when the backend, the site and the
e2e scripts are what they were at its last green run. It enters the dev
shells through saved profiles (`~/.cache/prigh/test-shell`), so it does not
evaluate the flake each time. `./test --full` runs everything again: every
dune test, also those in the slower `@runtest-full` tier, and every browser
e2e script at every size in Chromium, Firefox and WebKit (the desktop app's
engine on Linux and macOS), the whole matrix the flake checks run: run it
only when the user asks for it. Both work from any shell (outside the dev
shell they enter it) and test what they build. Run single suites as below
while iterating.

| Directory | What | Toolchain | Build and test |
|---|---|---|---|
| `backend/` | the agent, providers, RPC/web servers (`prigh`); `lib/` is grouped in subdirectories (see `backend/AGENTS.md`) | OxCaml 5.2, Eio | `nix develop`, then `cd backend && dune build @runtest` |
| `frontend/` | the frontends' dune project: `protocol/`, `client/` (+ `client/unix/`) and `ui/` shared, `tui/` and `web/` | OxCaml 5.2, Bonsai | `nix develop`, then `cd frontend && dune build @runtest` |
| `frontend/tui/` | the terminal frontend (`prigh-tui`): `lib/`, `bin/`, `test/`, `e2e/`, `tmux-test/` | OxCaml 5.2, Bonsai_term | `cd frontend && dune build @tui/test/runtest` (unit tests only) |
| `frontend/web/` | prigh-web, the browser frontend: `lib/`, `app/`, `bin/`, `test/`, `e2e/` | OxCaml 5.2, Bonsai_web | `cd frontend && dune build @web/test/runtest ./web/bin/site`; e2e: `ENGINES=chromium bash frontend/web/e2e/prigh_web.sh` (backend built, `PLAYWRIGHT_MODULE`/`PLAYWRIGHT_BROWSERS_PATH` set) |
| `desktop/` | prigh-desktop, the Tauri v2 window running `prigh desktop`: `core/` (logic), `app/` (shell), `test/` (window and browser window vs a fake sidecar) | Rust, Tauri | `nix develop .#desktop`, then `cd desktop && cargo test`; window: `nix build .#checks.x86_64-linux.desktop-window` |
| `desktop/computer/` | `prigh-computer`, the computer-use helper a tool host started with `-computer` drives (fake, wlroots, X11, Windows backends) | Rust | `cd desktop && cargo test -p prigh-computer`; on headless sway and Xvfb: `nix build .#checks.x86_64-linux.computer-use`; Windows: the `desktop` workflow's Windows job |
| `docker/` | the image's entrypoint (`prigh-docker`) and build step (`fetch-prigh.sh`) | bash | `bash docker/test-prigh-docker.sh` (`-promote` re-records) |
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
  build`, not just `@runtest`, which does not rebuild `main.exe`). They
  depend on it through `$PRIGH_BACKEND_DIGEST`, which `./test` sets to its
  hash: by hand, set it too (`export PRIGH_BACKEND_DIGEST=$(sha256sum
  backend/_build/default/bin/main.exe)`) or a backend change does not run
  them again. The tmux tests need a terminal multiplexer (`tmux`);
  each `.t` file in `tmux-test/` is a group of scenarios, and dune runs the
  groups in parallel. `dune build @tui/test/runtest` runs only the TUI's
  unit tests. `$PRIGH_BACKEND` overrides that path: unset it
  (e.g. when working inside prigh, which sets it) to test your build.
- prigh-web's browser e2e (`frontend/web/e2e`) is Playwright scripts run
  with `node`, not dune; the flake's Linux checks run it (`nix build .#checks.x86_64-linux.web-e2e`). Locally, point
  `PLAYWRIGHT_MODULE` at the playwright package's `index.mjs` and
  `PLAYWRIGHT_BROWSERS_PATH` at its browsers (both in the Nix store: `nix
  build --no-link --print-out-paths --inputs-from . nixpkgs#playwright-test
  nixpkgs#playwright-driver.browsers`) and `FONTCONFIG_FILE` at a
  fontconfig file (without one Chromium renders no text; `./test` writes one,
  as the flake's check does), and look at the screenshots
  (`SHOTS=dir`) as well as the text diff. Its scripts all run at once, each
  with its own backend, browser and directory (as many at a time as the
  CPUs and the available memory allow: `JOBS`, `MEM_PER_JOB` MiB each),
  and print what they see;
  `ENGINE.expected` is each browser's output (`ENGINE.core.expected`:
  `SUITE=core`'s, the smoke alone). They load Playwright through
  `playwright.mjs`, whose contexts block service workers unless a script
  asks for them (`pwa.mjs` allows the worker it tests). `./test` runs only
  `smoke.mjs` (`SUITE=core`), and skips it when the backend, the site and
  the scripts are what they were at its last green run
  (`PRIGH_TEST_E2E=always` runs it anyway). Wait for a condition, not a
  time (`waitForFunction`, `waitFor`): a fixed wait is either slow or flaky
  on a busy machine, and a wait that is meant to time out costs its whole
  timeout.
- The flake uses the binary cache `attic.caduc.co/prigh`, which a
  self-hosted build host fills from every push to `main` (DOCKER.md,
  "Binary cache"); pass
  `--accept-flake-config` to Nix in non-interactive use.
- The dev shell and `./test` turn on dune's shared cache (`DUNE_CACHE=enabled`,
  in `~/.cache/dune`): a new worktree builds in seconds from what the others
  built, rather than minutes. `dune cache trim --size=4GB` keeps it in
  bounds; `DUNE_CACHE=disabled` turns it off.
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

Run `./test` before pushing and before merging; `./test --full` only when
the user asks (see "Layout and builds"). Keep `./test` minimal and under a
minute on a busy machine; `--full` is where depth goes:

- Dune tests: `@runtest` is for unit and expect tests and a cheap check of
  each real binary. A slow, integration-style test whose behaviour cheaper
  tests already pin goes in the `@runtest-full` alias instead (`(alias
  runtest-full)` in place of `runtest`), which only `./test --full` runs.
  Where the slow ones live:
  - Backend: a test taking 100 ms of CPU or 1 s of wall or more goes in
    `backend/test/full/test_x_full.ml` (opening `Test_x` for its helpers),
    and `test_x.ml` keeps the cheap ones, so every area stays in the default
    run. `test/full` and `test/terminal` are vendored directories:
    `@runtest` skips them, `@runtest-full` runs them. ppx_expect rejects
    custom tags, so the tiers are split by directory. To time one test:
    `inline-test-runner.exe inline-test-runner <lib> -source-tree-root .
    -only-test file.ml:LINE`.
  - prigh-web: scale tests (huge replies, trees, file lists) go in
    `frontend/web/test/full/`. The rest run in two node processes: shard a
    lists its modules in `web/test/dune`, shard b takes the others, so a new
    test file needs no entry. `Harness` reuses the last rendering until the
    model changes: change state only through `App.update`.
  - TUI: the default runs the e2e transcript and the tmux group `smoke.t`;
    the other tmux groups and the websocket e2e are `@runtest-full`.
- Browser e2e: `./test` runs `smoke.mjs` alone, the main flows once each,
  one backend, one browser and few page loads. Add a check to it only for a
  main flow that nothing else catches and only a real browser can; anything
  else is a new script or a step in one, in the full suite (`scripts` in
  `frontend/web/e2e/prigh_web.sh`, at every size it needs).
- The dev profile's JavaScript has no source maps, to keep builds and page
  loads cheap (`--profile debug-js` adds them).

Keep tests fast: they wait for what they expect rather than sleeping, and
output does not depend on timing (a test that passes only on an idle
machine is broken).

- Backend: drive the agent with `Faux_provider` and print event streams; no
  network.
- TUI: drive `App.update` with keys and events and print the screen the user
  sees (the `H` harness in `frontend/tui/test/test_app.ml`); `frontend/tui/e2e` and
  `frontend/tui/tmux-test` check the real binaries.
- prigh-web: drive `App.update` through the `Harness` in
  `frontend/web/test` and print the rendered HTML/text; the Playwright e2e
  (`frontend/web/e2e`) checks real browsers.
- prigh-web on phones: every change to prigh-web is tested and its
  screenshots looked at on phones too, not just a desktop: at least 390x844
  and 320x640, portrait, with `isMobile` and `hasTouch` (Firefox has no
  `isMobile`: the viewport and `hasTouch`; WebKit has both), and as an installed PWA
  (standalone, so the safe-area insets for the notch and the home indicator
  apply). Consider the on-screen keyboard (the visual viewport shrinks) and
  touch (no hover: whatever a hover shows has a tap). `e2e/mobile.mjs` checks
  no page overflows sideways and no control is off screen, covered or
  under 44px (40px in the few dense rows it lists), with the audit in
  `e2e/phone_audit.mjs`, which `smoke.mjs` runs on the main page too.

## UI principles

The UX reference is pi (https://github.com/evan-a-w/pi). In every frontend:

- Never make the user retype what we just showed them: lists are pickers with
  fuzzy filtering, or the argument accepts what was displayed.
- Every error tells you what to do next (closest matches, the command to run).
- State is always visible: the status line shows model, thinking, context,
  cost, user, queued messages, agents, and any mode the UI is in (prigh-web
  has no header: the composer's row has the paperclip and the context ring
  (activity, context, cost: its card) at the left, then at the right the
  host chip, the model chip (provider mark, short name, dim reasoning
  level; its menu has the favourites and providers, the reasoning levels
  and the access mode) and send; the verbosity has no chip (Ctrl+O,
  the control centre); Ctrl+K and the sidebar's Search open the control
  centre over sessions, settings and commands; on a phone the host chip is
  its icon alone, the model's menu and the ring's card are sheets, the
  verbosity and usage are at the top of the ⋯ sheet, the top strip (under
  the safe area) has the sidebar's ☰, the session's name and ⋯, and the
  user is at the foot of the sidebar and on its settings page).
- Phones are first class (prigh-web is used as an installed PWA): one
  strip of chrome, never pieces floating over each other or the text; menus
  and the ⋯ are sheets from the bottom; settings fill the screen, a list of
  sections with a way back; 44px targets, inputs at 16px (no zoom on
  focus); the notch, home indicator and keyboard never cover a control.
- Dialogs own the keyboard until closed; Esc closes without side effects,
  Enter accepts. Every binding is in the keymap, `/help` and a test.
- Every action is acknowledged within one frame.
- Switching sessions or users resets everything that belongs to the old one.

## Pull requests

All changes go through pull requests on GitHub (`evan-a-w/prigh`): never
commit or push to `main` directly. Work on a branch (`feat/...`,
`fix/...`, `docs/...`), ideally in its own git worktree so parallel work
does not collide: make it with the `subagent` tool's `worktree` option
(delegated work) or the `worktree` tool, never `git worktree add` by hand,
so that it lives under `worktrees_dir` (`~/.prigh/worktrees/<project>/...`)
and the session shows it. Open, read and check out PRs (and their CI
checks) with the `pull_request` tool, which pushes the branch first and
uses the user's GitHub sign-in in prigh; only without that tool fall back
to `gh pr create` (`nix run nixpkgs#gh -- pr create` if `gh` is not
installed). If it says you are not signed in, ask the user to sign in
(Settings → Git hosting in prigh-web, or `/login github`). One PR per coherent
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
