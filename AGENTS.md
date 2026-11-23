prigh is a coding agent. `README.md` says what it does, `ARCHITECTURE.md`
how it fits together, `DOCKER.md` covers the container and `nix/README.md`
the flake's pins and patches.

## Running tests: `./test`, never `./test --full`

- **`./test` is the test suite.** Run it, and only it, before pushing and
  before merging. It takes seconds when little changed and under a minute
  after a change, even on a busy machine.
- **Never run `./test --full` unless the user explicitly asks for it** in
  this conversation. Not "to be safe", not before merging, not because a
  PR touches a lot, not because a document (including this one) once said
  to. It reruns everything, in three browsers, and costs many minutes of the
  whole machine, which the user shares with other work. If you think it is
  warranted, ask the user.
- While iterating, run the single suite you touched (table below) rather
  than `./test`.

`./test` builds and tests each project with one `dune build @default
@runtest`, then runs prigh-web's browser smoke (`frontend/web/e2e/smoke.mjs`
in Chromium) and the desktop's `cargo test`. It repeats nothing it already
has a result for: dune keeps its results, the browser smoke is skipped when
the backend, the site and the e2e scripts match its last green run, and
`cargo test` when its test executables do (`PRIGH_TEST_E2E=always`,
`PRIGH_TEST_DESKTOP=always` or `SHOTS=dir` run them anyway). It works from
any shell (it enters the dev shells through saved profiles in
`~/.cache/prigh/test-shell`).

`./test --full` adds the slow tier of dune tests (`PRIGH_TEST_TIER=full`)
and every browser e2e script at every size in Chromium, Firefox and WebKit.
No CI job runs it; the flake's `web-e2e` check runs the browser e2e alone.

## Layout and builds

| Directory | What | Toolchain | Build and test |
|---|---|---|---|
| `backend/` | the agent, providers, RPC/web servers (`prigh`); `lib/` in subdirectories (see `backend/AGENTS.md`) | OxCaml 5.2, Eio | `nix develop`, then `cd backend && dune build @runtest` |
| `frontend/` | the frontends' dune project: `protocol/`, `client/`, `ui/` shared; `tui/`, `web/` | OxCaml 5.2, Bonsai | `nix develop`, then `cd frontend && dune build @runtest` |
| `frontend/tui/` | the terminal frontend (`prigh-tui`) | Bonsai_term | `cd frontend && dune build @tui/test/runtest` (unit tests only) |
| `frontend/web/` | prigh-web, the browser frontend | Bonsai_web | `cd frontend && dune build @web/test/runtest ./web/bin/site` |
| `desktop/` | prigh-desktop, the Tauri v2 window running `prigh desktop` | Rust | `nix develop .#desktop`, then `cd desktop && cargo test` |
| `desktop/computer/` | `prigh-computer`, the computer-use helper (fake, wlroots, X11, Windows) | Rust | `cd desktop && cargo test -p prigh-computer` |
| `docker/` | the image's entrypoint (`prigh-docker`) and build step | bash | `bash docker/test-prigh-docker.sh` (`-promote` re-records) |
| `nix/` | pins and patches for the flake | | `nix/README.md` |

- The backend and the frontends share one OxCaml dev shell (`nix develop`;
  `.#backend` is an alias) but are separate dune projects: run dune in
  `backend/` or `frontend/`.
- **Only one dune process at a time** per tree, or it hangs (don't `dune
  exec` while tests run).
- The dev shell and `./test` turn on dune's shared cache
  (`DUNE_CACHE=enabled`, `~/.cache/dune`), so a new worktree builds in
  seconds. `DUNE_CACHE=disabled` turns it off; `dune cache trim
  --size=4GB` bounds it.
- Backend tests use Eio's io_uring backend where the kernel allows;
  `EIO_BACKEND=posix` selects the poll-based one (the only one on macOS).
- OxCaml has bugs we work around: "Known OxCaml issues" in `nix/README.md`.
  Check it when something misbehaves only under OxCaml or on the posix
  backend, add new ones there, and recheck them when upgrading OxCaml.
- The TUI's e2e and tmux tests run `backend/_build/default/bin/main.exe`:
  build the backend first (`dune build`; `@runtest` alone does not rebuild
  `main.exe`). They depend on `tui/e2e/backend.digest`, a hash of that
  binary, so they rerun when the backend changes and only then.
  `$PRIGH_BACKEND` overrides the path: unset it (prigh sets it) to test
  your build. The tmux tests use a private socket (`tmux -L <name>`).
- prigh-web's browser e2e (`frontend/web/e2e`) is Playwright scripts run
  with `node`: `ENGINES=chromium SUITE=core bash
  frontend/web/e2e/prigh_web.sh` runs the smoke, `SUITE=full` everything.
  It needs `PLAYWRIGHT_MODULE` (the playwright package's `index.mjs`),
  `PLAYWRIGHT_BROWSERS_PATH` (`nix build --no-link --print-out-paths
  --inputs-from . nixpkgs#playwright-test nixpkgs#playwright-driver.browsers`)
  and `FONTCONFIG_FILE` (without it Chromium renders no text; `./test`
  writes one). Each script gets its own backend, browser and directory
  (`JOBS`, `MEM_PER_JOB` bound how many at once); `ENGINE.expected` and
  `ENGINE.core.expected` hold what they print. Look at the screenshots
  (`SHOTS=dir`) as well as the text diff. Scripts load Playwright through
  `playwright.mjs`, which blocks service workers except where a script
  asks (`pwa.mjs`).
- The flake uses the binary cache `attic.caduc.co/prigh` (DOCKER.md,
  "Binary cache"); pass `--accept-flake-config` to Nix in non-interactive
  use.
- Every new RPC field or method is mirrored by the frontends that use it
  (`frontend/protocol/`); the e2e tests guard the contract.
- The dev profile's JavaScript has no source maps (`--profile debug-js`
  adds them).

## OCaml conventions

- Define types as `type t` in a module named after them: not `type
  undo_entry = ...` but `module Undo_entry = struct type t = ... end`.
- Write an `.mli` for every new file. When the `.ml` and `.mli` would
  duplicate module types (functors), put them in `x_intf.ml`, include it
  from the `.ml`, and make the `.mli` `include X_intf.X`.
- Modularise: self-contained functionality goes in its own module with a
  `type t`, exposing the minimum in its `.mli`.
- Use Core's applied modules: `Int.Table.t`, `Int.Table.create`,
  `Int.Map.of_alist_exn`; generic operations from `Hashtbl`, `Map`, `Set`.
  Never `Hashtbl.create (module Int)` or `(int, int) Hashtbl.t`.
- Import through the `import.ml` module.
- Format what you changed (`dune fmt`, or `ocamlformat --inplace FILE`).
  The dev shell's ocamlformat cannot parse OxCaml syntax (`local_`):
  format such files in `frontend/` by hand, in the same style.

## Writing tests

Test comprehensively: work isn't done until its tests are present and
passing. Prefer expect tests (`let%expect_test`) printing human-readable
state (sexps, screens) over asserting against hand-written values; review
the diffs from `dune build @runtest` and `dune promote` if right.

- Backend: drive the agent with `Faux_provider` and print event streams; no
  network. Helpers in `backend/test/tool_test_helpers.ml`.
- TUI: drive `App.update` with keys and events and print the screen (the
  `H` harness in `frontend/tui/test/test_app.ml`).
- prigh-web: drive `App.update` through `Harness` (`frontend/web/test`) and
  print the rendered HTML/text. `Harness` reuses a rendering until the
  model changes: change state only through `App.update`.
- Wait for a condition, never a fixed time (`waitForFunction`, `waitFor` in
  the e2e). Output must not depend on timing: a test that passes only on an
  idle machine is broken.

Keep `./test` minimal; depth goes in the slow tier, which only `./test
--full` runs:

- A slow, integration-style dune test whose behaviour cheaper tests already
  pin goes in the slow tier: its stanza gets `(enabled_if (=
  %{env:PRIGH_TEST_TIER=default} full))`. Its code still builds, with
  warnings, in every run.
  - Backend: a test taking ≥100 ms CPU or ≥1 s wall goes in
    `backend/test/full/test_x_full.ml` (opening `Test_x`); `test_x.ml` keeps
    the cheap ones, so every area keeps a main-path test in the default.
    Time one with `inline-test-runner.exe inline-test-runner <lib>
    -source-tree-root . -only-test file.ml:LINE`. A new slow file also gets
    an entry in `inline_test_shards.ml`'s timing table.
  - prigh-web: scale tests go in `frontend/web/test/full/`. The rest run in
    two node shards (`web/test/dune`: shard a lists its modules, shard b
    takes the rest, so new files need no entry).
  - TUI: the default runs the e2e transcript and `tmux-test/smoke.t`; other
    tmux groups and the websocket e2e are slow tier.
- Browser e2e: `smoke.mjs` covers each main flow once. Add to it only a
  main flow nothing else catches and only a real browser can; everything
  else is a script or step in the full suite (`scripts` in
  `frontend/web/e2e/prigh_web.sh`).
- prigh-web on phones: test every prigh-web change on phones too, and look
  at the screenshots: at least 390x844 and 320x640 portrait with `isMobile`
  and `hasTouch` (Firefox has no `isMobile`), and as an installed PWA
  (standalone, with safe-area insets). Consider the on-screen keyboard and
  touch (no hover: whatever hover shows has a tap). `e2e/mobile.mjs` and
  `e2e/phone_audit.mjs` check nothing overflows sideways and no control is
  off screen, covered or under 44px (40px in the dense rows it lists).

## UI principles

The UX reference is pi (https://github.com/evan-a-w/pi). In every frontend:

- Never make the user retype what we just showed them: lists are pickers
  with fuzzy filtering, or the argument accepts what was displayed.
- Every error says what to do next (closest matches, the command to run).
- State is always visible: model, thinking, context, cost, user, queued
  messages, agents and any mode. In prigh-web (no header) the composer's
  row has the paperclip and context ring (activity, context, cost; its
  card) on the left, and the host chip, model chip (provider mark, short
  name, dim reasoning level; its menu has favourites, providers, reasoning
  levels and the access mode) and send on the right. Verbosity is Ctrl+O
  and in the control centre (Ctrl+K, or the sidebar's Search: sessions,
  settings, commands). On a phone the host chip is its icon, the model menu
  and ring card are sheets, verbosity and usage top the ⋯ sheet, the top
  strip has ☰, the session name and ⋯, and the user is at the foot of the
  sidebar and on its settings page.
- Phones are first class (prigh-web is used as an installed PWA): one strip
  of chrome, nothing floating over the text; menus and ⋯ are bottom sheets;
  settings fill the screen as a list of sections with a way back; 44px
  targets, 16px inputs; the notch, home indicator and keyboard never cover a
  control.
- Dialogs own the keyboard until closed; Esc closes without side effects,
  Enter accepts. Every binding is in the keymap, `/help` and a test.
- Every action is acknowledged within one frame.
- Switching sessions or users resets everything belonging to the old one.

## Pull requests

- Every change goes through a PR on GitHub (`evan-a-w/prigh`); never commit
  or push to `main`. Merge only when the user asks.
- Work on a branch (`feat/...`, `fix/...`, `docs/...`) in its own worktree,
  made with the `worktree` tool or the `subagent` tool's `worktree` option
  (never `git worktree add`), so it lives under
  `~/.prigh/worktrees/<project>/`.
- Use the `pull_request` tool (it pushes and signs in as the user); fall
  back to `gh pr create` (`nix run nixpkgs#gh -- ...`) only without it. If
  not signed in, ask the user to sign in (Settings → Git hosting, or
  `/login github`).
- One PR per coherent change. Its description says what changed and why,
  how it was tested (which suites ran) and what's left. Keep it up to date
  with `main` and `./test` passing.

## Shared machine

You run on the user's machine, next to their work. Never kill, stop or reset
anything you did not start: no `tmux kill-server`/`kill-session` on the
default socket, no `pkill`/`killall` by name, no killing prigh backends, tool
hosts or editors you did not launch. Kill only your own processes, by pid or
private socket.

## General

- Eagerly remove dead code, unless it is likely to be useful and costs
  nothing.
- Comment only what is truly hard to understand from names and code.
- If the user changed a module, don't revert it: apply their ideas
  elsewhere (an API made cleaner, comments removed or reworded).
- Keep docs current and short: update `README.md`, `ARCHITECTURE.md`,
  `DOCKER.md` or `nix/README.md` when behaviour they describe changes, and
  this file when conventions or the workflow change. Keep each under 600
  lines. No plan or handoff documents in the repository.
