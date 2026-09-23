# Frontend plan — Bonsai_term TUI replacing the TypeScript frontend

Context: `HANDOFF.md` (why OxCaml, two switches, Nix), `ARCHITECTURE.md`
(backend + wire protocol), `UX_PLAN.md` (principles and P0 work).

## Layout

```
tui/                       dune project `prigh_tui`, built in the `prigh-ox` switch / `nix develop`
  protocol/  prigh_protocol   wire types + Jsonaf decoders/encoders (mirror of backend Rpc_json)
  client/    prigh_client     Async JSON-lines RPC client over an abstract Transport
  ui/        prigh_ui         platform-agnostic logic: Elm-style Model/Action/update + pure widgets
  term/      prigh_ui_term    Bonsai_term: Event.t -> Intent, Content -> View, spawns backend
  bin/       prigh-tui        executable
  test/                       expect tests for all of the above
  e2e/                        e2e against `main.exe serve -faux` (dune alias @e2e)
```

`prigh_protocol` is frontend-owned; the backend keeps `Rpc_json` as the
encoder. The contract is guarded by the e2e test (real backend, real
client) rather than by sharing source between two incompatible switches.

## Design

- **Pure core.** `Prigh_ui.App` is `update : Model.t -> Action.t -> Model.t *
  Command.t list`. Actions are `Intent` (user input), `Backend_event`,
  `Rpc_result`, `Tick`, `Resize`. Commands are `Rpc {method; params; reply}`,
  `Open_browser`, `Quit`. No I/O anywhere in `prigh_ui`, so every scenario in
  `UX_PLAN.md` §4 is a plain expect test that prints the rendered screen.
- **Bonsai wrapper.** `Prigh_ui.Component.create ~perform` wraps the pure core
  in `Bonsai.state_machine` and schedules commands via `perform : Command.t ->
  unit Effect.t`; it is what both platforms mount.
- **Headless widgets.** `Editor` (multi-line, history, kill commands),
  `Picker` (fuzzy filter + selection), `Transcript` (items + streaming tails),
  `Commands` (slash table, parse, complete), `Fuzzy`, `Model_match`
  (display-name / prefix / did-you-mean).
- **Intent + Keymap.** `Key.t` is the platform-neutral key; `Keymap.lookup :
  Mode.t -> Key.t -> Intent.t option` is one table that also produces `/help`
  text. A test asserts every binding is documented and exercised.
- **Content.** Rendering produces `Content.t` (styled lines) via
  `Render.screen : Model.t -> width -> height -> Content.t`; the term layer
  converts `Content.t` to `View.t` 1:1. Snapshots are `Content.to_plain`.
- **Modes.** `Mode.t = Editor | Picker | Login_prompt | Confirm`; a dialog owns
  the keyboard, Esc closes it, Enter accepts, opening a second is a no-op with
  a notice.
- **Fullscreen.** Bonsai_term redraws the whole screen, so the transcript is a
  scrollable viewport (PageUp/PageDown; auto-follow while at the bottom).

## Milestones

1. `prigh_protocol` + round-trip tests. ✔
2. `prigh_client` (Transport, framing, id correlation, event pipe) + tests
   with an in-memory transport. ✔
3. `prigh_ui` core: Editor, Commands, Transcript, Model/update, Render;
   parity with the TypeScript TUI (prompt/steer, streaming, tools, notices,
   login prompts, Esc/Ctrl+C semantics, `/help /model /thinking /auth /login
   /logout /compact /new /sessions /switch /fork /abort /state /clear /quit`). ✔
4. `prigh_ui_term` + `prigh-tui` binary; `./prigh` launches it. ✔
5. `UX_PLAN.md` P0: `Picker`, `/model`, `/sessions`, `/switch`, `/login`,
   `/logout`, `/thinking`, `/` command picker; backend `set_model` accepts
   display names / unique prefixes and answers "did you mean". ✔
6. e2e against `main.exe serve -faux` with an isolated `-auth-file`. ✔
7. Nix: flake builds `tui/` (the spike is retired); `./prigh` uses either the
   opam switch or `nix develop`. ✔
8. Delete `frontend/` (TypeScript); update `ARCHITECTURE.md`. ✔

9. `prigh_ui_web` on bonsai_web over a WebSocket (`prigh serve -web`), same
   `Screen.t` rendered as a DOM cell grid; `@` completion moved to the
   backend (`list_paths`, on the active tool host). ✔

Later (P1/P2 in `UX_PLAN.md`): login dialog block, confirmations, collapsible
tool output, markdown tables.
