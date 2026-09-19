# Parity plan — making prigh a complete daily driver

Context: `ARCHITECTURE.md` (current system), `UX_PLAN.md` (principles we hold
the TUI to), `FRONTEND_PLAN.md` (what is done). This plan closes the gap to
`~/dev/pi` for everything except pi's extensibility layer (extensions,
skills, prompt templates, themes, web UI) which prigh deliberately does not
have: tools, subagents and commands are compiled in.

Every milestone ends with tests that print **real UI state**: `App.update`
driven by keys/events and snapshotted with `Screen.to_plain ~show_cursor:true`
(the `H` harness in `tui/test/test_app.ml`), backend behaviour driven by
`Faux_provider` with printed event streams, and the `tui/e2e` script against
the real `main.exe serve -faux`. "Done" for any item means the expect test
shows the screen the user sees, not that the code compiles.

Conventions (from `backend/AGENTS.md`): `.mli` for every module, ocamlformat
after edits, one dune process at a time. `prigh_ui` stays free of I/O; anything
that touches the OS becomes a `Command.t` executed by the platform layer and
answered through `Reply`.

---

## M0 — Known bugs (fix first, each with a regression test)

1. **Ctrl+O does nothing in a real terminal.** `App.editing` handles
   `Toggle_tool_output` and `test_app.ml` proves the state machine flips
   `expand_tools`, so the loss is between the terminal and `Key_of_event`:
   `^O` is the tty `VDISCARD` character and is swallowed unless `IEXTEN` is
   cleared in raw mode, and/or Bonsai_term delivers it in a shape
   `Key_of_event.key` does not map. Fix: verify the termios flags Bonsai_term
   sets (add `IEXTEN` clearing in `Term_app` if needed) and add
   `test/test_key_of_event.ml` mapping every `Bonsai_term.Event.t` form for
   every bound key (`ASCII '\015'`, `ASCII 'o'` + `Ctrl`, …) to the expected
   `Key.t`. Verified in the tmux harness (M9) by pressing `C-o` and asserting
   the screen changes. M1 then replaces the toggle with verbosity cycling.
2. **Scroll does not follow streaming.** `scroll` is only reset by `submit`;
   `Message_update`/`Tool_output` leave it, and because `scroll` is measured
   from the bottom, new lines push the text the user is reading off the
   viewport. Fix: `scroll` becomes an anchor from the **top** when the user
   has scrolled (`Scrolled of int`) and `Follow` otherwise; new content
   never moves an anchored viewport; the status line shows `↓ N new lines`
   while anchored; `End` (empty editor) or `Page_down` past the bottom
   returns to `Follow`. Every event that appends to the transcript goes
   through one `Transcript.apply` (M3) so the rule is in one place.
   Test: scroll up during streaming, feed 10 deltas, show the screen twice
   (unchanged viewport, indicator increments), `End`, show (bottom).
3. **Esc-abort drops queued messages.** `Agent.abort` clears `steer_queue`
   and `follow_up_queue`; the TUI shows `[aborted]` and the text is gone.
   Fix: `abort` returns `{restored: [text…]}` (steer + follow-up in order)
   and the backend emits `Queue_update {steer: n; follow_up: n}` whenever
   either queue changes; the TUI puts restored texts into the editor (joined
   by blank lines) with a notice `restored 2 queued messages to the editor`.
   Tests: backend `test_agent.ml` (abort with two queued messages prints the
   restored list and a `Queue_update 0/0`), TUI scenario (queue, Esc, editor
   contains the texts, `abort` command printed once).
4. **Only the last line of streamed tool output is kept** (see M1, live
   tail keeps the last 5 lines).
5. **Flaky `list sessions` ordering** — `Session.create` stamps must be
   strictly increasing (M5).

---

## M1 — Transcript verbosity (Ctrl+O cycles Quiet / Normal / Verbose)

Replaces the single `expand_tools` boolean.

### Design

`tui/ui/verbosity.ml`:

```ocaml
type t = Quiet | Normal | Verbose  [@@deriving sexp_of, equal, enumerate]
val next : t -> t                  (* Quiet -> Normal -> Verbose -> Quiet *)
val name : t -> string
```

`Model.verbosity : Verbosity.t` (default `Normal`), `Model.expand_tools`
removed. `Intent.Toggle_tool_output` renamed `Cycle_verbosity` (Ctrl+O);
`/verbosity [quiet|normal|verbose]` sets it directly (no argument opens a
picker). The status line shows `view:quiet` etc. Cycling prints a one-line
notice (`view: verbose — everything is shown`) so the change is acknowledged
within one frame (UX principle 8).

What each level shows, decided in `Transcript.render_item ~verbosity`:

| Item | Quiet | Normal | Verbose |
|---|---|---|---|
| User message | full | full | full |
| Assistant text at end of turn | full | full | full |
| Assistant text between tool calls | first line, `…` | full | full |
| Thinking | hidden | first 3 lines, dim | full, dim |
| Tool call + result | **one merged line** `⚙ bash ls -la ✓ 12 lines` | call line + ≤5 result lines + `… (N more)` | full arguments (pretty JSON) + full result |
| Tool error | merged line, red, + first 3 lines of error | call line + ≤8 lines red | full |
| Subagent progress (M3) | one line per agent with state | live tail | every nested event |
| Notice Info | hidden | shown | shown |
| Notice Warn/Error | shown | shown | shown |
| Block (help, auth…) | shown | shown | shown |
| Compaction | one line | one line | summary text |

To render a merged line the transcript must pair a `Tool_call` with its
`Tool_result`. Change `Transcript.Item.t`:

```ocaml
| Tool of { call : P.Tool_call.t; result : P.Message.Tool_result.t option; live_tail : string option }
```

`Tool_start` adds `Tool { result = None }`, `Tool_output` updates the tail
of the last open tool, `Tool_end` fills `result`. `add_message` (session
reload) pairs by `call.id`. `tool_tail` on `t` goes away — the tail lives on
the item, which also fixes "only the last line of tool output is kept": keep
the last 5 lines while running (Normal shows 1, Verbose shows all 5).

"End of turn" vs "between tool calls" is known when the item is added: an
`Assistant` item is `final` when the `Message_end` carries `End_turn`
(streaming text is treated as non-final until then; at end it is re-tagged).
`Item.Assistant of { text; final : bool }`.

`Transcript.line_count` and `render_tail` take `~verbosity`; `App.scroll_by`
passes it. Switching verbosity resets `scroll` to 0 (follow the bottom) so the
view never lands in the middle of a collapsed block.

### Files

`ui/verbosity.{ml,mli}` (new), `ui/transcript.{ml,mli}`, `ui/app.{ml,mli}`,
`ui/intent.ml`, `ui/keymap.ml`, `ui/commands.ml`, `ui/render.ml`
(status line), `term/` untouched.

### Tests (`test/test_app.ml`, `test/test_widgets.ml`)

- Scenario "verbosity": prompt → `Tool_start bash` → 3 `Tool_output` chunks
  → `Tool_end` with a 20-line result → assistant text → `Message_end
  End_turn`. Show the screen at Normal, press Ctrl+O twice (Verbose, Quiet),
  show after each; assert the status line reads `view:…` and the notice
  appears. Then `/verbosity normal` and show again.
- "quiet hides intermediate text and thinking": thinking delta + text + tool
  call + final text; Quiet screen shows `user`, one `⚙` line and the final
  text only.
- "tool error is always visible": Quiet screen with `is_error = true`.
- "reload pairs calls and results": `Reload_messages` with an assistant tool
  call and a matching tool result renders one merged line in Quiet.
- "live tail while running keeps last 5 lines": widget test printing
  `Transcript.render_item` at each verbosity after 7 chunks.
- Keymap coverage test updated (Ctrl+O now `Cycle_verbosity`).
- e2e: add Ctrl+O toggles to the script; expected transcript updated.

---

## M2 — Inline autocomplete for `/commands`, arguments and `@files`

### Design

Autocomplete is **not a dialog**: it is a sub-state of `Editing` so the user
keeps typing and the transcript is never covered.

```ocaml
(* ui/autocomplete.ml *)
module Source = Command | Argument of Commands.Spec.t | Path
type t = { source : Source.t; prefix : string; items : Picker.Item.t list; selected : int }
val compute_command : text:string -> t option        (* "/mo" -> commands fuzzy *)
val compute_argument : Model-independent inputs -> t option
val accept : t -> editor_text:string -> string        (* replaces prefix with the item *)
```

`Model.autocomplete : Autocomplete.t option`. After every intent that
changes the editor in `Editing` mode, `App.refresh_autocomplete m` runs:

- line starts with `/` and has no space → command completion (fuzzy via
  `Fuzzy`, ranked prefix > substring > subsequence), max 8 rows.
- line is `/cmd <partial>` and `Commands.find cmd` has an
  `argument_completer` → argument completion. Completers:
  `model` → `m.models` (label name, detail key, dimmed when not logged in),
  `thinking` → levels, `verbosity` → levels, `login`/`logout` → providers
  from `m.auth`, `switch` → sessions (needs `m.sessions` cache, fetched on
  first use via `Rpc list_sessions ~tag:Sessions_cache`), `cd` → directories
  (path source).
- the word under the cursor starts with `@` (or `/cmd` args of path type) →
  `Command.List_paths { prefix; reply_tag = Paths_for_autocomplete prefix }`
  to the platform; the term layer runs `fd`/`readdir` under cwd and replies
  with an array of paths; the app installs them only if the editor prefix is
  still the same (stale replies are dropped).

Keys while autocomplete is open (handled in `App.editing` before the editor):
Tab / Enter accept (Enter on a command with args leaves the cursor after the
space and reopens argument completion; Enter on a command without args
submits it immediately), Up/Down move, Esc closes (does *not* abort the
run), any other key updates the prefix. Empty `/` shows the whole command
list, so the full-screen command picker is no longer needed; `Commands`
picker kind is removed.

`Render.screen` draws the list between the editor rows and the status line:
`  /model    [name|id|provider/id]  pick or switch the model` with the
selected row inverted; the cursor stays in the editor. The transcript
shrinks by the list height, exactly as it does for the picker today.

`@path` tokens are kept in the prompt text; the backend `prompt` method gets
an optional `attachments : string list`; the TUI extracts `@tokens` that
resolve to files (it knows from the completion reply; unknown tokens are left
as plain text) and the backend appends `<file path="…">…</file>` blocks (read
with `Tool_read` limits) to the user message. This is also how pi does it.

### Files

`ui/autocomplete.{ml,mli}` (new), `ui/commands.{ml,mli}` (`argument`
field), `ui/app.ml`, `ui/render.ml`, `ui/mode.ml` (drop `Commands`),
`term/term_app.ml` (`List_paths`), `backend/lib/rpc_server.ml`,
`backend/lib/agent.ml` (`prompt ?attachments`).

### Tests

- "typing / lists commands, Down twice + Tab fills /login ": screen after
  each key.
- "/mo Enter opens argument completion over models; typing fab Enter calls
  set_model": commands printed, screen shown.
- "Esc closes autocomplete without abort while running".
- "@ completion is asynchronous and drops stale replies": type `@sr`,
  observe `List_paths` command, type `c` before the reply, reply for `sr`
  arrives and is ignored, reply for `src` shows `src/app.ml`.
- "submit with @file sends attachments param".
- Backend: `test_rpc` prompt with attachments shows the file block in the
  faux provider's request.
- Fuzzy ranking table for command names.

---

## M3 — Subagents: flexible, observable, parallel

### Backend

**One tool, no roles.** The model decides what a subagent may do.

```
subagent
  task        string   required  complete description, subagent sees nothing else
  tools       [string] optional  subset of the parent's tools; default: all
  model       string   optional  key/id/name/prefix (Model.resolve); default: parent's
  thinking    string   optional  default: parent's
  cwd         string   optional  default: parent's cwd
  max_turns   int      optional  default 50
  context     string   optional  extra text prepended as a system block (e.g. a plan)
```

Result: the subagent's final assistant text plus a trailer
`[subagent: N turns, in/out tokens, $cost]`. Errors, aborts and turn-limit
exhaustion are error results carrying whatever text was produced.

- **Depth cap.** `Tool.Context.depth : int`; the subagent tool is not
  offered (removed from the tool list) when `depth >= 2`, so a subagent can
  delegate once but not recurse forever.
- **Cancellation.** Already shared through `context.cancel`; add the test.
- **Usage roll-up.** `Agent` accumulates subagent usage into
  `State.usage`/`cost_usd` (not `context_tokens`); the subagent's usage
  is reported in a new event (below) so the TUI can show it per agent.
- **Structured events instead of text chunks.** `Agent_event.t` gains

  ```ocaml
  | Subagent of { call_id : string; agent_id : string; event : t }   (* recursive *)
  | Subagent_start of { call_id; agent_id; task; model; tools }
  | Subagent_end   of { call_id; agent_id; usage; turns; cost_usd; result : Tool.Result.t }
  ```

  `Tool.Context.emit : Agent_event.t -> unit` replaces `on_output` for this
  tool (bash keeps `on_output`, which `Agent_loop` still maps to
  `Tool_output`). `Rpc_json` encodes them as `{"event":"subagent",
  "call_id", "agent_id", "inner": {...}}` etc.; `agent_id` is
  `<call_id>` for depth 1 and `<parent>/<call_id>` deeper. The TUI protocol
  decoder is recursive.
- **Parallel tool execution.** `Agent_loop` runs the tool calls of one
  assistant turn with `Eio.Fiber.List.map` when **every** call in the turn is
  parallel-safe (`Tool_spec.parallel_safe : bool` — true for `read`, `ls`,
  `grep`, `find`, `subagent`; false for `bash`, `write`, `edit`); otherwise
  sequentially as today. Results are appended in call order regardless of
  completion order so the context stays deterministic. This is what lets the
  model launch three subagents at once.
- `Tools.all` is built per-context so the subagent list can be filtered
  (`tools` argument, depth cap). Unknown tool names in `tools` are an
  argument error listing valid names.

### TUI — seeing what subagents do

`Model.agents : Agent_view.t list` (ordered by start), where

```ocaml
(* ui/agent_view.ml *)
type status = Running | Done of { turns; cost_usd } | Failed of string
type t = { id : string; call_id : string; task : string; model : string
         ; status : status; transcript : Transcript.t; children : t list }
```

Each subagent gets its **own `Transcript.t`** fed by the nested events
through the same `App.event` code path (refactored into
`Transcript.apply : t -> P.Event.t -> t` so main and subagents share it).
Nested `Subagent` events within a subagent recurse into `children`.

Focus: `Model.focus : [ `Main | `Agent of string ]`. **Shift+Tab cycles**
Main → agent 1 → agent 2 → … → Main (only agents from the current or last
turn; finished agents stay cyclable until the next user prompt so you can
read their reports). `Alt+1…9` jumps directly, `/agents` opens a picker
(task, status, model) that focuses one. Esc while focused on an agent
returns to Main (before its abort meaning is considered).

Rendering when an agent is focused: the transcript area shows that agent's
transcript (same verbosity rules), with a header line
`◆ subagent 2/3  claude-haiku  ⠋ running 4 turns  "find all auth code…"`,
and the editor stays the main editor (prompt/steer still go to the main
agent — steering subagents is out of scope). The status line always shows an
agent strip when any exist: `agents: [main] 1⠋ 2✓ 3✗  (Shift+Tab)`, with
the focused one highlighted.

In the **main** transcript a subagent call renders as a `Tool` item whose
live tail is the subagent's last tool call line, and, per verbosity:
Quiet — `⚙ subagent "task…" ✓ 6 turns $0.02`; Normal — that plus the first
5 lines of the report; Verbose — full report and every nested tool line
indented by two spaces.

Session reload: subagent transcripts are not persisted (they are inside the
tool result), so after `/switch` only the result text is shown.

### Files

Backend: `lib/tool_subagent.{ml,mli}`, `lib/tool.{ml,mli}` (depth, emit),
`lib/tool_spec.ml` (`parallel_safe`), `lib/tools.{ml,mli}` (`for_context`),
`lib/agent_event.ml`, `lib/agent_loop.ml`, `lib/agent.ml` (usage),
`lib/rpc_json.ml`. TUI: `protocol/event.ml` (recursive), `ui/agent_view.{ml,mli}`
(new), `ui/transcript.ml` (`apply`), `ui/app.ml`, `ui/render.ml`,
`ui/keymap.ml` (`Shift+Tab`, `Alt+digit`), `ui/mode.ml` (`Agents` picker).

### Tests

Backend (`test/test_subagent.ml`, `test/test_agent_loop.ml`), each printing
the emitted event stream:

- subagent with `tools: ["read"]` is offered only `read` (assert the
  provider request's tool list); unknown tool name → argument error.
- `model` argument switches the provider request's model; bad name → error
  with "did you mean".
- depth: a subagent's tool list excludes `subagent` at depth 2.
- parallel: a turn with three `subagent` calls runs them concurrently
  (faux provider `delay_between_events` + a counter proving overlap);
  results are appended in call order.
- mixed turn (`read` + `bash`) is sequential.
- abort mid-subagent: parent `abort` yields `[cancelled]` results for the
  subagent and any remaining calls; no fiber leaks (switch completes).
- usage roll-up: parent `State.usage` includes the child's tokens; event
  stream contains `Subagent_end` with usage.
- `Rpc_json` round trip of nested events; protocol decoder test on the TUI
  side with the same JSON literal (guards the contract).

TUI (`test/test_app.ml`):

- "two parallel subagents": events for two `Subagent_start`, interleaved
  nested `Tool_start`/`Message_update`, `Subagent_end` for one. Show main
  screen (tool item live tails + agent strip), Shift+Tab → agent 1 screen,
  Shift+Tab → agent 2 (finished, report visible), Shift+Tab → main. Alt+2
  jumps. Esc returns to main and does **not** emit `abort`.
- "verbosity applies inside an agent view".
- "/agents picker lists task, status, model; Enter focuses".
- "new user prompt clears finished agents from the strip".
- e2e: faux script with a subagent call, checks the nested events arrive
  through the real client.

---

## M4 — Editor ergonomics and input

| Item | Keys | Notes |
|---|---|---|
| Word left / right | Alt+B / Alt+F, Ctrl+Left / Ctrl+Right | `Editor.word_left/right`, unicode-aware word boundaries (`Text_width.uchars`) |
| Delete word forward | Alt+D | |
| Kill to line start | Ctrl+U (currently kills whole line → move that to nothing; matches readline) | keep `Kill_line` reachable via Ctrl+U on an empty prefix? No: readline semantics, documented in `/help` |
| Kill ring + yank | Ctrl+Y yank, Alt+Y yank-pop | `Editor.kill_ring : string list` (max 20); every kill pushes |
| Undo | Ctrl+_ (Ctrl+-) | `Editor.undo_stack` of `(lines, cursor)` snapshots, grouped per word/kill; max 100 |
| Persistent history | | `~/.prigh/history` (one JSON line per entry, last 500). Loaded by the platform at start (`Command.Load_history` → `Reply History`), appended on submit (`Command.Append_history`). Secrets (login prompt) never recorded (already true) |
| Follow-up queue | Alt+Enter queues a follow-up (delivered after the run ends); Alt+Up pops the last queued message (steer or follow-up) back into the editor via a new `dequeue` RPC | Newline stays on Ctrl+J and Alt+J. Queued messages render as a dim block above the editor `queued (2): …`; the status line shows `queued:2` driven by the `Queue_update` event (M0.3), not a guess |
| Copy last message | Ctrl+X | `Command.Copy_to_clipboard text` (term layer: OSC 52, falling back to `wl-copy`/`xclip`/`pbcopy`); with a subagent focused (M3) copies its report |
| Suspend | Ctrl+Z | `Command.Suspend`: term layer releases the terminal, `SIGTSTP`s itself, re-initialises on `SIGCONT` and forces a redraw |
| Word delete backward | Alt+Backspace (alias of Ctrl+W) | |
| File reference | Ctrl+R opens path completion at the cursor (same as typing `@`) | M2's `Path` source |
| External editor | Ctrl+G | `Command.Edit_externally text` → term layer suspends Bonsai_term (`Notty` release), runs `$VISUAL`/`$EDITOR` on a temp file, resumes and replies `Editor_text` |
| Inline bash | `!cmd` runs a shell command through a new backend RPC `shell {command}` (streams `Tool_output`-style chunks under a synthetic call id, rendered as a `Tool` item) and adds the command+output to the context as a user message; `!!cmd` runs it without adding to context | mirrors pi |
| Bracketed paste | already handled in `term_app.ml`; add a `[N lines pasted]` chip when >3 lines are inserted at once: the editor stores the text but renders the chip until the cursor enters it | UX_PLAN P3 |
| Free Ctrl+L | Ctrl+L → open model picker (as pi); `/clear` stays the way to clear | |

### Tests

`test_widgets.ml` table-driven editor tests (keys → lines + cursor) for word
nav, kill ring, undo; `test_app.ml` scenarios: follow-up queue (Alt+Enter
prints no RPC until `State running=false`, then `follow_up`… actually the
backend queues, so assert the `follow_up` command and the `queued:1` status),
Alt+Up restores; `!ls` emits `shell`; paste chip; `Load_history` at start and
`Append_history` on submit; Ctrl+G emits `Edit_externally` and the reply
replaces the editor text; Ctrl+X emits `Copy_to_clipboard` with the last
assistant text; Ctrl+Z emits `Suspend`; Ctrl+R opens path completion;
Alt+Up emits `dequeue` and the reply lands in the editor. Keymap coverage
test extended (every binding hit). tmux (M9): Ctrl+Z then `fg` redraws
intact; Ctrl+G with `EDITOR=true` round-trips.

---

## M5 — Sessions

Backend additions (`Session`, `Agent`, `Rpc_server`):

- `name` entry type (`{"type":"name","name":…}`); `Session.Summary.name`;
  `set_session_name` RPC.
- `list_sessions` returns `name`, `updated_at`, `message_count`,
  `first_prompt`, `cwd`, `parent` (for clones/forks).
- `delete_session {path}` (refuses the active session).
- `export {format: "markdown"|"jsonl", path?}`: markdown transcript
  (user/assistant/tool blocks with fences) or a copy of the JSONL; returns
  the written path. HTML export is deferred (see end).
- `import {path}`: copies a JSONL file into the sessions dir and switches to
  it.
- `clone`: `fork` at head (exists) — expose as `clone`.
- `fork` and `rewind` take an entry id; `get_entries` already lists them.
- `session_stats`: message count, turns, tool calls by name, tokens
  in/out/cache, cost, context %, model changes, compactions, duration.
- `set_cwd {path}`: changes the agent cwd (tools and system prompt),
  records a `cwd` entry so reload restores it; `State.cwd` updates.
- `git_branch` field on `State` (computed cheaply by reading
  `.git/HEAD`, refreshed on `set_cwd` and at the start of each turn).

TUI commands:

| Command | Behaviour |
|---|---|
| `/name [text]` | sets the name; without text prompts in a `Text_prompt` mode (new generic single-line dialog, reused by `/cd` etc.) |
| `/session` | block with stats |
| `/sessions` | picker now shows `name ∣ date ∣ N msgs ∣ first prompt`, Ctrl+N toggles named-only, Ctrl+D deletes (with confirm) |
| `/fork` | picker of the user messages on the active path (`get_entries`); Enter forks at that message and puts its text in the editor so you can re-send an edited version (pi behaviour) |
| `/rewind` | same picker; Enter rewinds the head to that message (confirm) |
| `/tree` | tree view of `get_entries` as a picker: indented branches, current path marked, Enter switches head (`rewind`/`switch`) — filters like pi's (`Ctrl+F` cycle: default / no tool results / user only) are optional |
| `/clone` | clone at head |
| `/export [path]` | markdown by default, `.jsonl` if the path says so |
| `/import <path>` | |
| `/cd <path>` | with path autocomplete (M2) |

`Confirm_action.t` grows `Rewind`, `Delete_session`, `Logout` (exists).

### Tests

Backend `test_session.ml`/`test_rpc.ml`: name entry round trip, list with
names, delete refuses active, export markdown output printed, import +
switch, stats printed, `set_cwd` recorded and replayed, `fork ~at` /
`rewind` from an entry id. TUI: each command as a screen scenario
(`/fork` picker → Enter → `fork` command with the id and editor prefilled;
`/rewind` confirm; `/tree` rendering with two branches; `/sessions` Ctrl+D
confirm → `delete_session`). Also fix the flaky "list sessions" ordering by
making `Session.create` stamps strictly increasing (noted in `HANDOFF.md`).

---

## M6 — Models, thinking, status line

- **Scoped models**: `~/.prigh/config.json` `{"scoped_models": ["anthropic/…", …]}`
  read/written by the backend (`get_config`/`set_config` RPC). `/scoped-models`
  opens a multi-select picker (Space toggles, Enter saves; Ctrl+A all,
  Ctrl+X none). Default scope: every model of a logged-in provider.
- **Ctrl+P / Shift+Ctrl+P** cycle forward/backward within the scope
  (immediate `set_model`, notice shows the new key).
- **Ctrl+T** cycles the thinking level (off → low → on → high → max, skipping
  levels the model does not support; `supports_thinking=false` models show
  `thinking:n/a`).
- Status line rewrite (`Render.status`), left-truncating so the model key is
  always visible:
  `~/proj (main)  claude-fable-5-1  think:high  view:normal  ctx:42% 61k  $0.12  queued:1  agents:[main] 1⠋ 2✓`
  (`queued:` = steer + follow-up counts from `Queue_update`; `↓ N new` is
  appended while the viewport is anchored, M0.2).
  Context % is coloured green <50, yellow <80, red ≥80; cwd shows `~`.
- `/model` picker gains a **"logged in only"** toggle (Ctrl+N) and shows the
  scoped mark.

### Tests

Config round trip (backend); `/scoped-models` picker screens (toggle, save →
`set_config`); Ctrl+P cycles and wraps; Ctrl+T skips unsupported levels;
status line screens at width 40 and 120 (truncation keeps the model key).

---

## M7 — Rendering

- **Markdown** (`ui/markdown.ml`): headings (levels styled distinctly),
  ordered/unordered nested lists with proper indentation, block quotes,
  horizontal rules, tables as a monospace grid (`Text_width`-aware, columns
  truncated to fit `width`), fenced code with a language tag line, inline
  code/bold/italic/strikethrough, links rendered as `text` with an OSC-8
  hyperlink (`Style.link : string option`; `View_of_content` emits
  `\027]8;;url\027\\`), falling back to `text (url)` when `$TERM` lacks
  hyperlink support. No syntax highlighting (deferred).
- **Diffs**: `Tool_edit` returns a unified diff of each applied edit in its
  result text (`--- a/… +++ b/… @@`); the renderer colours `+`/`-`/`@@`
  lines. `Tool_write` returns `wrote N lines to path`. Applies at Normal
  (first 5 lines of the diff) and Verbose (full).
- **Bash results** at Normal show the first 5 lines *and* the last 3 (the
  exit status/summary usually lives at the end) with a `… (N lines
  hidden)` gap line.
- Login prompt and confirm dialogs become bordered blocks above the editor
  (UX_PLAN P1): the auth URL, instructions, progress lines and the masked
  answer field in one block; Esc cancels via `auth_cancel`.
- Transcript **search**: Ctrl+F opens a search field in place of the editor
  (`Mode.Search`), highlights matches, `n`/`N` (or Down/Up) jump, Esc
  closes; the viewport scrolls to the match. **Ctrl+Up / Ctrl+Down** jump
  the viewport to the previous/next user message. **Home/End** in the editor
  when it is empty scroll the transcript to top/bottom.
- `/hotkeys` = the keys half of `/help` (alias), `/help <command>` prints one
  command's usage.

### Tests

Widget tests printing `Markdown.render` for a fixture covering every
construct at width 40 and 80; diff colouring test (`Content` styles printed
with `Screen.to_styled` — add a debug renderer that shows style codes as
`[red]…[/]`); login dialog screen; search scenario (type, n, Esc); jump
scenario with three user messages at height 8.

---

## M8 — Safety and polish

- **Confirm gate for destructive tools** (off by default; `/confirm
  [on|off]` and config): when on, `bash`, `write`, `edit` are held until the
  user answers a `Confirm` dialog showing the command / path (backend emits
  `Tool_confirm {call_id; summary}`; TUI answers `tool_confirm_respond`;
  Esc denies). Deny produces an error tool result so the model sees it.
- **Bash timeout** visible in the tool line (`⚙ bash (timed out after 120s)`).
- **Backend crash surface**: `Backend_closed` shows the last 20 stderr lines
  in the transcript before quitting is offered (instead of quitting
  immediately); `Ctrl+C` then exits.
- **Resize with a dialog open** re-lays-out (already the case; add the
  test).
- `README.md`/`ARCHITECTURE.md` updated per milestone; `/help` regenerated
  from the tables (it already is).

---

## M9 — Testing harness: real screens, real terminal

Today's tests stop at `App.update` + `Render.screen` (pure) and a
protocol-level e2e. `UX_PLAN.md` §4 asks for what the user sees; M0.1 shows
why: the terminal path (`Term_app`, `Key_of_event`, `View_of_content`,
Bonsai_term itself) has no test. Three layers, cheapest first:

1. **Pure screen scenarios** (exists; extend). Every milestone above adds
   `test_app.ml` scenarios. Add the missing §4 ones now: resize 120→40 mid
   stream (no lost/duplicated lines), login flow end to end with the
   `prompt_cancelled` branch, Ctrl+C with each dialog open, wide characters
   in the editor and transcript (cursor column asserted).
2. **Rendered-frame snapshots.** Drive the *real* Bonsai_term app with
   `Bonsai_term`'s test driver if it ships one (check
   `bonsai_term/src/driver.mli` for `For_testing`), else run `Term_app`'s
   `View.t` through Notty's `Render` into an in-memory buffer and decode with
   a small VT emulator (`test/vt.ml`: cursor movement, erase, SGR, OSC-8) to
   a cell grid. Prints the grid and cursor; compares against
   `Screen.to_plain` so the two paths cannot diverge. Covers
   `View_of_content` (styles, hyperlinks) and `Key_of_event` (feeds
   `Event.t` values, including bracketed paste sequences).
3. **tmux harness** (`tui/tmux-test/`, dune alias `@tmux`, skipped when
   `tmux` is absent). Starts `main.exe -faux` with an isolated `HOME` and
   `-auth-file` inside `tmux new-session -d -x 100 -y 30`, sends keys with
   `tmux send-keys` (`C-o`, `M-Enter`, `S-Tab`, pastes via `tmux
   load-buffer`/`paste-buffer`), waits for a marker in `tmux capture-pane
   -p`, and diffs captured panes against `.expected` files (normalised:
   spinner frames, timestamps, paths). Scenarios: startup screen, prompt
   with tool call at each verbosity (Ctrl+O ×3), Shift+Tab across two
   subagents, `/model` picker, login with pasted key, resize (`tmux
   resize-window`) during streaming, Ctrl+Z/fg, Ctrl+C twice exits with
   the tty restored (`stty -a` sane afterwards). This is the only layer that
   can catch M0.1-class bugs, so it runs in CI whenever `term/` or the
   keymap changes.
4. **Keymap coverage stays mechanical**: the existing test iterates
   `Keymap.bindings`; extend it to also assert every binding appears in at
   least one `test_app` scenario (scenarios register the intents they
   exercise in a global set checked at the end of the test module) and in
   `test_key_of_event`.

CI gate: `dune build @runtest @e2e` on both projects, plus `@tmux` when tmux
is present.

---

## Deferred (explicitly not in this plan; say if you want them)

- Image attachments / clipboard image paste (needs image blocks through
  every provider and a terminal image protocol).
- HTML export (`/export` does markdown/JSONL).
- Syntax highlighting in code fences.
- Themes / settings menu; the theme is the fixed `Style` palette.
- Steering a subagent from the UI (view-only in M3).
- Session sharing (`/share`, `/web`).

---

## Order and estimates

| # | Milestone | Rough size | Depends on |
|---|---|---|---|
| 0 | Known bugs | small (1 day) | — (0.1 needs the tmux harness skeleton from 9) |
| 1 | Verbosity | small (1 day) | 0 |
| 2 | Autocomplete + `@files` | medium (2 days) | — |
| 3 | Subagents + parallel tools + agent views | large (4–5 days) | 1 |
| 4 | Editor & input | medium (2–3 days) | 2 (path completion for `!`/`@`) |
| 5 | Sessions | medium (3 days) | 4 (`Text_prompt` mode) |
| 6 | Models/thinking/status | small (1–2 days) | 5 (config RPC) |
| 7 | Rendering | medium (3 days) | 1 |
| 8 | Safety & polish | small (1–2 days) | 3 (confirm uses the event plumbing) |
| 9 | Testing harness | medium (2 days), started in 0 and grown per milestone | — |

Start with the tmux skeleton (M9.3) and M0 together, since the first bug
can only be closed with it; then 1 and 2 (independent); 3 is the most
valuable and the riskiest (backend concurrency), so it goes right after.

## Test gates for every milestone

1. `cd backend && dune build @runtest` clean (no unpromoted diffs).
2. `cd tui && dune build @runtest` clean; the keymap coverage test passes
   with every new binding exercised.
3. `cd tui && dune build @e2e` clean; the e2e script is extended whenever the
   wire protocol changes.
4. A manual pass in a real terminal for milestones touching rendering
   (1, 3, 4, 7): narrow window, paste, emoji, resize during streaming,
   Shift+Tab across agents, Ctrl+C sequence. Record it in the commit
   message.
