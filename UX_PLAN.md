# TUI UX plan and testing story

The frontend currently renders a transcript plus an editor and treats every
slash command as "print some text". That is not good enough: choosing a model
prints a list and then rejects the display name you copy from it. This document
fixes the bar we hold the TUI to, lists the concrete work to reach it, and
defines how we test it so it stays there.

## Principles we uphold

1. **Never make the user retype what we just showed them.** Anything the UI
   lists (models, sessions, providers, login methods) is chosen from a picker
   with fuzzy filtering, or by an argument that accepts what was displayed
   (id, `provider/id`, or display name, case-insensitive, unique prefix).
2. **Every error tells you what to do next.** "unknown model X" must include
   the closest matches and how to open the picker. "not logged in" names the
   `/login` command. Backend errors are shown once, not swallowed.
3. **State is always visible.** The status line shows model (`provider/id`),
   thinking level, context usage, cost, running/idle and any *mode* the UI is
   in (login prompt, picker open, quit confirmation). Mode is also reflected
   by the editor marker (`>` prompt, `?` login answer, `/` picker filter).
4. **Modes are modal and escapable.** A dialog (picker, login prompt,
   confirmation) owns the keyboard until it closes; **Esc** always closes it
   without side effects; **Enter** always means "accept the highlighted
   thing". No dialog can be stacked on another; opening one while another is
   open is a no-op with a notice.
5. **Streaming never garbles the terminal.** Only complete lines go to
   scrollback; the bottom panel is redrawn atomically; resize redraws
   correctly; wide characters and long lines wrap without cursor drift.
6. **Keyboard conventions are stable and documented.** One table
   (`keys.ts` + `/help`): Enter send/steer, Alt+Enter or Ctrl+J newline, Esc
   abort/close, Tab complete, Up/Down history or list navigation, Ctrl+C
   clear-then-quit, Ctrl+L clear screen, Ctrl+A/E/K/U/W editing. Adding a
   binding means adding a row to that table and a test.
7. **Slash commands are discoverable.** `/` alone (or Tab on a partial
   command) opens a command picker with descriptions; unknown commands
   suggest the closest one.
8. **Feedback within one frame.** Every submitted action produces an
   immediate visible acknowledgement (spinner, "queued", picker closes and
   status line changes) before the backend replies.

## Work items

### P0 — pickers and selection (the `/model` complaint)

- **`Picker` component** (`tui/picker.ts`): title, search input, filtered
  list with highlight, Up/Down/PageUp/PageDown, Enter selects, Esc cancels,
  fuzzy filter (subsequence match with ranking: prefix > word-start >
  substring > subsequence). Pure and testable: `render(width, height) →
  string[]` and `handleKey(key) → "open" | "selected" | "cancelled"`.
- **`/model [filter]`** opens the picker over `list_models`, grouped by
  provider, current model marked, showing name, key, context window and
  price; a row is dimmed with "not logged in" if the provider has no
  credential. With an argument that resolves uniquely (id, key, display
  name, unique prefix, case-insensitive) it switches directly; otherwise it
  opens the picker pre-filtered.
- **Backend `set_model` accepts display names and unique prefixes** and
  returns `unknown model "x"; did you mean: a, b, c` (edit-distance ranked).
- **`/sessions`** and **`/switch`** open a picker (date, message count, first
  prompt, cwd) instead of printing numbered lines and asking for the number.
- **`/login`** without a provider opens a picker of providers × methods with
  their configured status; **`/logout`** likewise over configured providers.
- **`/thinking`** opens a picker of levels with the current one marked.
- **`/`** (empty) or Tab with multiple completions opens the command picker.

### P1 — dialogs and modes

- **Login dialog** replaces the ad-hoc prompt mode: shows the auth URL,
  instructions, progress lines and the answer field (masked for secrets)
  in one bordered block above the editor; Esc cancels via `auth_cancel`.
- **Confirmations** for destructive actions (`/new` with unsaved... n/a, but
  `/rewind`, `/logout`, quitting mid-run) as a yes/no dialog.
- **Notices** get a severity (info / warn / error) and colour; errors from
  the backend are rendered with the error style and never truncated silently.

### P2 — transcript and status

- Tool calls render as collapsible blocks: one summary line while running
  (with live tail), full output on demand (`Ctrl+O` toggles last tool
  output); errors in red.
- Thinking is dimmed and can be hidden (`/thinking-display off`).
- Status line shows mode and hint text (`login: Enter answers, Esc cancels`,
  `picker: type to filter`), and truncates from the left so the model key is
  always visible.
- Assistant markdown: headings, code fences with language, lists, inline
  code, tables (monospace grid).

### P3 — polish

- Paste of multi-line text into the editor shows a `[N lines pasted]` chip
  rather than exploding the panel.
- Wide-char (CJK, emoji) aware width calculation in `rowsFor` and the editor
  cursor.
- Resize while a picker is open re-lays-out the picker.

## Testing story

The TUI already has the seam we need: `App` takes a `Terminal` interface
(`write`, `columns`, `onInput`, `onResize`, `setRawMode`) and a `Client`.
Everything below builds on faking those two.

### 1. Virtual terminal (the foundation)

`test/helpers/screen.ts`: a small VT emulator that consumes exactly the escape
sequences we emit (`\x1b[nA`, `\r`, `\x1b[J`, `\x1b[nC`, `\x1b[2J`, `\x1b[H`,
SGR) and maintains a grid of cells plus cursor position. Tests assert on
`screen.text()` (ANSI stripped, trailing spaces trimmed) and
`screen.cursor`, i.e. on **what the user sees**, not on byte streams. This is
what makes cursor-drift and garbling bugs testable.

### 2. Fake client

`test/helpers/fake-client.ts`: implements the `Client` surface used by `App`
(`call`, `subscribe`, `getState`, `listModels`, ...) with scripted responses
and an `emit(event)` to push backend events. Records every RPC call so tests
can assert `set_model` was called with `anthropic/claude-fable-5-1`.

### 3. Component tests (pure, fast)

Every widget is a pure function of state → lines and key → state:
`Editor`, `Picker`, `fuzzyFilter`, `renderStatus`, `renderToolResult`,
markdown. Tests are table-driven: input keys, expected lines, expected
cursor. Fuzzy filter gets a ranking table (query, candidates, expected order).

### 4. App-level scenario tests (headless)

Drive `App` with `FakeTerminal` + `FakeClient`, feed keystrokes as raw bytes
through `parseKeys`, and snapshot the virtual screen at each step. Required
scenarios, one per principle:

- `/model` → picker open → type `fable` → Enter → `set_model` called with the
  right key → status line shows it → picker gone, editor focused.
- `/model claude fable 5.1` (display name) switches without the picker;
  `/model zzz` shows "did you mean" and opens the picker pre-filtered.
- Streaming: emit `message_update` deltas with embedded newlines and a
  mid-word tail; screen shows complete lines in scrollback and the tail in
  the panel; the cursor is in the editor after every event.
- Login: `/login anthropic` → `auth_url` event renders URL → `prompt` event
  switches editor into masked mode → typing shows `***` → Enter calls
  `auth_respond(id, secret)` → `done` event switches model. Esc mid-prompt
  calls `auth_cancel` and restores the normal prompt. `prompt_cancelled`
  from the backend restores it too.
- Esc closes any open dialog and does not call `abort`; Esc with no dialog
  and a running turn calls `abort`.
- Ctrl+C: clears text, then asks, then quits; never quits with a dialog
  open without cancelling it.
- Resize from 120 to 40 columns mid-stream: no duplicated or lost lines.
- Every entry in the keybinding table is exercised by at least one test
  (a test iterates the table and fails on an unexercised binding).

### 5. Snapshot files

Screen snapshots are stored as plain text next to the tests
(`test/__snapshots__/*.txt`) with a `UPDATE_SNAPSHOTS=1` mode, mirroring the
backend's expect-test/promote workflow. Reviewers read the diff of a screen,
not of escape codes.

### 6. End-to-end with the real backend

`test/e2e.test.ts` spawns `backend/_build/default/bin/main.exe serve -faux`
through the real `Client`, with an isolated `-auth-file` and `HOME` in a
temp dir, and runs a short script: prompt → tool call → `/model` picker →
`/login deepseek` → answer key → `/auth` shows it → quit. Guards the wire
contract (`protocol.ts` guards vs `Rpc_json`) so a backend field rename
fails here rather than at runtime.

### 7. Manual checklist

For changes to rendering primitives (`app.ts` redraw, `rowsFor`, `keys.ts`),
`frontend/CHECKLIST.md` lists a 2-minute manual pass in a real terminal:
narrow window, paste, emoji, resize during streaming, Ctrl+C sequence. A
PR touching those files must say the checklist was run.

### CI gate

`npm test` runs 3–6; the backend `dune build @runtest` covers the RPC side.
Snapshot changes must be committed alongside the change that caused them.
