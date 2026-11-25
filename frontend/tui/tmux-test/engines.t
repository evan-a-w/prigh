The composer engine: a Codex session (a stand-in `codex` CLI) and the
CLIs' statuses.
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh engines
  === a Codex session's turn
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ hello
    Hello from Codex.
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  Codex · gpt-5.2-codex  think:default  mode:auto  view:normal  ctx:0.0%/200k  subscription
  === /compact, by Codex itself
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ hello
    Hello from Codex.
    Codex compacted its context
  ────────────────────────────────────────────────────────────────────────────── context compacted ───
  >
  Codex · gpt-5.2-codex  think:default  mode:auto  view:normal  ctx:0.0%/200k  subscription
  === a control Codex sessions do not have
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ hello
    Hello from Codex.
    Codex compacted its context
  ─────────────────────── Codex sessions have no side questions (/btw): ask in the session instead ───
  >
  Codex · gpt-5.2-codex  think:default  mode:auto  view:normal  ctx:0.0%/200k  subscription
  === /adapters
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ hello
    Hello from Codex.
    Codex compacted its context
  ┌─ Agent CLIs ─────────────────────────────────────────────────────────────────────────────────────┐
  │ laptop:                                                                                          │
  │   ✓ Codex  0.166.0 · logged in (ChatGPT)                                                         │
  │   ✗ Claude Code  not installed                                                                   │
  │       Claude Code is not installed on laptop, please install it                                  │
  │                                                                                                  │
  │ prigh never installs them or logs in: run that on the host                                       │
  └───────────────────────────────────────────────────────────────────────────────────── Esc closes ─┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  Codex · gpt-5.2-codex  mode:auto  ctx:0.0%/200k  ↑↓ scroll · Esc closes · typing goes to the editor
