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
  $TMP/cwd  Codex · gpt-5.2-codex  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  === a control Codex sessions do not have
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ hello
    Hello from Codex.
  ─────────────────── Codex compacts its context by itself when it fills: /compact isn't available ───
  >
  $TMP/cwd  Codex · gpt-5.2-codex  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  === /adapters
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ hello
    Hello from Codex.
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
  Codex · gpt-5.2-codex  mode:auto  ctx:0.0%/1.0M  ↑↓ scroll · Esc closes · typing goes to the editor
