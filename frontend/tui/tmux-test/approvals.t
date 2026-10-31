What waits for the user: confirmations, approvals, plan mode, questions.
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh confirm
  === dialog
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    running
  … bash $ echo ran-it
  ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
  │ Run bash: echo ran-it?                                                                           │
  │ y  allow once                                                                                    │
  │ a  always allow bash(echo:*) this session                                                        │
  │ n  deny                                                                                          │
  │ r  deny, saying why                                                                              │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  ?
  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y/a/n/r
  === allowed
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    running
  ✓ bash $ echo ran-it
      ran-it
    allowed bash
    first done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
  === denied
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    running
  ✓ bash $ echo ran-it
      ran-it
    allowed bash
    first done
  
  ▌ go again
    again
  ✕ bash $ echo never
      [denied by user]
    denied bash
    second done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh approvals
  === dialog with always
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    one
  … bash $ echo first-run
  ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
  │ Run bash: echo first-run?                                                                        │
  │ y  allow once                                                                                    │
  │ a  always allow bash(echo:*) this session                                                        │
  │ n  deny                                                                                          │
  │ r  deny, saying why                                                                              │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  ?
  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y/a/n/r
  === the second echo ran without asking
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    one
  ✓ bash $ echo first-run
      first-run
    allowed bash; always: bash(echo:*)
    first done
  
  ▌ again
    two
  ✓ bash $ echo second-run
      second-run
    second done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
  === denied with a reason
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    one
  ✓ bash $ echo first-run
      first-run
    allowed bash; always: bash(echo:*)
    first done
  
  ▌ again
    two
  ✓ bash $ echo second-run
      second-run
    second done
  
  ▌ deploy
    three
  ✕ bash $ make deploy
      [denied by user] Their reason: not on a Friday
    denied bash
    third done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
  === Alt+M: auto-edit
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    one
  ✓ bash $ echo first-run
      first-run
    allowed bash; always: bash(echo:*)
    first done
  
  ▌ again
    two
  ✓ bash $ echo second-run
      second-run
    second done
  
  ▌ deploy
    three
  ✕ bash $ make deploy
      [denied by user] Their reason: not on a Friday
    denied bash
    third done
  ────────────────────── mode: auto-edit — file edits run; bash, job_input and MCP tools ask first ───
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto-edit  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh plan
  === the plan, in plan mode
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ◇ Plan mode on: the model was told to plan, not change files
  ▌ add a.txt
    looking
  ✕ write a.txt  1 line
      [plan mode] Plan mode is on: you are planning, not implementing, so files, worktrees and pull
      requests cannot be changed. Only the user turns plan mode off; do not try to get around this or
      ask them to. Keep investigating with read-only tools, then present the plan in your reply and
      wait.
    Plan:
    1. Write a.txt
    2. Check it
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:plan  (/plan off or Alt+M leaves)  view:normal  ctx:0.0%/1.0M  $0.00
  === plan mode off, implemented
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ◇ Plan mode on: the model was told to plan, not change files
  ▌ add a.txt
    looking
  ✕ write a.txt  1 line
      [plan mode] Plan mode is on: you are planning, not implementing, so files, worktrees and pull
      requests cannot be changed. Only the user turns plan mode off; do not try to get around this or
      ask them to. Keep investigating with read-only tools, then present the plan in your reply and
      wait.
    Plan:
    1. Write a.txt
    2. Check it
  
  ◇ Plan mode off: the model was told it can change files
  ▌ go ahead
    implementing
  ✓ write a.txt  1 line
    all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh question
  === first question
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    asking
  … ask_user  waiting for your answer
      ? Which database?
      ? Anything else?
  ┌─ Storage (1/2) ──────────────────────────────────────────────────────────────────────────────────┐
  │ Which database?                                                                                  │
  │ › ( ) Postgres  what prod uses                                                                   │
  │   ( ) SQLite                                                                                     │
  │   Other:                                                                                         │
  │ ↑↓ choose · type your own · Enter picks · ←→ questions · Esc declines (the model is told)        │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  Enter accepts · Esc declines
  === second question
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    asking
  … ask_user  waiting for your answer
      ? Which database?
      ? Anything else?
  ┌─ Question 2/2 ───────────────────────────────────────────────────────────────────────────────────┐
  │ Anything else?                                                                                   │
  │ › keep it small                                                                                  │
  │ type an answer · Enter answers · ←→ questions · Esc declines (the model is told)                 │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  Enter accepts · Esc declines
  === answered
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    asking
  ✓ ask_user
      Q: Which database?
      A: SQLite
      Q: Anything else?
      A: keep it small
    thanks
  ─────────────────────────────────────────────────────────────────────────────────────── answered ───
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
