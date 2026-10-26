The backend going away and coming back, and switching sessions meanwhile.
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh reconnect
  === after the backend was killed
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ before
    faux reply
  ───────────────────────────────────────────────────────────────────── reconnected to the backend ───
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === prompt works again
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ before
    faux reply
  
  ▌ after
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh fallback
  === chain and default directory set
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ┌─ Notice ─────────────────────────────────────────────────────────────────────────────────────────┐
  │ default directory: $TMP/cwd (new sessions start there; /default-dir off clears │
  │ it)                                                                                              │
  └────────────────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === handed over
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    error: HTTP 429: The usage limit has been reached (usage limit reached)
    deepseek/deepseek-flash: HTTP 429: The usage limit has been reached (usage limit reached); handing
    over to anthropic/claude-fable-5-1
  
  ↪ Handed over from deepseek/deepseek-flash to anthropic/claude-fable-5-1
    carried on
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  claude-fable-5-1  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh reconnect_dialog
  === typing the reason
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    asking
  … bash $ make deploy
  ┌─ Why not? (the model sees it) ───────────────────────────────────────────────────────────────────┐
  │ Run bash: make deploy?                                                                           │
  │ › not on a Fri                                                                                   │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  Enter denies · Esc goes back
  === reconnected: the dialog closed
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    asking
  … bash $ make deploy
  ───────────────────────────────────────────────────────────────────── reconnected to the backend ───
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh switch_pending
  === the picker, a confirmation waiting behind it
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    waiting
  ✓ bash $ sleep 2
    now
  … bash $ rm -rf build
  Sessions  (2)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
  /
    ── cwd ──
   * ▶ (unnamed) ∣ <time> ∣ 2 msgs ∣ go   $TMP/cwd
  ▸  (unnamed) ∣ <time> ∣ 2 msgs ∣ hello  $TMP/cwd
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  ctx:0.0%/1.0M  to answer:1  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
  === switched: no dialog from the other session
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ hello
    in the first session
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
  === back: its confirmation again
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    waiting
  ✓ bash $ sleep 2
    now
  … bash $ rm -rf build
  ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
  │ Run bash: rm -rf build?                                                                          │
  │ y  allow once                                                                                    │
  │ a  always allow bash(rm:*) this session                                                          │
  │ n  deny                                                                                          │
  │ r  deny, saying why                                                                              │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  ?
  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y/a/n/r
  === answered
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    waiting
  ✓ bash $ sleep 2
    now
  ✓ bash $ rm -rf build
    allowed bash
    all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
