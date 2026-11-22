Typing, resizing and scrolling (starting, a prompt and quitting are in
smoke.t).
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh ctrl_o
  === after C-o
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ──────────────────────────────────────────────────────────── view: verbose — everything is shown ───
  >
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh resize
  === 40x12
    prigh in $TMP/cwd ·
    /help · Esc aborts · Ctrl+C twice
    quits
  
  ▢ Using prigh-tui
  ▌ first
    faux reply
  ────────────────────────────────────────
  >
  deepseek-flash  mode:auto  ctx:0.0%/1.0M
  === 100x30
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ first
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh scroll
  === scrolled back
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ prompt 1
    faux reply
  
  ▌ prompt 2
    faux reply
                                                                                        ↓ bottom · End
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  === Ctrl+End: at the bottom
  ▌ prompt 2
    faux reply
  
  ▌ prompt 3
    faux reply
  
  ▌ prompt 4
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > draft
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  === clicked: at the bottom
  ▌ prompt 2
    faux reply
  
  ▌ prompt 3
    faux reply
  
  ▌ prompt 4
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > draft
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
