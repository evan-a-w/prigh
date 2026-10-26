Starting, typing, resizing and quitting.
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh startup
  === initial screen
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh prompt
  === typed
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > hello there
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === after reply
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ hello there
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh ctrl_o
  === after C-o
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ──────────────────────────────────────────────────────────── view: verbose — everything is shown ───
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:verbose  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh resize
  === 40x12
    prigh in $TMP/cwd ·
    /help · Esc aborts · Ctrl+C twice
    quits
  
  ▌ first
    faux reply
  ────────────────────────────────────────
  >
  deepseek-flash  think:on  ctx:0.0%/1.0M
  === 100x30
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ first
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh quit
  === after first C-c
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  Ctrl+C again quits
  === exited (tty flags)
  EXITED
  ixon isig icanon iexten echo
