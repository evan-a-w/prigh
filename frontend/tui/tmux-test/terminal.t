Handing the terminal over: job control, the editor, a paste.
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh suspend
  === suspended (shell visible)
  [1]+  Stopped sh $TMP/run.sh
  bash$
  === after fg (repainted)
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ before
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === editor works
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ before
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > still typing
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh editor
  === after Ctrl+G round trip
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > draftedited by editor
  
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh paste
  === chip after a 5-line paste
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > [5 lines pasted]
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === cursor inside expands the chip
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > line one
    line two
    line three
    line four
    line five
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === submitted as one message
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ line one
  ▌ line two
  ▌ line three
  ▌ line four
  ▌ line five
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
