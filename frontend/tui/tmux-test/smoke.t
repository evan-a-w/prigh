The default run's real-terminal smoke test: the built TUI starts against the
real backend, sends a prompt and gets the reply, and quits, leaving the
terminal as it found it. The other groups run in `dune build @runtest-full`.

  $ bash ./harness.sh startup
  === initial screen
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh prompt
  === typed
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > hello there
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  === after reply
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▢ Using prigh-tui
  ▌ hello there
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:default  mode:auto  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh quit
  === after first C-c
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:default  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  Ctrl+C again quits
  === exited (tty flags)
  EXITED
  ixon isig icanon iexten echo
