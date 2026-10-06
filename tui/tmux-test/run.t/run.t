Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh startup
  === initial screen
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh prompt
  === typed
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > hello there
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  === after reply
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > hello there
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh ctrl_o
  === after C-o
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  view: verbose — everything is shown
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:verbose  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh resize
  === 40x12
  prigh in $TMP/cwd ·
  /help · Esc aborts · Ctrl+C twice quits
  > first
  faux reply
  ────────────────────────────────────────
  >
  …deepseek-flash  ctx:0.0%/1.0M  $0.00
  === 100x30
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > first
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh quit
  === after first C-c
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  press Ctrl+C again to quit
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00  Ctrl+C again quits
  === exited (tty flags)
  EXITED
  ixon isig icanon iexten echo
  $ bash ./harness.sh tools
  === normal
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > go
  let me look
  ⚙ bash command=printf 'one\ntwo\nthree\n'
    one
    two
    three
  three lines. now delegating
  ⚙ subagent "count files" ✓ 1 turns $0.00
    child reporting: done
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ⚙ subagent "say hello" ✓ 1 turns $0.00
    child reporting: done
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  waiting for both
  ⚙ subagent_wait
    [subagent a1 finished] count files
    child reporting: done
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  
    [subagent a2 finished] say hello
    … (2 more)
  all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === verbose
  let me look
  ⚙ bash
    {
      command: "printf 'one\\ntwo\\nthree\\n'"
    }
    one
    two
    three
  three lines. now delegating
  ⚙ subagent "count files" ✓ 1 turns $0.00
    child reporting: done
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ⚙ subagent "say hello" ✓ 1 turns $0.00
    child reporting: done
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  waiting for both
  ⚙ subagent_wait
    {}
    [subagent a1 finished] count files
    child reporting: done
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  
    [subagent a2 finished] say hello
    child reporting: done
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  all done
  view: verbose — everything is shown
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  view:verbose  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === quiet
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > go
  let me look
  ⚙ bash printf 'one\ntwo\nthree\n' ✓ 3 lines
  three lines. now delegating
  ⚙ subagent "count files" ✓ 1 turns $0.00
  ⚙ subagent "say hello" ✓ 1 turns $0.00
  waiting for both
  ⚙ subagent_wait ✓ 7 lines
  all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  view:quiet  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === agent 1
  ◆ subagent 1/2  deepseek-flash  ✓ done 1 turns $0.00  "count files"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  > count files
  child reporting: done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  view:quiet  ctx:0.0%/1.0M  $0.00  agents:main [1✓] 2✓
  === agent 2
  ◆ subagent 2/2  deepseek-flash  ✓ done 1 turns $0.00  "say hello"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  > say hello
  child reporting: done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  view:quiet  ctx:0.0%/1.0M  $0.00  agents:main 1✓ [2✓]
  === main again
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > go
  let me look
  ⚙ bash printf 'one\ntwo\nthree\n' ✓ 3 lines
  three lines. now delegating
  ⚙ subagent "count files" ✓ 1 turns $0.00
  ⚙ subagent "say hello" ✓ 1 turns $0.00
  waiting for both
  ⚙ subagent_wait ✓ 7 lines
  all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  view:quiet  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  $ bash ./harness.sh suspend
  === suspended (shell visible)
  [1]+  Stopped sh $TMP/run.sh
  bash$
  === after fg (repainted)
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > before
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  === editor works
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > before
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > still typing
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh editor
  === after Ctrl+G round trip
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > draftedited by editor
  
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh confirm
  === dialog
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > go
  running
  ⚙ bash command=echo ran-it
  ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
  │ Run bash: echo ran-it? (y/n)                                                                     │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  ?
  …deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y / n
  === allowed
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > go
  running
  ⚙ bash command=echo ran-it
    ran-it
  first done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  === denied
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > go
  running
  ⚙ bash command=echo ran-it
    ran-it
  first done
  > go again
  again
  ⚙ bash command=echo never
    [denied by user]
  denied bash
  second done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh paste
  === chip after a 5-line paste
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > [5 lines pasted]
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  === cursor inside expands the chip
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > line one
    line two
    line three
    line four
    line five
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  === submitted as one message
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > line one
  > line two
  > line three
  > line four
  > line five
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh reconnect
  === after the backend was killed
  reconnected to the backend
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > before
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  === prompt works again
  reconnected to the backend
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  > before
  faux reply
  > after
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh fallback
  === chain and default directory set
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  fallback: deepseek/deepseek-flash → anthropic/claude-fable-5-1 (now on deepseek/deepseek-flash)
  default directory: $TMP/cwd (new sessions start there; /default-dir off clears it)
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  view:normal  ctx:0.0%/1.0M  $0.00
  === handed over
  prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  fallback: deepseek/deepseek-flash → anthropic/claude-fable-5-1 (now on deepseek/deepseek-flash)
  default directory: $TMP/cwd (new sessions start there; /default-dir off clears it)
  > go
  error: HTTP 429: The usage limit has been reached (usage limit reached)
  deepseek/deepseek-flash: HTTP 429: The usage limit has been reached (usage limit reached); handing
  over to anthropic/claude-fable-5-1
  ↪ handed over from deepseek/deepseek-flash to anthropic/claude-fable-5-1
  carried on
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  claude-fable-5-1  think:on  view:normal  ctx:0.0%/1.0M  $0.00
