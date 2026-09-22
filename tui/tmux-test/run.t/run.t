Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh startup
  === initial screen
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00
  $ bash ./harness.sh prompt
  === typed
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > hello there
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00
  === after reply
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > hello there
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 10  $0.00
  $ bash ./harness.sh ctrl_o
  === after C-o
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  view: verbose — everything is shown
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:verbose  ctx:0% 0  $0.00
  $ bash ./harness.sh resize
  === 40x12
  session <id> in
  $TMP/cw
  d. /help for commands, Esc aborts,
  Ctrl+C twice quits.
  > first
  faux reply
  ────────────────────────────────────────
  >
  …deepseek-flash  ctx:0% 10  $0.00
  === 100x30
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > first
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 10  $0.00
  $ bash ./harness.sh quit
  === after first C-c
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  press Ctrl+C again to quit
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00  Ctrl+C again quits
  === exited (tty flags)
  EXITED
  ixon isig icanon iexten echo
  $ bash ./harness.sh tools
  === normal
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > go
  let me look
  ⚙ bash command=printf 'one\ntwo\nthree\n'
    one
    two
    three
  three lines. now delegating
  ⚙ subagent "count files" ✓ 1 turns $0.00
    child one reporting: 0 files
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ⚙ subagent "say hello" ✓ 1 turns $0.00
    child two reporting: hello
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00  agents:[main] 1✓ 2✓
  === verbose
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > go
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
    child one reporting: 0 files
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ⚙ subagent "say hello" ✓ 1 turns $0.00
    child two reporting: hello
    [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  all done
  view: verbose — everything is shown
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:off  view:verbose  ctx:0% 0  $0.00  agents:[main] 1✓ 2✓
  === quiet
  > go
  let me look
  ⚙ bash printf 'one\ntwo\nthree\n' ✓ 3 lines
  three lines. now delegating
  ⚙ subagent "count files" ✓ 1 turns $0.00
  ⚙ subagent "say hello" ✓ 1 turns $0.00
  all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:off  view:quiet  ctx:0% 0  $0.00  agents:[main] 1✓ 2✓
  === agent 1
  ◆ subagent 1/2  deepseek-flash  ✓ done 1 turns $0.00  "count files"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  > count files
  child one reporting: 0 files
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:off  view:quiet  ctx:0% 0  $0.00  agents:main [1✓] 2✓
  === agent 2
  ◆ subagent 2/2  deepseek-flash  ✓ done 1 turns $0.00  "say hello"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  > say hello
  child two reporting: hello
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:off  view:quiet  ctx:0% 0  $0.00  agents:main 1✓ [2✓]
  === main again
  > go
  let me look
  ⚙ bash printf 'one\ntwo\nthree\n' ✓ 3 lines
  three lines. now delegating
  ⚙ subagent "count files" ✓ 1 turns $0.00
  ⚙ subagent "say hello" ✓ 1 turns $0.00
  all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:off  view:quiet  ctx:0% 0  $0.00  agents:[main] 1✓ 2✓
  $ bash ./harness.sh suspend
  === suspended (shell visible)
  [1]+  Stopped                 sh $TMP/run.sh
  bash-5.2$
  === after fg (repainted)
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > before
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 10  $0.00
  === editor works
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > before
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > still typing
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 10  $0.00
  $ bash ./harness.sh editor
  === after Ctrl+G round trip
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > draftedited by editor
  
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00
  $ bash ./harness.sh confirm
  === dialog
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > go
  running
  ⚙ bash command=echo ran-it
  ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
  │ Run bash: echo ran-it? (y/n)                                                                     │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  ?
  …deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00  confirm: y / n
  === allowed
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > go
  running
  ⚙ bash command=echo ran-it
    ran-it
  first done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00
  === denied
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
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
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00
  $ bash ./harness.sh paste
  === chip after a 5-line paste
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > [5 lines pasted]
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00
  === cursor inside expands the chip
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > line one
    line two
    line three
    line four
    line five
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 0  $0.00
  === submitted as one message
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > line one
  > line two
  > line three
  > line four
  > line five
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 10  $0.00
  $ bash ./harness.sh reconnect
  === after the backend was killed
  reconnected to the backend
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > before
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 10  $0.00
  === prompt works again
  reconnected to the backend
  session <id> in $TMP/cwd. /help for commands, Esc
  aborts, Ctrl+C twice quits.
  > before
  faux reply
  > after
  faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:off  view:normal  ctx:0% 10  $0.00
