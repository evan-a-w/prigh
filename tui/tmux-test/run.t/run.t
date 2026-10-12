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
    view: verbose — everything is shown
  ────────────────────────────────────────────────────────────────────────────────────────────────────
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
  …deepseek-flash  think:on  ctx:0.0%/1.0M
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
    press Ctrl+C again to quit
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  Ctrl+C again quits
  === exited (tty flags)
  EXITED
  ixon isig icanon iexten echo
  $ bash ./harness.sh tools
  === normal
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    let me look
  ✓ bash $ printf 'one\ntwo\nthree\n'
      one
      two
      three
    three lines. now delegating
  ✓ subagent count files  1 turn $0.00
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ✓ subagent say hello  1 turn $0.00
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    waiting for both
  ✓ subagent_wait
      [subagent a1 finished] count files
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  
      [subagent a2 finished] say hello
      … 2 more lines
    all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === verbose
    let me look
  ✓ bash $ printf 'one\ntwo\nthree\n'
      {
        command: "printf 'one\\ntwo\\nthree\\n'"
      }
      one
      two
      three
    three lines. now delegating
  ✓ subagent count files  1 turn $0.00
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ✓ subagent say hello  1 turn $0.00
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    waiting for both
  ✓ subagent_wait
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
  …deepseek-flash  think:on  mode:auto  view:verbose  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === quiet
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    let me look
  ✓ bash $ printf 'one\ntwo\nthree\n'  3 lines
    three lines. now delegating
  ✓ subagent count files  1 turn $0.00
  ✓ subagent say hello  1 turn $0.00
    waiting for both
  ✓ subagent_wait  7 lines
    all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:quiet  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === agent 1
  ◆ subagent 1/2  deepseek-flash  ✓ done 1 turns $0.00  "count files"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ count files
    child reporting: done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:quiet  ctx:0.0%/1.0M  $0.00  agents:main [1✓] 2✓
  === agent 2
  ◆ subagent 2/2  deepseek-flash  ✓ done 1 turns $0.00  "say hello"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ say hello
    child reporting: done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:quiet  ctx:0.0%/1.0M  $0.00  agents:main 1✓ [2✓]
  === main again
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    let me look
  ✓ bash $ printf 'one\ntwo\nthree\n'  3 lines
    three lines. now delegating
  ✓ subagent count files  1 turn $0.00
  ✓ subagent say hello  1 turn $0.00
    waiting for both
  ✓ subagent_wait  7 lines
    all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:quiet  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
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
  …deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y/a/n/r
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
  …deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y/a/n/r
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
    mode: auto-edit — file edits run; bash, job_input and MCP tools ask first
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto-edit  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh plan
  === the plan to approve
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
    mode: plan — read-only planning: no file edits; bash, job_input and MCP tools ask first
  
  ◇ Plan mode on: the model was told to plan, not change files
  ▌ add a.txt
    looking
  ✕ write a.txt  1 line
      [plan mode] Files cannot be changed while planning. Keep investigating with read-only tools,
      then call exit_plan_mode with the plan for the user to approve.
    planned
  … exit_plan_mode  waiting for your approval
      1. Write a.txt
      2. Check it
  ┌─ Plan ───────────────────────────────────────────────────────────────────────────────────────────┐
  │ Approve the plan and start implementing?                                                         │
  │ 1. Write a.txt                                                                                   │
  │ 2. Check it                                                                                      │
  │                                                                                                  │
  │ y  approve: leave plan mode and implement                                                        │
  │ n  keep planning                                                                                 │
  │ r  keep planning, saying what to change                                                          │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  ?
  …deepseek-flash  think:on  mode:plan  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y/n/r
  === approved and implemented
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
    mode: plan — read-only planning: no file edits; bash, job_input and MCP tools ask first
  
  ◇ Plan mode on: the model was told to plan, not change files
  ▌ add a.txt
    looking
  ✕ write a.txt  1 line
      [plan mode] Files cannot be changed while planning. Keep investigating with read-only tools,
      then call exit_plan_mode with the plan for the user to approve.
    planned
  ✓ exit_plan_mode  approved
      1. Write a.txt
      2. Check it
    plan approved
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
  │ Enter picks                                                                                      │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  …deepseek-flash  ctx:0.0%/1.0M  ↑↓ choose · Space picks · type to answer · ←→ questions · Enter acc…
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
  │ Enter answers                                                                                    │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  …deepseek-flash  ctx:0.0%/1.0M  ↑↓ choose · Space picks · type to answer · ←→ questions · Enter acc…
  === answered
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    asking
  ✓ ask_user
      Q: Which database?
      A: SQLite
      Q: Anything else?
      A: keep it small
    answered
    thanks
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
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
  $ bash ./harness.sh reconnect
  === after the backend was killed
    reconnected to the backend
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ before
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === prompt works again
    reconnected to the backend
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
    fallback: deepseek/deepseek-flash → anthropic/claude-fable-5-1 (now on deepseek/deepseek-flash)
    default directory: $TMP/cwd (new sessions start there; /default-dir off clears
    it)
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === handed over
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
    fallback: deepseek/deepseek-flash → anthropic/claude-fable-5-1 (now on deepseek/deepseek-flash)
    default directory: $TMP/cwd (new sessions start there; /default-dir off clears
    it)
  
  ▌ go
    error: HTTP 429: The usage limit has been reached (usage limit reached)
    deepseek/deepseek-flash: HTTP 429: The usage limit has been reached (usage limit reached); handing
    over to anthropic/claude-fable-5-1
  
  ↪ Handed over from deepseek/deepseek-flash to anthropic/claude-fable-5-1
    carried on
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  claude-fable-5-1  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh send_raw
  === the popup highlights a directory
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > /cd $TMP/cwd/sub/
  ▸ $TMP/cwd/sub/deeper/
  …deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00  Tab/Enter accept · Alt+S as typed
  === Alt+S: /cd to what was typed
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
    cwd changed
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd/sub  deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00
