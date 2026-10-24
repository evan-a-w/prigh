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
  ↩ subagent a1 "count files"  finished
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ↩ subagent a2 "say hello"  finished
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
    all done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === verbose
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
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
  ↩ subagent a1 "count files"  finished
      child reporting: done
      [subagent: 1 turns, 0 in / 0 out tokens, $0.0000]
  ↩ subagent a2 "say hello"  finished
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
  ↩ subagent a1 "count files"  finished
  ↩ subagent a2 "say hello"  finished
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
  ↩ subagent a1 "count files"  finished
  ↩ subagent a2 "say hello"  finished
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
  │ ↑↓ choose · type your own · Enter picks · ←→ questions · Esc declines (the model is told)        │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  …deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00  Enter accepts · Esc declines
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
  …deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00  Enter accepts · Esc declines
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
  $ bash ./harness.sh switch_pending
  === the picker, a confirmation waiting behind it
    new session
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    waiting
  ✓ bash $ sleep 2
    now
  … bash $ rm -rf build
  Sessions  (2)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
  /
   * ▶ (unnamed) ∣ <time> ∣ 2 msgs ∣ go   $TMP/cwd
  ▸  (unnamed) ∣ <time> ∣ 2 msgs ∣ hello  $TMP/cwd
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  …deepseek-flash  mode:ask  to answer:1  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
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
  …deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  confirm: y/a/n/r
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
  …deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00  Enter denies · Esc goes back
  === reconnected: the dialog closed
    reconnected to the backend
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ go
    asking
  … bash $ make deploy
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:ask  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh auto_background
  === moved to the background
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ build it
    building
  ✓ bash $ echo compiling; while [ ! -f gate ]; do sleep 0.0…  job j1 moved to the background after 1s
      compiling
    carrying on meanwhile
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00  jobs:1 <spin>
  === its report
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ build it
    building
  ✓ bash $ echo compiling; while [ ! -f gate ]; do sleep 0.0…  job j1 moved to the background after 1s
      compiling
    carrying on meanwhile
  ↩ job j1 "echo compiling; while [ ! -f gate ]; do sleep 0.05; done; e…"  exited 0
      compiling
      linked
    the build finished
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh connect_signin
  === refused: the sign-in dialog
  ┌─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token                                                                                            │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  Tab next field · Enter signs in · Esc closes
  === the token, masked
  ┌─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token  ******                                                                                    │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  Tab next field · Enter signs in · Esc closes
  === signed in
    connected to 127.0.0.1:<port>
    tools now run on <host> in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  server:127.0.0.1:<port>
  === saved (600)
  ((connect 127.0.0.1:<port>) (token s3cret))
  === signed out
    signed out of 127.0.0.1:<port>; the saved token is forgotten
  ┌─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token                                                                                            │
  │ signed out                                                                                       │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  Tab next field · Enter signs in · Esc closes
  === saved (600)
  ((connect 127.0.0.1:<port>))
  === closed: disconnected
    signed out of 127.0.0.1:<port>; the saved token is forgotten
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  not signed in to 127.0.0.1:<port> · /signin signs in · /connect switches server
  === a local backend again
    connected to a local backend
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  $ bash ./harness.sh startup_signin
  === refused at startup
  ┌─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token                                                                                            │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  Tab next field · Enter signs in · Esc closes
  === signed in
    connected to 127.0.0.1:<port>
    tools now run on <host> in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  …deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  server:127.0.0.1:<port>
  === saved (600)
  ((connect 127.0.0.1:<port>) (token s3cret))
  $ bash ./harness.sh detached
  === from inside tmux, relative DIR and -auth-file, odd values
  prigh is running in tmux session prigh-cwd: $TMP/tmux.sh switch-client -t prigh-cwd
  exit 0, at once
  === sessions: other prigh-cwd
  === the TUI in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === the pane: in $TMP/cwd
  HOME=$TMP/home
  PRIGH_BACKEND=$BACKEND
  PRIGH_ODD=sp ace "dq" 'sq' $HOME `tick` #{pane_id} semi;|line two
  PRIGH_TMUX=$TMP/tmux.sh
  TMUX=<the private server>
  === again, outside tmux: -2
  prigh is running in tmux session prigh-cwd-2: $TMP/tmux.sh attach -t prigh-cwd-2
  exit 0, at once
  === sessions: other prigh-cwd prigh-cwd-2
  === prigh-cwd-2 answers
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ hello
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  $TMP/cwd  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00
  === quit: the session closed
  === sessions: other prigh-cwd
  === a TUI that fails keeps its pane until Enter
  prigh is running in tmux session broken_one: $TMP/tmux.sh attach -t broken_one
  exit 0, at once
  === the error
  cannot reach a local backend: (Unix.Unix_error "No such file or directory" Core_unix.create_process
   "((prog /nonexistent/prigh) (args (serve -faux -cwd $TMP/cwd)) (env (Extend ())))
  ")
  
  prigh-tui exited with status 1: press Enter to close this session
  === sessions: other prigh-cwd
  === errors before detaching
  no such directory: $TMP/missing
  exit 2, at once
  -connect cannot be used with -local or -faux: pass one
  exit 2, at once
  cannot run $PRIGH_TMUX (/nonexistent/tmux: No such file or directory): set it to a tmux executable, or unset it to use tmux from PATH
  exit 1, at once
  === sessions: other prigh-cwd
