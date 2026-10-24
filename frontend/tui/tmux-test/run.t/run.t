Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh startup
  === initial screen
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh prompt
  === typed
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  > hello there
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === after reply
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ hello there
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh ctrl_o
  === after C-o
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up ─────────────── view: verbose — everything is shown ──
  >
  deepseek-flash  think:on  mode:auto  view:verbose                         ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh resize
  === 40x12
  prigh  new session                 …/cwd
  
  
  
  
  
  
  ▌ first
    faux reply
  ── /help · Tab commands ────────────────
  >
  deepseek-flash       ctx ▱▱▱▱▱ 0.0%/1.0M
  === 100x30
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ first
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh quit
  === after first C-c
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── Ctrl+C again quits ──────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === exited (tty flags)
  EXITED
  ixon isig icanon iexten echo
  $ bash ./harness.sh tools
  === normal
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
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
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal     agents:[main] 1✓ 2✓  ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === verbose
  prigh  new session                                                        $TMP/cwd
  
  
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
  ── /help · Tab commands · Alt+Enter follow-up ─────────────── view: verbose — everything is shown ──
  >
  deepseek-flash  think:on  mode:auto  view:verbose    agents:[main] 1✓ 2✓  ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === quiet
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  ── /help · Tab commands ─────────────── view: quiet — intermediate output and thinking are hidden ──
  >
  deepseek-flash  think:on  mode:auto  view:quiet      agents:[main] 1✓ 2✓  ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === agent 1
  prigh  new session                                                        $TMP/cwd
  ◆ subagent 1/2  deepseek-flash  ✓ done 1 turns $0.00  "count files"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ count files
    child reporting: done
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:quiet      agents:main [1✓] 2✓  ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === agent 2
  prigh  new session                                                        $TMP/cwd
  ◆ subagent 2/2  deepseek-flash  ✓ done 1 turns $0.00  "say hello"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ say hello
    child reporting: done
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:quiet      agents:main 1✓ [2✓]  ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === main again
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:quiet      agents:[main] 1✓ 2✓  ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh suspend
  === suspended (shell visible)
  [1]+  Stopped sh $TMP/run.sh
  bash$
  === after fg (repainted)
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ before
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === editor works
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ before
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  > still typing
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh editor
  === after Ctrl+G round trip
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  > draftedited by editor
  
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh confirm
  === dialog
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Confirm ────────────────────────────────────────────────────────────────────────────────────────╮
  │ Run bash: echo ran-it?                                                                           │
  │ y  allow once                                                                                    │
  │ a  always allow bash(echo:*) this session                                                        │
  │ n  deny                                                                                          │
  │ r  deny, saying why                                                                              │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── confirm: y/a/n/r ────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === allowed
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ go
    running
  ✓ bash $ echo ran-it
      ran-it
    allowed bash
    first done
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === denied
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh approvals
  === dialog with always
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Confirm ────────────────────────────────────────────────────────────────────────────────────────╮
  │ Run bash: echo first-run?                                                                        │
  │ y  allow once                                                                                    │
  │ a  always allow bash(echo:*) this session                                                        │
  │ n  deny                                                                                          │
  │ r  deny, saying why                                                                              │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── confirm: y/a/n/r ────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === the second echo ran without asking
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === denied with a reason
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
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
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === Alt+M: auto-edit
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
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
  ── /help ────────────── mode: auto-edit — file edits run; bash, job_input and MCP tools ask first ──
  >
  deepseek-flash  think:on  mode:auto-edit  view:normal                     ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh plan
  === the plan to approve
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ◇ Plan mode on: the model was told to plan, not change files
  ╭─ Plan ───────────────────────────────────────────────────────────────────────────────────────────╮
  │ Approve the plan and start implementing?                                                         │
  │ 1. Write a.txt                                                                                   │
  │ 2. Check it                                                                                      │
  │                                                                                                  │
  │ y  approve: leave plan mode and implement                                                        │
  │ n  keep planning                                                                                 │
  │ r  keep planning, saying what to change                                                          │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── confirm: y/n/r ──────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:plan  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === approved and implemented
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
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
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh question
  === first question
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Storage (1/2) ──────────────────────────────────────────────────────────────────────────────────╮
  │ Which database?                                                                                  │
  │ › ( ) Postgres  what prod uses                                                                   │
  │   ( ) SQLite                                                                                     │
  │   Other:                                                                                         │
  │ ↑↓ choose · type your own · Enter picks · ←→ questions · Esc declines (the model is told)        │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Enter accepts · Esc declines ────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === second question
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Question 2/2 ───────────────────────────────────────────────────────────────────────────────────╮
  │ Anything else?                                                                                   │
  │ › keep it small                                                                                  │
  │ type an answer · Enter answers · ←→ questions · Esc declines (the model is told)                 │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Enter accepts · Esc declines ────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === answered
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ go
    asking
  ✓ ask_user
      Q: Which database?
      A: SQLite
      Q: Anything else?
      A: keep it small
    thanks
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ───────────────────── answered ──
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh paste
  === chip after a 5-line paste
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  > [5 lines pasted]
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === cursor inside expands the chip
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  > line one
    line two
    line three
    line four
    line five
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === submitted as one message
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ line one
  ▌ line two
  ▌ line three
  ▌ line four
  ▌ line five
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh reconnect
  === after the backend was killed
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ before
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─── reconnected to the backend ──
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === prompt works again
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ before
    faux reply
  
  ▌ after
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh fallback
  === chain and default directory set
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
                                                    ╭─ Notice ───────────────────────────────────────╮
                                                    │ default directory: $TMP/cwd  │
                                                    │ (new sessions start there; /default-dir off    │
                                                    │ clears it)                                     │
                                                    ╰──────────────────────────────── Esc dismisses ─╯
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === handed over
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ go
    error: HTTP 429: The usage limit has been reached (usage limit reached)
    deepseek/deepseek-flash: HTTP 429: The usage limit has been reached (usage limit reached); handing
    over to anthropic/claude-fable-5-1
  
  ↪ Handed over from deepseek/deepseek-flash to anthropic/claude-fable-5-1
    carried on
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  claude-fable-5-1  think:on  mode:auto  view:normal                        ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh send_raw
  === the popup highlights a directory
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Directories ────────────────────────────────────────────────────────────────────────────────────╮
  │ ▸ $TMP/cwd/sub/deeper/                                                         │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Tab/Enter accept · Alt+S as typed ───────────────────────────────────────────────────────────────
  > /cd $TMP/cwd/sub/
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === Alt+S: /cd to what was typed
  prigh  new session                                                    $TMP/cwd/sub
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ────────────────── cwd changed ──
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh switch_pending
  === the picker, a confirmation waiting behind it
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Sessions (2) ───────────────────────────────────────────────────────────────────────────────────╮
  │ /                                     Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named │
  │  * ▶ (unnamed) ∣ <time> ∣ 2 msgs ∣ go   $TMP/cwd                     │
  │ ▸  (unnamed) ∣ <time> ∣ 2 msgs ∣ hello  $TMP/cwd                     │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Enter switches · Tab marks · Ctrl+D deletes · Esc closes ────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal  to answer:1              ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === switched: no dialog from the other session
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ hello
    in the first session
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === back: its confirmation again
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Confirm ────────────────────────────────────────────────────────────────────────────────────────╮
  │ Run bash: rm -rf build?                                                                          │
  │ y  allow once                                                                                    │
  │ a  always allow bash(rm:*) this session                                                          │
  │ n  deny                                                                                          │
  │ r  deny, saying why                                                                              │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── confirm: y/a/n/r ────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === answered
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ go
    waiting
  ✓ bash $ sleep 2
    now
  ✓ bash $ rm -rf build
    allowed bash
    all done
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh reconnect_dialog
  === typing the reason
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Why not? (the model sees it) ───────────────────────────────────────────────────────────────────╮
  │ Run bash: make deploy?                                                                           │
  │ › not on a Fri                                                                                   │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Enter denies · Esc goes back ────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === reconnected: the dialog closed
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ go
    asking
  … bash $ make deploy
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─── reconnected to the backend ──
  >
  deepseek-flash  think:on  mode:ask  view:normal                           ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh auto_background
  === moved to the background
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ build it
    building
  ✓ bash $ echo compiling; while [ ! -f gate ]; do sleep 0.0…  job j1 moved to the background after 1s
      compiling
    carrying on meanwhile
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                jobs:1 <spin>  ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === its report
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ build it
    building
  ✓ bash $ echo compiling; while [ ! -f gate ]; do sleep 0.0…  job j1 moved to the background after 1s
      compiling
    carrying on meanwhile
  ↩ job j1 "echo compiling; while [ ! -f gate ]; do sleep 0.05; done; e…"  exited 0
      compiling
      linked
    the build finished
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh connect_signin
  === refused: the sign-in dialog
  prigh  not signed in                                                                         offline
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────╮
  │ user                                                                                             │
  │ token                                                                                            │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Tab next field · Enter signs in · Esc closes ────────────────────────────────────────────────────
  >
  offline
  === the token, masked
  prigh  not signed in                                                                         offline
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────╮
  │ user                                                                                             │
  │ token  ******                                                                                    │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Tab next field · Enter signs in · Esc closes ────────────────────────────────────────────────────
  >
  offline
  === signed in
  prigh  new session                                      $TMP/cwd · 127.0.0.1:<port>
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    tools now run on <host> in $TMP/cwd
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─ connected to 127.0.0.1:<port> ──
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === saved (600)
  ((connect 127.0.0.1:<port>) (token s3cret))
  === signed out
  prigh  not signed in                                                                         offline
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────╮
  │ user                                                                                             │
  │ token                                                                                            │
  │ signed out                                                                                       │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Tab next field · Enter signs in ── signed out of 127.0.0.1:<port>; the saved token is forgotten ──
  >
  offline
  === saved (600)
  ((connect 127.0.0.1:<port>))
  === closed: disconnected
  prigh  not signed in                                                                         offline
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── not signed in to 127.0.0.1:<port> · /signin signs in · /connect switches server ──────────────────
  >
  offline
  === a local backend again
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─ connected to a local backend ──
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  $ bash ./harness.sh startup_signin
  === refused at startup
  prigh  not signed in                                                                         offline
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ╭─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────╮
  │ user                                                                                             │
  │ token                                                                                            │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  ╰──────────────────────────────────────────────────────────────────────────────────────────────────╯
  ── Tab next field · Enter signs in · Esc closes ────────────────────────────────────────────────────
  >
  offline
  === signed in
  prigh  new session                                      $TMP/cwd · 127.0.0.1:<port>
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    tools now run on <host> in $TMP/cwd
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─ connected to 127.0.0.1:<port> ──
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
  === saved (600)
  ((connect 127.0.0.1:<port>) (token s3cret))
  $ bash ./harness.sh detached
  === from inside tmux, relative DIR and -auth-file, odd values
  prigh is running in tmux session prigh-cwd: $TMP/tmux.sh switch-client -t prigh-cwd
  exit 0, at once
  === sessions: other prigh-cwd
  === the TUI in $TMP/cwd
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
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
  prigh  new session                                                        $TMP/cwd
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ hello
    faux reply
  ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal                          ctx ▱▱▱▱▱ 0.0%/1.0M  $0.00
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
