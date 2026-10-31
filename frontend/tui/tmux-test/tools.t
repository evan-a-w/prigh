Tool calls as they run, and moving to the background.
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

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
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
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
  ──────────────────────────────────────────────────────────── view: verbose — everything is shown ───
  >
  deepseek-flash  think:on  mode:auto  view:verbose  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
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
  ────────────────────────────────────── view: quiet — intermediate output and thinking are hidden ───
  >
  deepseek-flash  think:on  mode:auto  view:quiet  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
  === agent 1
  ◆ subagent 1/2  deepseek-flash  ✓ done 1 turns $0.00  "Count files"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ count files
    child reporting: done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >  a1 has finished · Esc goes back to the main agent
  deepseek-flash  think:on  ctx:0.0%/1.0M  agents:main [1✓] 2✓  → a1 (finished) · Esc: main agent
  === agent 2
  ◆ subagent 2/2  deepseek-flash  ✓ done 1 turns $0.00  "say hello"
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ▌ say hello
    child reporting: done
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >  a2 has finished · Esc goes back to the main agent
  deepseek-flash  think:on  ctx:0.0%/1.0M  agents:main 1✓ [2✓]  → a2 (finished) · Esc: main agent
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
  deepseek-flash  think:on  mode:auto  view:quiet  ctx:0.0%/1.0M  $0.00  agents:[main] 1✓ 2✓
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
  $ bash ./harness.sh send_raw
  === the popup highlights a directory
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ▸ $TMP/cwd/sub/deeper/
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > /cd $TMP/cwd/sub/
  deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00  Tab/Enter accept · Alt+S as typed
  === Alt+S: /cd to what was typed
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ──────────────────────────────────────────────────────────────────────────────────── cwd changed ───
  >
  $TMP/cwd/sub  deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00
