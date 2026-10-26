Signing in to a backend elsewhere, and sessions left running.
Each command runs one scenario from harness.sh in a fresh tmux session and
prints the captured panes.

  $ bash ./harness.sh connect_signin
  === refused: the sign-in dialog
  ┌─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token                                                                                            │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  Tab next field · Enter signs in · Esc closes
  === the token, masked
  ┌─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token  ******                                                                                    │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  Tab next field · Enter signs in · Esc closes
  === signed in
    tools now run on <host> in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ─────────────────────────────────────────────────────────────────── connected to 127.0.0.1:<port> ───
  >
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  server:127.0.0.1:<port>
  === saved (600)
  ((connect 127.0.0.1:<port>) (token s3cret))
  === signed out
  ┌─ Sign in to 127.0.0.1:<port> ─────────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token                                                                                            │
  │ signed out                                                                                       │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ──────────────────────────────────── signed out of 127.0.0.1:<port>; the saved token is forgotten ───
  >
  Tab next field · Enter signs in · Esc closes
  === saved (600)
  ((connect 127.0.0.1:<port>))
  === closed: disconnected
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  not signed in to 127.0.0.1:<port> · /signin signs in · /connect switches server
  === a local backend again
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ─────────────────────────────────────────────────────────────────── connected to a local backend ───
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
  >
  Tab next field · Enter signs in · Esc closes
  === signed in
    tools now run on <host> in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ─────────────────────────────────────────────────────────────────── connected to 127.0.0.1:<port> ───
  >
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  server:127.0.0.1:<port>
  === saved (600)
  ((connect 127.0.0.1:<port>) (token s3cret))
  $ bash ./harness.sh connect_websocket
  === refused at startup: the sign-in dialog names the URL
  ┌─ Sign in to ws://127.0.0.1:<port> ────────────────────────────────────────────────────────────────┐
  │ user                                                                                             │
  │ token                                                                                            │
  │ unauthorised: bad user name or password                                                          │
  │ prigh serve -tokens: your user name and password; -token: no user                                │
  └──────────────────────────────────────────────────────────────────────────────────────────────────┘
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  Tab next field · Enter signs in · Esc closes
  === signed in: server:ws://127.0.0.1:<port>
    tools now run on <host> in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  ────────────────────────────────────────────────────────────── connected to ws://127.0.0.1:<port> ───
  >
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  server:ws://127.0.0.1:<port>
  === saved (600)
  ((connect ws://127.0.0.1:<port>/ws) (token s3cret))
  === a turn over the WebSocket
    tools now run on <host> in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ hello
    faux reply
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  >
  deepseek-flash  think:on  mode:auto  view:normal  ctx:0.0%/1.0M  $0.00  server:ws://127.0.0.1:<port>
  === /connect completes the URL
    tools now run on <host> in $TMP/cwd
    prigh in $TMP/cwd · /help · Esc aborts · Ctrl+C twice quits
  
  ▌ hello
    faux reply
  ▸ ws://127.0.0.1:<port>/ws  the current server
    local                    a backend started here, like prigh-tui -local
  ────────────────────────────────────────────────────────────────────────────────────────────────────
  > /connect
  deepseek-flash  think:on  mode:auto  ctx:0.0%/1.0M  $0.00  Tab accepts · Enter as typed · Esc hides
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
