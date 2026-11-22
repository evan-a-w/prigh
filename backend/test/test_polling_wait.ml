open! Core
open! Prigh

let check ?timeout command =
  printf
    "%s\n  -> %s\n"
    command
    (match Polling_wait.check ?timeout command with
     | None -> "runs"
     | Some refusal -> refusal)
;;

let%expect_test "the report's wait: pgrep -f that matches its own shell, in a sleep loop" =
  check
    {|timeout 7000 bash -c 'while pgrep -f "nix build -L --keep-going .#devShells" >/dev/null; do sleep 15; done'; grep -a "^error" /tmp/oxbuild.log|};
  [%expect
    {|
    timeout 7000 bash -c 'while pgrep -f "nix build -L --keep-going .#devShells" >/dev/null; do sleep 15; done'; grep -a "^error" /tmp/oxbuild.log
      -> Not run: pgrep -f 'nix build -L --keep-going .#devShells' matches this command's own command line (the shell running it is bash -c with this text, which contains the pattern), so it always finds a process, even when nothing else matches. Bracket a character so the pattern cannot match itself: pgrep -f '[n]ix build -L --keep-going .#devShells'. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null.
    |}];
  (* With the pattern bracketed, the loop is still refused: it is unbounded. *)
  check
    {|timeout 7000 bash -c 'while pgrep -f "[n]ix build" >/dev/null; do sleep 15; done'|};
  [%expect
    {|
    timeout 7000 bash -c 'while pgrep -f "[n]ix build" >/dev/null; do sleep 15; done'
      -> Not run: this command waits by polling (a while loop that sleeps), which blocks you for as long as it waits and spins until its timeout if what it waits for never comes. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null. To wait for a server to come up, bound the loop with a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; done' (loops under a timeout of at most 120s run).
    |}]
;;

let%expect_test "pgrep and pkill" =
  List.iter
    ~f:(fun c -> check c)
    [ "pkill -f my-server"
    ; "pgrep -af 'node server.js'"
    ; "pgrep -u lloyd -f 'python app'"
      (* Without -f only process names are matched, never the shell. *)
    ; "pgrep nix"
    ; "pkill -9 my-server"
    ; "pgrep -f '[n]ix build'"
      (* A regex that cannot match its own text. *)
    ; "pgrep -f 'nix (build|develop)'"
    ];
  [%expect
    {|
    pkill -f my-server
      -> Not run: pkill -f 'my-server' matches this command's own command line (the shell running it is bash -c with this text, which contains the pattern), so it would kill this very shell. Bracket a character so the pattern cannot match itself: pkill -f '[m]y-server'. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null.
    pgrep -af 'node server.js'
      -> Not run: pgrep -f 'node server.js' matches this command's own command line (the shell running it is bash -c with this text, which contains the pattern), so it always finds a process, even when nothing else matches. Bracket a character so the pattern cannot match itself: pgrep -f '[n]ode server.js'. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null.
    pgrep -u lloyd -f 'python app'
      -> Not run: pgrep -f 'python app' matches this command's own command line (the shell running it is bash -c with this text, which contains the pattern), so it always finds a process, even when nothing else matches. Bracket a character so the pattern cannot match itself: pgrep -f '[p]ython app'. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null.
    pgrep nix
      -> runs
    pkill -9 my-server
      -> runs
    pgrep -f '[n]ix build'
      -> runs
    pgrep -f 'nix (build|develop)'
      -> runs
    |}]
;;

let%expect_test "loops" =
  List.iter
    ~f:(fun c -> check c)
    [ "while true; do date; sleep 60; done"
    ; "until [ -f out.txt ]; do sleep 5; done"
    ; "while sleep 10; do ls; done"
    ; "while ! curl -sf localhost:8080; do sleep $DELAY; done"
    ; "for i in $(seq 1 600); do sleep 1; done"
    ; "for i in {1..200}; do curl -s x; sleep 1; done"
    ];
  [%expect
    {|
    while true; do date; sleep 60; done
      -> Not run: this command waits by polling (a while loop that sleeps), which blocks you for as long as it waits and spins until its timeout if what it waits for never comes. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null. To wait for a server to come up, bound the loop with a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; done' (loops under a timeout of at most 120s run).
    until [ -f out.txt ]; do sleep 5; done
      -> Not run: this command waits by polling (an until loop that sleeps), which blocks you for as long as it waits and spins until its timeout if what it waits for never comes. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null. To wait for a server to come up, bound the loop with a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; done' (loops under a timeout of at most 120s run).
    while sleep 10; do ls; done
      -> Not run: this command waits by polling (a while loop that sleeps), which blocks you for as long as it waits and spins until its timeout if what it waits for never comes. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null. To wait for a server to come up, bound the loop with a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; done' (loops under a timeout of at most 120s run).
    while ! curl -sf localhost:8080; do sleep $DELAY; done
      -> Not run: this command waits by polling (a while loop that sleeps), which blocks you for as long as it waits and spins until its timeout if what it waits for never comes. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null. To wait for a server to come up, bound the loop with a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; done' (loops under a timeout of at most 120s run).
    for i in $(seq 1 600); do sleep 1; done
      -> Not run: this command waits by polling (a for loop that sleeps 600s in all), which blocks you for as long as it waits and spins until its timeout if what it waits for never comes. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null. To wait for a server to come up, bound the loop with a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; done' (loops under a timeout of at most 120s run).
    for i in {1..200}; do curl -s x; sleep 1; done
      -> Not run: this command waits by polling (a for loop that sleeps 200s in all), which blocks you for as long as it waits and spins until its timeout if what it waits for never comes. To wait for a command that takes long (a build, tests, an install), start it with bash and background: true, then keep working or end your turn: its report ([job ID exited N] and its last lines) arrives when it exits. For a process already running that is not a job, wait on its pid: timeout 600 tail --pid=PID -f /dev/null. To wait for a server to come up, bound the loop with a short timeout: timeout 60 bash -c 'until curl -sf URL; do sleep 1; done' (loops under a timeout of at most 120s run).
    |}]
;;

let%expect_test "bounded or not polling: runs" =
  List.iter
    ~f:(fun (timeout, c) -> check ?timeout c)
    [ None, "timeout 60 bash -c 'until curl -sf localhost:8080; do sleep 1; done'"
    ; None, "timeout 2m bash -c 'while ! test -f out; do sleep 2; done'"
    ; Some 90, "until curl -sf localhost:8080; do sleep 1; done"
    ; None, "while read -r line; do echo \"$line\"; done < file"
    ; None, "while read -r f; do process $f; sleep 0.1; done < list"
    ; None, "for i in 1 2 3; do curl -s x; sleep 2; done"
    ; None, "for f in *.log; do gzip $f; sleep 5; done"
    ; None, "for i in $(seq 10); do sleep 1; done"
    ; None, "sleep 5 && echo done"
    ; None, "echo 'while waiting, do not sleep' > notes.txt"
    ];
  [%expect
    {|
    timeout 60 bash -c 'until curl -sf localhost:8080; do sleep 1; done'
      -> runs
    timeout 2m bash -c 'while ! test -f out; do sleep 2; done'
      -> runs
    until curl -sf localhost:8080; do sleep 1; done
      -> runs
    while read -r line; do echo "$line"; done < file
      -> runs
    while read -r f; do process $f; sleep 0.1; done < list
      -> runs
    for i in 1 2 3; do curl -s x; sleep 2; done
      -> runs
    for f in *.log; do gzip $f; sleep 5; done
      -> runs
    for i in $(seq 10); do sleep 1; done
      -> runs
    sleep 5 && echo done
      -> runs
    echo 'while waiting, do not sleep' > notes.txt
      -> runs
    |}]
;;
