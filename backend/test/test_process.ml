open! Core
open! Prigh

let sh ~env ?stdin ?timeout ?cancel ?cwd script =
  Process.run_collect
    ~env
    ?stdin
    ?timeout
    ?cancel
    ?cwd
    ~prog:"sh"
    ~args:[ "-c"; script ]
    ()
;;

let%expect_test "stdout, stderr and exit code" =
  Eio_main.run
  @@ fun env ->
  print_s [%sexp (sh ~env "echo out; echo err >&2; exit 3" : Process.Output.t)];
  [%expect {| ((exit (Exited 3)) (stdout "out\n") (stderr "err\n")) |}]
;;

let%expect_test "stdin is delivered, including large input" =
  Eio_main.run
  @@ fun env ->
  print_s [%sexp (sh ~env ~stdin:"hello\n" "cat" : Process.Output.t)];
  [%expect {| ((exit (Exited 0)) (stdout "hello\n") (stderr "")) |}];
  let big =
    String.init 1_000_000 ~f:(fun i -> Char.of_int_exn (97 + (i % 26)))
  in
  print_s [%sexp (sh ~env ~stdin:big "wc -c" : Process.Output.t)];
  [%expect {| ((exit (Exited 0)) (stdout "1000000\n") (stderr "")) |}]
;;

let%expect_test "child that closes stdin early does not break us" =
  Eio_main.run
  @@ fun env ->
  let big = String.make 1_000_000 'x' in
  print_s [%sexp (sh ~env ~stdin:big "head -c 3" : Process.Output.t)];
  [%expect {| ((exit (Exited 0)) (stdout xxx) (stderr "")) |}]
;;

let%expect_test "large interleaved output is fully captured" =
  Eio_main.run
  @@ fun env ->
  let out = sh ~env "seq 1 20000; seq 1 20000 >&2" in
  print_s
    [%sexp
      { exit = (out.exit : Process.Exit.t)
      ; stdout_lines = (List.length (String.split_lines out.stdout) : int)
      ; stderr_lines = (List.length (String.split_lines out.stderr) : int)
      }];
  [%expect {| ((exit (Exited 0)) (stdout_lines 20000) (stderr_lines 20000)) |}]
;;

let%expect_test "streaming callbacks see chunks in order" =
  Eio_main.run
  @@ fun env ->
  let chunks = ref [] in
  let exit =
    Process.run
      ~env
      ~prog:"sh"
      ~args:[ "-c"; "echo a; sleep 0.05; echo b; sleep 0.05; echo c" ]
      ~on_stdout:(fun s -> chunks := s :: !chunks)
      ()
  in
  print_s [%sexp (exit : Process.Exit.t), (List.rev !chunks : string list)];
  [%expect {| ((Exited 0) ("a\n" "b\n" "c\n")) |}]
;;

let%expect_test "timeout kills the process and keeps partial output" =
  Eio_main.run
  @@ fun env ->
  let out =
    sh
      ~env
      ~timeout:(Time_ns.Span.of_ms 200.)
      "echo partial; sleep 5; echo never"
  in
  print_s [%sexp (out : Process.Output.t)];
  [%expect {| ((exit Timed_out) (stdout "partial\n") (stderr "")) |}]
;;

let%expect_test "cancellation from another fiber" =
  Eio_main.run
  @@ fun env ->
  let cancel = Cancellation.create () in
  let out = ref None in
  Eio.Fiber.both
    (fun () ->
       out := Some (sh ~env ~cancel "echo started; sleep 5; echo never"))
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
       Cancellation.cancel cancel);
  print_s [%sexp (!out : Process.Output.t option)];
  [%expect {| (((exit Cancelled) (stdout "started\n") (stderr ""))) |}]
;;

let%expect_test "already-cancelled token" =
  Eio_main.run
  @@ fun env ->
  let cancel = Cancellation.create () in
  Cancellation.cancel cancel;
  print_s [%sexp ((sh ~env ~cancel "sleep 5").exit : Process.Exit.t)];
  [%expect {| Cancelled |}]
;;

let%expect_test "cwd, extra env and signal exit" =
  Eio_main.run
  @@ fun env ->
  let dir = Filename_unix.temp_dir "prigh" "" in
  let out = sh ~env ~cwd:dir "pwd" in
  print_s [%sexp (String.equal (String.strip out.stdout) dir : bool)];
  [%expect {| true |}];
  print_s
    [%sexp
      (Process.run_collect
         ~env
         ~extra_env:[ "PRIGH_TEST_VAR", "yes" ]
         ~prog:"sh"
         ~args:
           [ "-c"; "echo $PRIGH_TEST_VAR; test -n \"$HOME\" && echo home-set" ]
         ()
       : Process.Output.t)];
  [%expect
    {|
    ((exit (Exited 0)) (stdout  "yes\
                               \nhome-set\
                               \n") (stderr ""))
    |}];
  print_s [%sexp ((sh ~env "kill -TERM $$").exit : Process.Exit.t)];
  [%expect {| (Signaled sigterm) |}]
;;

let%expect_test "missing program" =
  Eio_main.run
  @@ fun env ->
  let result =
    Or_error.try_with (fun () ->
      Process.run_collect ~env ~prog:"/nonexistent/prog" ~args:[] ())
  in
  print_s [%sexp (Or_error.is_error result : bool)];
  [%expect {| true |}]
;;

let%expect_test "cancellation kills the whole process group" =
  Eio_main.run
  @@ fun env ->
  let cancel = Cancellation.create () in
  let out = ref None in
  let started = Time_ns.now () in
  Eio.Fiber.both
    (fun () ->
       (* [sh -c "a; b"] forks [sleep], which would otherwise outlive the shell
          and hold the output pipe open. *)
       out := Some (sh ~env ~cancel "echo started; sleep 30; echo never"))
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
       Cancellation.cancel cancel);
  let elapsed = Time_ns.diff (Time_ns.now ()) started in
  print_s [%sexp (!out : Process.Output.t option)];
  printf "returned within a second: %b\n" Time_ns.Span.(elapsed < of_int_sec 1);
  [%expect
    {|
    (((exit Cancelled) (stdout "started\n") (stderr "")))
    returned within a second: true
    |}]
;;

(* eio_posix learns that a child exited from its SIGCHLD handler. Under
   OxCaml a signal arriving while the scheduler releases the domain lock
   before [ppoll] used to wait for the next wake-up, which for a lone child
   never came (fixed by nix/opam-repository's iomux patch). A missed
   wake-up shows here as a child not reaped within its timeout. *)
let%expect_test "eio_posix reaps every child promptly" =
  Eio_posix.run
  @@ fun env ->
  let env = (env :> Eio_unix.Stdenv.base) in
  let clock = Eio.Stdenv.clock env in
  let late = ref 0 in
  for _ = 1 to 500 do
    match
      Eio.Time.with_timeout clock 2. (fun () ->
        Ok (Process.run_collect ~env ~prog:"true" ~args:[] ()))
    with
    | Ok { exit = Exited 0; _ } -> ()
    | Ok output -> print_s [%sexp (output : Process.Output.t)]
    | Error `Timeout -> incr late
  done;
  printf "children not reaped within 2s: %d\n" !late;
  [%expect {| children not reaped within 2s: 0 |}]
;;

(* Whether [pid] is still running: gone, or a zombie waiting for init. *)
let running pid =
  match In_channel.read_all (sprintf "/proc/%d/stat" pid) with
  | exception _ -> false
  | stat ->
    (match String.rsplit2 stat ~on:')' with
     | Some (_, rest) -> not (String.is_prefix (String.lstrip rest) ~prefix:"Z")
     | None -> false)
;;

let%expect_test
    "cancelled by Eio (the process exiting), not [cancel]: what the tool \
     started is killed too"
  =
  Eio_main.run
  @@ fun env ->
  let dir = Filename_unix.temp_dir "prigh-process" "" in
  let pidfile = dir ^/ "pid" in
  let started, set_started = Eio.Promise.create () in
  (match
     Eio.Fiber.first
       (fun () ->
          `Finished
            (sh
               ~env
               (sprintf
                  "sleep 1000 & echo $! > %s.tmp; mv %s.tmp %s; wait"
                  pidfile
                  pidfile
                  pidfile)))
       (fun () ->
          let rec wait () =
            if Sys_unix.file_exists_exn pidfile
            then Eio.Promise.resolve set_started ()
            else (
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              wait ())
          in
          wait ();
          `Cancelled)
   with
   | `Cancelled -> print_endline "cancelled once it had started"
   | `Finished (_ : Process.Output.t) -> print_endline "finished?");
  Eio.Promise.await started;
  let grandchild = Int.of_string (String.strip (In_channel.read_all pidfile)) in
  let deadline = Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_sec 10.) in
  let rec gone () =
    if not (running grandchild)
    then true
    else if Time_ns.( > ) (Time_ns.now ()) deadline
    then (
      Signal_unix.send_i Signal.kill (`Pid (Pid.of_int grandchild));
      false)
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      gone ())
  in
  printf "background grandchild gone: %b\n" (gone ());
  [%expect
    {|
    cancelled once it had started
    background grandchild gone: true
    |}]
;;

let%expect_test "process tree: descendants of a pid" =
  let p = Pid.of_int in
  let parents =
    [ p 10, p 1; p 11, p 10; p 12, p 11; p 13, p 10; p 20, p 1; p 21, p 20 ]
  in
  print_s [%sexp (Process_tree.descendants parents ~root:(p 10) : Pid.t list)];
  print_s [%sexp (Process_tree.descendants parents ~root:(p 12) : Pid.t list)];
  [%expect
    {|
    (11 12 13)
    ()
    |}];
  Eio_main.run
  @@ fun env ->
  let parents = Option.value_exn (Process_tree.parents ~env) in
  print_s
    [%sexp
      (List.exists parents ~f:(fun (pid, ppid) ->
         Pid.equal pid (Core_unix.getpid ())
         && Pid.equal ppid (Core_unix.getppid_exn ()))
       : bool)];
  [%expect {| true |}]
;;

let alive pid =
  match Signal_unix.send Signal.zero (`Pid pid) with
  | `Ok -> true
  | `No_such_process -> false
;;

let read_pid dir name =
  Pid.of_string (String.strip (In_channel.read_all (Filename.concat dir name)))
;;

(* Whether [f] holds within 5s. *)
let within_5s ~env f =
  let clock = Eio.Stdenv.clock env in
  let deadline = Eio.Time.now clock +. 5. in
  let rec go () =
    f ()
    || (Float.( < ) (Eio.Time.now clock) deadline
        &&
        (Eio.Time.sleep clock 0.01;
         go ()))
  in
  go ()
;;

(* Cancels [script] once it wrote [pid] in [dir]; prints how it ended and
   whether that took under 2s. *)
let cancel_once_started ~env ~dir script =
  let cancel = Cancellation.create () in
  let clock = Eio.Stdenv.clock env in
  let cancelled_at = ref 0. in
  let out = ref None in
  Eio.Fiber.both
    (fun () -> out := Some (sh ~env ~cwd:dir ~cancel script))
    (fun () ->
       let rec wait () =
         if not (Sys_unix.file_exists_exn (Filename.concat dir "pid"))
         then (
           Eio.Time.sleep clock 0.01;
           wait ())
       in
       wait ();
       cancelled_at := Eio.Time.now clock;
       Cancellation.cancel cancel);
  let under_2s = Float.( < ) (Eio.Time.now clock -. !cancelled_at) 2. in
  print_s
    [%sexp
      (Option.map !out ~f:(fun o -> o.exit) : Process.Exit.t option)
    , ~~(under_2s : bool)]
;;

let%expect_test "cancel kills what left the process group" =
  Eio_main.run
  @@ fun env ->
  let dir = Filename_unix.temp_dir "prigh" "" in
  (* [timeout] leads a group of its own. *)
  cancel_once_started
    ~env
    ~dir
    "timeout 60 sh -c 'echo $$ > pid; exec sleep 30'; touch survived";
  let pid = read_pid dir "pid" in
  let killed = within_5s ~env (fun () -> not (alive pid)) in
  print_s [%sexp ~~(killed : bool)];
  print_s
    [%sexp (Sys_unix.file_exists_exn (Filename.concat dir "survived") : bool)];
  [%expect
    {|
    ((Cancelled) (under_2s true))
    (killed true)
    false
    |}]
;;

let%expect_test "cancel does not wait for a daemon holding the output" =
  Eio_main.run
  @@ fun env ->
  let dir = Filename_unix.temp_dir "prigh" "" in
  (* Reparented to init before the cancel: out of the tree's reach. *)
  cancel_once_started
    ~env
    ~dir
    "(setsid sh -c 'echo $$ > daemon; exec sleep 30' &); until [ -s daemon ]; \
     do sleep 0.01; done; echo > pid; sleep 30";
  let daemon = read_pid dir "daemon" in
  Signal_unix.send_i Signal.kill (`Pid daemon);
  [%expect {| ((Cancelled) (under_2s true)) |}]
;;
