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
