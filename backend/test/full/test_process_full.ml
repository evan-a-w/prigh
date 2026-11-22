open! Core
open! Prigh
open! Test_process

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
