open! Core
open! Prigh

let%expect_test "has_entry: whole entries only" =
  let environ = String.concat ~sep:"\000" [ "A=1"; "X=ab"; "YX=cd"; "Z=ef" ] in
  List.iter
    [ "A=1"; "X=ab"; "Z=ef"; "X=a"; "X=abc"; "X=cd"; "B=1"; "=1" ]
    ~f:(fun entry ->
      printf "%s %b\n" entry (Stray_processes.has_entry environ ~entry));
  printf "empty %b\n" (Stray_processes.has_entry "" ~entry:"A=1");
  printf "alone %b\n" (Stray_processes.has_entry "A=1" ~entry:"A=1");
  printf
    "trailing NUL %b\n"
    (Stray_processes.has_entry "B=2\000A=1\000" ~entry:"A=1");
  [%expect
    {|
    A=1 true
    X=ab true
    Z=ef true
    X=a false
    X=abc false
    X=cd false
    B=1 false
    =1 false
    empty false
    alone true
    trailing NUL true
    |}]
;;

let%expect_test "start_ticks: past a name with spaces and parentheses" =
  let stat name =
    sprintf
      "1234 (%s) S 1 1234 1234 0 -1 4194560 100 0 0 0 1 2 0 0 20 0 1 0 5124443 \
       9000 200 18446744073709551615 0 0 0 0 0 0 0 0 0 0 0 0 17 3 0 0"
      name
  in
  List.iter [ "sleep"; "a b"; "x) (y"; ")" ] ~f:(fun name ->
    print_s
      [%sexp
        (name : string), (Stray_processes.start_ticks (stat name) : int option)]);
  print_s [%sexp (Stray_processes.start_ticks "garbage" : int option)];
  print_s
    [%sexp
      (Option.is_some
         (Stray_processes.start_ticks (In_channel.read_all "/proc/self/stat"))
       : bool)];
  [%expect
    {|
    (sleep (5124443))
    ("a b" (5124443))
    ("x) (y" (5124443))
    (")" (5124443))
    ()
    true
    |}]
;;

(* Started with [variable=value] in its environment, as a bash call's
   descendants are. *)
let spawn ~value =
  let pid =
    Core_unix.create_process_env
      ~prog:"sleep"
      ~args:[ "303" ]
      ~env:(`Extend [ Stray_processes.variable, value ])
      ()
  in
  pid.pid
;;

let%expect_test "find: the processes started with this call's marker" =
  let marker = Stray_processes.Marker.create () in
  let other = Stray_processes.Marker.create () in
  let mine = spawn ~value:(Stray_processes.Marker.value marker) in
  let theirs = spawn ~value:(Stray_processes.Marker.value other) in
  let longer = spawn ~value:(Stray_processes.Marker.value marker ^ "0") in
  let rec until_exec tries =
    let started pid =
      match
        In_channel.read_all (sprintf "/proc/%d/cmdline" (Pid.to_int pid))
      with
      | cmdline -> String.is_prefix cmdline ~prefix:"sleep"
      | exception _ -> false
    in
    if tries > 0 && not (List.for_all [ mine; theirs; longer ] ~f:started)
    then (
      Core_unix.nanosleep 0.01 |> (ignore : float -> unit);
      until_exec (tries - 1))
  in
  until_exec 500;
  let found = Stray_processes.find marker in
  print_s
    [%sexp
      (List.map found ~f:(fun { pid; command } ->
         (if Pid.equal pid mine then "mine" else "another"), command)
       : (string * string) list)];
  List.iter [ mine; theirs; longer ] ~f:(fun pid ->
    Signal_unix.send_i Signal.kill (`Pid pid);
    ignore (Core_unix.waitpid pid : Core_unix.Exit_or_signal.t));
  printf "after: %d\n" (List.length (Stray_processes.find marker));
  [%expect
    {|
    ((mine "sleep 303"))
    after: 0
    |}]
;;
