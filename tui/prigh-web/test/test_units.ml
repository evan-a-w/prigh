open! Core
open Prigh_web

let%expect_test "relative ages" =
  let now = Option.value_exn (Rel_time.parse "2026-10-05 12:00:00.000000Z") in
  List.iter
    [ "2026-10-05 11:59:30.5Z"
    ; "2026-10-05 11:59:00Z"
    ; "2026-10-05 11:01:00Z"
    ; "2026-10-05 09:00:00Z"
    ; "2026-10-03 12:00:00Z"
    ; "2026-08-01 12:00:00Z"
    ; "2024-10-01 12:00:00Z"
    ; "2026-10-05 12:00:30Z"
    ]
    ~f:(fun s ->
      match Rel_time.parse s with
      | Some time -> printf "%s: %s\n" s (Rel_time.ago ~now time)
      | None -> printf "%s: unparsed\n" s);
  print_s [%sexp (Rel_time.parse "yesterday" : Time_ns.Alternate_sexp.t option)];
  [%expect
    {|
    2026-10-05 11:59:30.5Z: just now
    2026-10-05 11:59:00Z: 1m ago
    2026-10-05 11:01:00Z: 59m ago
    2026-10-05 09:00:00Z: 3h ago
    2026-10-03 12:00:00Z: 2d ago
    2026-08-01 12:00:00Z: 2mo ago
    2024-10-01 12:00:00Z: 2y ago
    2026-10-05 12:00:30Z: just now
    ()
    |}]
;;

let%expect_test "what counts as a slash command" =
  List.iter
    [ "/model sonnet"
    ; "  /help  "
    ; "/name  My   session "
    ; "/etc/hosts is broken"
    ; "/model\nsecond line"
    ; "not /a command"
    ; "/"
    ]
    ~f:(fun text ->
      print_s
        [%sexp (text : string), (Slash.parse text : Slash.Parsed.t option)]);
  [%expect
    {|
    ("/model sonnet" (((name model) (rest sonnet))))
    ("  /help  " (((name help) (rest ""))))
    ("/name  My   session " (((name name) (rest "My   session"))))
    ("/etc/hosts is broken" ())
    ( "/model\
     \nsecond line" ())
    ("not /a command" ())
    (/ (((name "") (rest ""))))
    |}];
  List.iter [ "hlep"; "mdoel"; "sess"; "zzz"; "" ] ~f:(fun name ->
    printf
      "%S -> %s\n"
      name
      (Option.value_map (Slash.closest name) ~default:"none" ~f:(fun s ->
         s.name)));
  [%expect
    {|
    "hlep" -> help
    "mdoel" -> model
    "sess" -> session
    "zzz" -> none
    "" -> none
    |}]
;;

let%expect_test "history keeps 100 entries, no consecutive duplicates" =
  let h =
    List.fold
      (List.init 105 ~f:Int.to_string)
      ~init:History.empty
      ~f:History.add
  in
  let h = History.add h "104" in
  let entries = History.to_list h in
  print_s
    [%sexp (List.length entries : int), (List.take entries 3 : string list)];
  [%expect {| (100 (104 103 102)) |}]
;;
