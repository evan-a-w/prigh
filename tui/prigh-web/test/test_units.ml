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

let%expect_test "message times in the reader's zone, relative to today" =
  let utc s = Option.value_exn (Rel_time.parse s) in
  (* Monday 5 October 2026, 10:00 in a zone two hours ahead of UTC (Paris in
     summer) and in one ten hours behind (Hawaii). *)
  let paris =
    Message_time.create
      ~now:(utc "2026-10-05 08:00:00Z")
      ~utc_offset:(Time_ns.Span.of_hr 2.)
  in
  let hawaii =
    Message_time.create
      ~now:(utc "2026-10-05 20:00:00Z")
      ~utc_offset:(Time_ns.Span.of_hr (-10.))
  in
  let show t s =
    let time = utc s in
    printf
      "%s: %-22s %-15s %s\n"
      s
      (Message_time.short t time)
      (Message_time.day_label t (Message_time.date t time))
      (Message_time.full t time)
  in
  List.iter
    ~f:(show paris)
    [ "2026-10-05 07:59:59Z" (* today, 09:59 *)
    ; "2026-10-04 22:00:00Z" (* today, 00:00 local *)
    ; "2026-10-04 21:59:00Z" (* yesterday, 23:59 local *)
    ; "2026-10-03 22:30:00Z" (* yesterday, 00:30 local *)
    ; "2026-10-03 12:05:00Z" (* Saturday *)
    ; "2026-01-01 00:00:00Z"
    ; "2025-12-31 21:59:00Z" (* 23:59 local: last year *)
    ; "2026-10-06 09:00:00Z" (* tomorrow: a clock ahead of ours *)
    ];
  [%expect
    {|
    2026-10-05 07:59:59Z: 09:59                  Today           Monday 5 October 2026, 09:59:59
    2026-10-04 22:00:00Z: 00:00                  Today           Monday 5 October 2026, 00:00:00
    2026-10-04 21:59:00Z: Yesterday 23:59        Yesterday       Sunday 4 October 2026, 23:59:00
    2026-10-03 22:30:00Z: Yesterday 00:30        Yesterday       Sunday 4 October 2026, 00:30:00
    2026-10-03 12:05:00Z: 3 Oct 14:05            Sat 3 Oct       Saturday 3 October 2026, 14:05:00
    2026-01-01 00:00:00Z: 1 Jan 02:00            Thu 1 Jan       Thursday 1 January 2026, 02:00:00
    2025-12-31 21:59:00Z: 31 Dec 2025 23:59      Wed 31 Dec 2025 Wednesday 31 December 2025, 23:59:00
    2026-10-06 09:00:00Z: 6 Oct 11:00            Tue 6 Oct       Tuesday 6 October 2026, 11:00:00
    |}];
  List.iter
    ~f:(show hawaii)
    [ "2026-10-05 20:00:00Z"; "2026-10-05 09:59:00Z"; "2026-10-05 10:00:00Z" ];
  [%expect
    {|
    2026-10-05 20:00:00Z: 10:00                  Today           Monday 5 October 2026, 10:00:00
    2026-10-05 09:59:00Z: Yesterday 23:59        Yesterday       Sunday 4 October 2026, 23:59:00
    2026-10-05 10:00:00Z: 00:00                  Today           Monday 5 October 2026, 00:00:00
    |}];
  print_s [%sexp (paris : Message_time.t)];
  [%expect {| ((today 2026-10-05) (utc_offset 2h)) |}];
  print_s
    [%sexp
      (Message_time.equal
         paris
         (Message_time.create
            ~now:(utc "2026-10-05 21:59:59Z")
            ~utc_offset:(Time_ns.Span.of_hr 2.))
       : bool)
    , (Message_time.equal
         paris
         (Message_time.create
            ~now:(utc "2026-10-05 22:00:00Z")
            ~utc_offset:(Time_ns.Span.of_hr 2.))
       : bool)];
  [%expect {| (true false) |}]
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

let%expect_test "browser carets (UTF-16) as byte offsets" =
  let text = "日本 é😀x" in
  List.iter [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 20 ] ~f:(fun utf16 ->
    let byte = Utf16.byte_offset text ~utf16 in
    printf "%d -> %d |%s|\n" utf16 byte (String.prefix text byte));
  [%expect
    {|
    0 -> 0 ||
    1 -> 3 |日|
    2 -> 6 |日本|
    3 -> 7 |日本 |
    4 -> 9 |日本 é|
    5 -> 13 |日本 é😀|
    6 -> 13 |日本 é😀|
    7 -> 14 |日本 é😀x|
    8 -> 14 |日本 é😀x|
    20 -> 14 |日本 é😀x|
    |}]
;;
