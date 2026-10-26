open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* /nix-gc: the Nix store for superusers. *)

let connected ?(superuser = true) () =
  let h = Test_app.connected ~width:80 ~height:16 () in
  H.step
    ~quiet:true
    h
    (Hello { client_id = "c1"; namespace = None; user = None; superuser });
  H.event h (State (state ()));
  h
;;

let run h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let status_json =
  {|{"store_bytes":12884901888,"store_paths":4321,"fs_total_bytes":107374182400,
     "fs_free_bytes":42949672960,
     "last_gc":{"at_ms":1761400000000,"by":"schedule","generations":14,
                "generations_deleted":3,"paths_deleted":120,
                "freed_bytes":1288490188,"duration_ms":42000},
     "running":null,
     "schedule":{"interval_seconds":86400,"keep_days":14,
                 "next_at_ms":1761486400000}}|}
;;

let%expect_test "/nix-gc shows the store, the last collection and the schedule" =
  let h = connected () in
  run h "/nix-gc";
  H.reply h Nix_store_shown status_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ nix_store_status) (params ()) (tag Nix_store_shown))

    ▌ earlier question
      earlier answer
    ┌─ Nix store ──────────────────────────────────────────────────────────────────┐
    │ store: 12.0 GiB in 4321 paths                                                │
    │ /nix: 40.0 GiB free of 100.0 GiB (40%)                                       │
    │ last collection: 2025-10-25 13:46 by the automatic collection, freed 1.2     │
    │ GiB: 120 paths, 3 generations deleted in 42 s                                │
    │ automatic: every 1 day, deleting profile generations older than 14 days;     │
    │ next at 2025-10-26 13:46                                                     │
    │ /nix-gc run collects garbage; run DAYS (or all) first deletes older profile  │
    │ generations                                                                  │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ───────────────────────────────────────────────────── reading the Nix store… ───
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

let%expect_test "/nix-gc run: progress in the status line, then what was freed" =
  let h = connected () in
  run h "/nix-gc run";
  H.event
    h
    (Nix_gc_progress
       { paths_deleted = 12; generations_deleted = 0; message = None });
  H.show h;
  H.reply
    h
    Nix_gc_done
    {|{"at_ms":1761400000000,"by":"alice","generations":null,
       "generations_deleted":0,"paths_deleted":40,"freed_bytes":524288000,
       "duration_ms":5000}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ nix_store_gc) (params ()) (tag Nix_gc_done))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────── collecting Nix garbage… ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  nix-gc: 12 paths deleted









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────── Nix garbage collected: freed 500.0 MiB: 40 paths deleted in 5 s ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/nix-gc run 14 and run all ask first; n cancels" =
  let h = connected () in
  run h "/nix-gc run 14";
  H.show h;
  H.key h (Key.char 'y');
  run h "/nix-gc run all";
  H.key h (Key.char 'n');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────┐
    │ Collect Nix garbage, first deleting profile generations older than 14 days   │
    │ of every user? They can no longer roll back to them (current ones are kept). │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  confirm: y / n
    (Rpc
      (method_ nix_store_gc)
      (params ((delete_generations 14)))
      (tag Nix_gc_done))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > n▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  nix-gc: 0 paths deleted
    |}]
;;

let%expect_test "one collection at a time; progress for others' is ignored" =
  let h = connected () in
  H.event
    h
    (Nix_gc_progress
       { paths_deleted = 1; generations_deleted = 0; message = None });
  print_s [%sexp (h.model.nix_gc : P.Nix_gc_progress.t option)];
  run h "/nix-gc run";
  run h "/nix-gc run";
  run h "/nix-gc run all";
  H.show h;
  [%expect
    {|
    ()
    (Rpc (method_ nix_store_gc) (params ()) (tag Nix_gc_done))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Warning ────────────────────────────────────────────────────────────────────┐
    │ the Nix garbage collection you started is still running (see the status      │
    │ line)                                                                        │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  nix-gc: 0 paths deleted
    |}]
;;

let%expect_test "failing: the backend says why" =
  let h = connected () in
  run h "/nix-gc run";
  H.reply_error h Nix_gc_done "nix store gc failed: disk on fire";
  print_s [%sexp (h.model.nix_gc : P.Nix_gc_progress.t option)];
  H.show h;
  [%expect
    {|
    (Rpc (method_ nix_store_gc) (params ()) (tag Nix_gc_done))
    ()









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────── nix store gc failed: disk on fire ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "to everyone else /nix-gc does not exist" =
  let h = connected ~superuser:false () in
  H.keys h "/nix";
  print_s
    [%message
      "completing /nix"
        ~offered:
          (Option.map h.model.autocomplete ~f:(fun ac ->
             List.map (Autocomplete.items ac) ~f:(fun i -> i.id))
           : string list option)];
  let h = connected ~superuser:false () in
  run h "/nix-gc";
  H.show h;
  let h = connected ~superuser:false () in
  run h "/help nix-gc";
  H.show h;
  print_s
    [%message
      "/help"
        ~mentions_nix:
          (String.is_substring
             (Content.to_plain (Commands.help ~superuser:false))
             ~substring:"nix"
           : bool)];
  [%expect
    {|
    ("completing /nix" (offered ()))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────── unknown command /nix-gc (back in the editor); Tab or / lists commands ───
    > /nix-gc▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────── unknown command /nix-gc (back in the editor); Tab or / lists commands ───
    > /help nix-gc▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (/help (mentions_nix false))
    |}]
;;

let%expect_test "a superuser acting as another user keeps /nix-gc" =
  let h = connected () in
  H.step
    ~quiet:true
    h
    (Hello
       { client_id = "c1"
       ; namespace = Some "bob"
       ; user = Some "alice"
       ; superuser = true
       });
  run h "/nix-gc";
  [%expect
    {| (Rpc (method_ nix_store_status) (params ()) (tag Nix_store_shown)) |}]
;;

let%expect_test "mistakes show the usage, the command kept to fix" =
  List.iter
    [ "/nix-gc run soon"; "/nix-gc run -3"; "/nix-gc now" ]
    ~f:(fun text ->
      let h = connected () in
      run h text;
      H.show h);
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ "soon" is not a number of days: /nix-gc [run [DAYS|all]] (no argument: show  │
    │ the store; run: collect garbage; run DAYS: first delete profile generations  │
    │ older than DAYS days; run all: all but the current ones)                     │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /nix-gc run soon▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ -3 days is out of range (0 to 36500): /nix-gc [run [DAYS|all]] (no argument: │
    │ show the store; run: collect garbage; run DAYS: first delete profile         │
    │ generations older than DAYS days; run all: all but the current ones)         │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /nix-gc run -3▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ usage: /nix-gc [run [DAYS|all]] (no argument: show the store; run: collect   │
    │ garbage; run DAYS: first delete profile generations older than DAYS days;    │
    │ run all: all but the current ones)                                           │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /nix-gc now▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "parsing, sizes and spans" =
  List.iter
    [ ""
    ; "run"
    ; "run 14"
    ; "run 14d"
    ; "run 0"
    ; "run all"
    ; "run 99999"
    ; "run x"
    ; "x"
    ]
    ~f:(fun rest ->
      print_s
        [%message rest ~_:(Nix_gc_command.parse rest : Nix_gc_command.Action.t)]);
  List.iter
    [ 0
    ; 1023
    ; 1024
    ; 1536
    ; 1048576
    ; 1073741824
    ; 5 * 1099511627776
    ; 2000 * 1099511627776
    ]
    ~f:(fun n -> print_endline (Nix_gc_command.bytes (Int64.of_int n)));
  List.iter [ 0; 59; 60; 3599; 3600; 86399; 86400; 172800 ] ~f:(fun s ->
    print_endline (Nix_gc_command.span (Time_ns.Span.of_int_sec s)));
  [%expect
    {|
    ("" Status)
    (run (Collect ()))
    ("run 14" (Collect ((Older_than_days 14))))
    ("run 14d" (Collect ((Older_than_days 14))))
    ("run 0" (Collect (All)))
    ("run all" (Collect (All)))
    ("run 99999" (
      Error
      "99999 days is out of range (0 to 36500): /nix-gc [run [DAYS|all]] (no argument: show the store; run: collect garbage; run DAYS: first delete profile generations older than DAYS days; run all: all but the current ones)"))
    ("run x" (
      Error
      "\"x\" is not a number of days: /nix-gc [run [DAYS|all]] (no argument: show the store; run: collect garbage; run DAYS: first delete profile generations older than DAYS days; run all: all but the current ones)"))
    (x (
      Error
      "usage: /nix-gc [run [DAYS|all]] (no argument: show the store; run: collect garbage; run DAYS: first delete profile generations older than DAYS days; run all: all but the current ones)"))
    0 B
    1023 B
    1.0 KiB
    1.5 KiB
    1.0 MiB
    1.0 GiB
    5.0 TiB
    2000.0 TiB
    0 s
    59 s
    1 min
    59 min
    1 h
    23 h
    1 day
    2 days
    |}]
;;

let%expect_test "decoding: nulls, the schedule off, superusers in hello" =
  let decode of_json sexp_of json =
    print_s
      (Or_error.sexp_of_t sexp_of (Or_error.bind (P.Json.parse json) ~f:of_json))
  in
  (* Times print in the local zone: [running] without its start. *)
  decode
    (fun json ->
      Or_error.map (P.Nix_store_status.of_json json) ~f:(fun s ->
        ( { s with running = None }
        , Option.map s.running ~f:(fun r -> r.by, r.generations) )))
    [%sexp_of:
      P.Nix_store_status.t * (string * P.Nix_generations.t option) option]
    {|{"store_bytes":5000000000,"store_paths":2,"fs_total_bytes":9,"fs_free_bytes":0,
       "last_gc":null,"running":{"by":"schedule","since_ms":1000,"generations":"all"},
       "schedule":{"interval_seconds":0,"keep_days":null,"next_at_ms":null}}|};
  decode
    P.Nix_store_status.of_json
    P.Nix_store_status.sexp_of_t
    {|{"store_bytes":1,"store_paths":1,"fs_total_bytes":1,"fs_free_bytes":1}|};
  decode
    P.Hello_reply.of_json
    P.Hello_reply.sexp_of_t
    {|{"client_id":"c1","namespace":"alice","user":"alice","superuser":true}|};
  [%expect {|
    (Ok (
      ((store_bytes    5000000000)
       (store_paths    2)
       (fs_total_bytes 9)
       (fs_free_bytes  0)
       (last_gc ())
       (running ())
       (schedule ((
         (interval 0s)
         (keep_days ())
         (next_at   ())))))
      ((schedule (All)))))
    (Ok (
      (store_bytes    1)
      (store_paths    1)
      (fs_total_bytes 1)
      (fs_free_bytes  1)
      (last_gc  ())
      (running  ())
      (schedule ())))
    (Ok (
      (client_id c1)
      (namespace (alice))
      (user      (alice))
      (superuser true)))
    |}]
;;
