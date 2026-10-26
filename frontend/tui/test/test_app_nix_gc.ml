open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* /nix-gc: the Nix store for superusers. *)

let connected () =
  let h = Test_app.connected ~width:80 ~height:16 () in
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

let%expect_test "refused or failing: the backend says what to do" =
  let h = connected () in
  run h "/nix-gc run";
  H.reply_error
    h
    Nix_gc_done
    "cleaning the Nix store is for superusers: ask alice to run /nix-gc";
  print_s [%sexp (h.model.nix_gc : P.Nix_gc_progress.t option)];
  H.show h;
  run h "/nix-gc";
  H.reply_error
    h
    Nix_store_shown
    "cleaning the Nix store is for superusers: ask alice to run /nix-gc";
  H.show h;
  [%expect
    {|
    (Rpc (method_ nix_store_gc) (params ()) (tag Nix_gc_done))
    ()









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────── cleaning the Nix store is for superusers: ask alice to run /nix-gc ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ nix_store_status) (params ()) (tag Nix_store_shown))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────── cleaning the Nix store is for superusers: ask alice to run /nix-gc ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
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
    ~f:(fun n -> print_endline (Nix_gc_command.bytes n));
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
