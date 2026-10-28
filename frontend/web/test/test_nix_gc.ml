open! Core
module H = Harness

(* The Nix store, for superusers: in Settings and /nix-gc. *)

let superuser () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"
         ; namespace = Some "alice"
         ; user = Some "alice"
         ; superuser = true
         }));
  h
;;

let status_json =
  {|{"store_bytes":12884901888,"store_paths":4321,"fs_total_bytes":107374182400,
     "fs_free_bytes":42949672960,
     "last_gc":{"at_ms":1759600000000,"by":"schedule","generations":14,
                "generations_deleted":3,"paths_deleted":120,
                "freed_bytes":1288490188,"duration_ms":42000},
     "running":null,
     "schedule":{"interval_seconds":86400,"keep_days":14,
                 "next_at_ms":1759686400000}}|}
;;

let section h = H.text h ~selector:".settings-section:nth-last-child(2)"

let report_json =
  {|{"at_ms":1759660000000,"by":"alice","generations":null,
     "generations_deleted":0,"paths_deleted":40,"freed_bytes":524288000,
     "duration_ms":5000}|}
;;

let%expect_test "Settings shows superusers the store; others do not see it" =
  let h = superuser () in
  H.act h Open_settings;
  section h;
  H.reply h "nix_store_status" status_json;
  section h;
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ nix_store_status) (params ())
     (tag (Nix_status (open_settings false))))
    Nix store
    Disk usage The packages agents fetch with Nix, on the shared /nix volume. Reading the store…
    Collect garbage Delete the store paths no profile or GC root uses. Deleting users' old profile generations first frees more, but they can no longer roll back to them.
    (Also delete generations over 14 days…) (Collect)
    Nix store
    Disk usage Store: 12.0 GiB in 4321 paths; /nix: 40.0 GiB free of 100.0 GiB (40%). Last collection: 2025-10-04 17:46 by the automatic collection, freed 1.2 GiB: 120 paths, 3 generations deleted in 42 s Automatic: every 1 day, deleting profile generations older than 14 days; next at 2025-10-05 17:46
    (Refresh)
    Collect garbage Delete the store paths no profile or GC root uses. Deleting users' old profile generations first frees more, but they can no longer roll back to them.
    (Also delete generations over 14 days…) (Collect)
    |}];
  let h = H.create () in
  H.act h Open_settings;
  H.text h ~selector:".settings-section h3";
  [%expect
    {|
    (Focus dialog)
    Appearance
    Preferences
    On the backend
    Git hosting
    Compaction
    Account
    |}]
;;

let%expect_test
    "Collect: progress in Settings and the status, then what was freed"
  =
  let h = superuser () in
  H.quiet h (fun () ->
    H.act h Open_settings;
    H.reply h "nix_store_status" status_json);
  H.click h ~selector:".nix-gc .primary";
  H.event
    h
    {|{"type":"event","event":"nix_gc_progress","paths_deleted":12,
       "generations_deleted":0,"message":"deleting unused links..."}|};
  section h;
  H.text h ~selector:".status-extras .nix-gc";
  H.reply h "nix_store_gc" report_json;
  section h;
  H.elements h ~selector:".status-extras .nix-gc";
  [%expect
    {|
    (Run "/nix-gc run")
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ nix_store_gc) (params ()) (tag Nix_collected))
    Nix store
    Disk usage Store: 12.0 GiB in 4321 paths; /nix: 40.0 GiB free of 100.0 GiB (40%). Last collection: 2025-10-04 17:46 by the automatic collection, freed 1.2 GiB: 120 paths, 3 generations deleted in 42 s Automatic: every 1 day, deleting profile generations older than 14 days; next at 2025-10-05 17:46
    (Refresh)
    Collect garbage Delete the store paths no profile or GC root uses. Deleting users' old profile generations first frees more, but they can no longer roll back to them. Collecting: 12 paths deleted · deleting unused links...
    (Collecting…)
    (Nix GC)
    (Expire_toast (id 1) (after_ms 4020))
    (Rpc (method_ nix_store_status) (params ())
     (tag (Nix_status (open_settings false))))
    Nix store
    Disk usage Store: 12.0 GiB in 4321 paths; /nix: 40.0 GiB free of 100.0 GiB (40%). Last collection: 2025-10-04 17:46 by the automatic collection, freed 1.2 GiB: 120 paths, 3 generations deleted in 42 s Automatic: every 1 day, deleting profile generations older than 14 days; next at 2025-10-05 17:46
    (Refresh)
    Collect garbage Delete the store paths no profile or GC root uses. Deleting users' old profile generations first frees more, but they can no longer roll back to them. Freed 500.0 MiB: 40 paths deleted in 5 s
    (Also delete generations over 14 days…) (Collect)
    |}]
;;

let%expect_test "deleting generations asks first, then returns to Settings" =
  let h = superuser () in
  H.quiet h (fun () ->
    H.act h Open_settings;
    H.reply h "nix_store_status" status_json);
  H.click h ~selector:".nix-gc .ghost";
  H.text h ~selector:".nix-gc-confirm";
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:".settings-dialog";
  H.click h ~selector:".nix-gc .ghost";
  H.click h ~selector:".nix-gc-confirm .danger";
  H.elements h ~selector:".settings-dialog";
  [%expect
    {|
    (Run "/nix-gc run 14")
    (Focus dialog)
    Delete old profile generations?
    (Close (Esc))
    Collecting Nix garbage, first deleting profile generations older than 14 days of every user. They can no longer roll back to them; current ones are kept.
    Collect in Settings keeps the generations.
    (Cancel) (Delete and collect)
    Close_dialog
    (Focus dialog)
    <div class="modal settings-dialog">
    (Run "/nix-gc run 14")
    (Focus dialog)
    Dialog_accept
    (Focus dialog)
    (Expire_toast (id 0) (after_ms 4460))
    (Rpc (method_ nix_store_gc) (params ((delete_generations 14)))
     (tag Nix_collected))
    <div class="modal settings-dialog">
    |}]
;;

let%expect_test "/nix-gc opens Settings once the store is read" =
  let h = superuser () in
  H.type_ h "/nix-gc";
  H.act h Send;
  H.reply h "nix_store_status" status_json;
  H.elements h ~selector:".settings-dialog";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /nix-gc) (max 500)))
     (tag Ignore))
    (Rpc (method_ nix_store_status) (params ())
     (tag (Nix_status (open_settings true))))
    (Focus dialog)
    <div class="modal settings-dialog">
    |}]
;;

let%expect_test "to everyone else /nix-gc does not exist" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"
         ; namespace = Some "bob"
         ; user = Some "bob"
         ; superuser = false
         }));
  let completions () =
    print_s
      [%sexp
        (Option.value_map (H.model h).completion ~default:[] ~f:(fun c ->
           List.map (Prigh_web.Completion.items c) ~f:(fun i -> i.id))
         : string list)]
  in
  H.quiet h (fun () -> H.type_ h "/nix");
  completions ();
  H.quiet h (fun () -> H.type_ h "/nix-gc");
  H.act h Send;
  H.text h ~selector:".toasts";
  H.quiet h (fun () ->
    H.type_ h "/help nix-gc";
    H.act h Send);
  H.text h ~selector:".toasts";
  [%expect
    {|
    ()
    (Rpc (method_ push_pref) (params ((key history) (value /nix-gc) (max 500)))
     (tag Ignore))
    Unknown command /nix-gc: /help lists the commands. (Dismiss)
    No command /nix-gc: /help lists them. (Dismiss)
    |}];
  H.quiet h (fun () -> H.act h Open_help);
  H.text h ~selector:".help-dialog";
  let help = [%expect.output] in
  print_s
    [%message
      "help"
        ~commands:(String.is_substring help ~substring:"/setusr" : bool)
        ~mentions_nix:(String.is_substring help ~substring:"nix" : bool)];
  [%expect {| (help (commands true) (mentions_nix false)) |}]
;;

let%expect_test "a superuser acting as another user keeps /nix-gc" =
  let h = superuser () in
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"
         ; namespace = Some "bob"
         ; user = Some "alice"
         ; superuser = true
         });
    H.type_ h "/nix-gc");
  H.act h Send;
  [%expect
    {|
    (Cache_prefs (((key history) (value ((/nix-gc))))))
    (Rpc (method_ nix_store_status) (params ())
     (tag (Nix_status (open_settings true))))
    |}]
;;

let%expect_test
    "/nix-gc run DAYS|all asks; one at a time; mistakes say the usage"
  =
  let h = superuser () in
  H.quiet h (fun () ->
    H.type_ h "/nix-gc run all";
    H.act h Send);
  H.text h ~selector:".nix-gc-confirm";
  H.act h Dialog_accept;
  H.quiet h (fun () ->
    H.type_ h "/nix-gc run";
    H.act h Send);
  H.text h ~selector:".toasts";
  H.quiet h (fun () ->
    H.type_ h "/nix-gc run soon";
    H.act h Send);
  H.text h ~selector:".toasts";
  H.fail h "nix_store_gc" "nix store gc failed: disk on fire";
  H.text h ~selector:".toasts";
  [%expect
    {|
    Delete old profile generations?
    (Close (Esc))
    Collecting Nix garbage, first deleting every old profile generation of every user. They can no longer roll back to them; current ones are kept.
    Collect in Settings keeps the generations.
    (Cancel) (Delete and collect)
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4060))
    (Rpc (method_ nix_store_gc) (params ((delete_generations all)))
     (tag Nix_collected))
    Collecting Nix garbage, deleting every old profile generation…
    The Nix garbage collection you started is still running: Settings shows how far it got. (Dismiss)
    Collecting Nix garbage, deleting every old profile generation…
    "soon" is not a number of days: /nix-gc [run [DAYS|all]] (no argument: show the store; run: collect garbage; run DAYS: first delete profile generations older than DAYS days; run all: all but the current ones) (Dismiss)
    (Rpc (method_ nix_store_status) (params ())
     (tag (Nix_status (open_settings false))))
    "soon" is not a number of days: /nix-gc [run [DAYS|all]] (no argument: show the store; run: collect garbage; run DAYS: first delete profile generations older than DAYS days; run all: all but the current ones) (Dismiss)
    Nix garbage collection failed: nix store gc failed: disk on fire (Dismiss)
    |}]
;;
