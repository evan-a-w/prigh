open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf
module Promise = Eio.Promise
module Fiber = Eio.Fiber

let%expect_test "Nix's output: lines, sizes, path-info and df" =
  List.iter
    [ "deleting '/nix/store/abc-hello-2.12'"
    ; "removing profile version 7"
    ; "removing old generations of profile \
       /home/a/.local/state/nix/profiles/profile"
    ; "2 store paths deleted, 1.50 MiB freed"
    ; "1 store path deleted, 512 B freed"
    ; "0 store paths deleted, 0.00 MiB freed"
    ; "12 store paths deleted, 2.3 GiB freed"
    ; "3 store paths deleted, lots freed"
    ; "  finding garbage collector roots...  "
    ]
    ~f:(fun line ->
      print_s [%sexp (Nix_gc_output.Line.classify line : Nix_gc_output.Line.t)]);
  [%expect
    {|
    (Deleting /nix/store/abc-hello-2.12)
    Removing_generation
    (Other
     "removing old generations of profile /home/a/.local/state/nix/profiles/profile")
    (Summary (paths 2) (bytes 1572864))
    (Summary (paths 1) (bytes 512))
    (Summary (paths 0) (bytes 0))
    (Summary (paths 12) (bytes 2469606195))
    (Other "3 store paths deleted, lots freed")
    (Other "finding garbage collector roots...")
    |}];
  print_s
    [%sexp
      (Nix_store_ops.parse_path_info
         "/nix/store/aaa-x   \t  100\n/nix/store/bbb-y\t2048\n\n"
       : (int * int) Or_error.t)];
  print_s
    [%sexp (Nix_store_ops.parse_path_info "garbage" : (int * int) Or_error.t)];
  print_s
    [%sexp
      (Nix_store_ops.parse_df
         "Filesystem 1024-blocks Used Available Capacity Mounted on\n\
          /dev/sda1 1000 600 400 60% /nix\n"
       : (int * int) Or_error.t)];
  print_s [%sexp (Nix_store_ops.parse_df "nothing" : (int * int) Or_error.t)];
  [%expect
    {|
    (Ok (2 2148))
    (Error "unexpected nix path-info line \"garbage\"")
    (Ok (1024000 409600))
    (Error "unexpected df output \"nothing\"")
    |}]
;;

let%expect_test "which generations: from requests and the helper" =
  List.iter
    [ None
    ; Some `Null
    ; Some (`Number "14")
    ; Some (`Number "0")
    ; Some (`String "all")
    ; Some (`Number "-1")
    ; Some (`Number "1.5")
    ; Some (`String "old")
    ]
    ~f:(fun json ->
      let parsed = Nix_generations.of_json json in
      print_s
        [%message
          ""
            ~_:
              (Option.value_map json ~default:"absent" ~f:Json.to_string
               : string)
            ~_:(parsed : Nix_generations.t option Or_error.t)
            ~arg:
              (Result.map parsed ~f:Nix_generations.to_arg : string Or_error.t)]);
  List.iter [ "keep"; "all"; "30d"; "0d"; "d"; "99999d"; "x" ] ~f:(fun arg ->
    print_s
      [%message
        arg
          ~_:(Nix_generations.of_arg arg : Nix_generations.t option Or_error.t)]);
  [%expect
    {|
    (absent (Ok ()) (arg (Ok keep)))
    (null (Ok ()) (arg (Ok keep)))
    (14 (Ok ((Older_than_days 14))) (arg (Ok 14d)))
    (0 (Ok (All)) (arg (Ok all)))
    ("\"all\"" (Ok (All)) (arg (Ok all)))
    (-1
     (Error
      "param \"delete_generations\" must be a number of days (0 for all old generations) or \"all\"")
     (arg
      (Error
       "param \"delete_generations\" must be a number of days (0 for all old generations) or \"all\"")))
    (1.5
     (Error
      "param \"delete_generations\" must be a number of days (0 for all old generations) or \"all\"")
     (arg
      (Error
       "param \"delete_generations\" must be a number of days (0 for all old generations) or \"all\"")))
    ("\"old\""
     (Error
      "param \"delete_generations\" must be a number of days (0 for all old generations) or \"all\"")
     (arg
      (Error
       "param \"delete_generations\" must be a number of days (0 for all old generations) or \"all\"")))
    (keep (Ok ()))
    (all (Ok (All)))
    (30d (Ok ((Older_than_days 30))))
    (0d (Ok (All)))
    (d (Error "bad generations \"d\" (keep, all or Nd)"))
    (99999d (Error "99999 days is out of range (0 to 36500)"))
    (x (Error "bad generations \"x\" (keep, all or Nd)"))
    |}]
;;

let%expect_test "the helper's files" =
  let report =
    "status=ok\n\
     at=1700000000.5\n\
     by=alice\n\
     generations=14d\n\
     generations_deleted=3\n\
     paths_deleted=120\n\
     freed_bytes=5000000\n\
     seconds=12.25\n"
  in
  print_s [%sexp (Nix_gc_report.of_fields report : Nix_gc_report.t Or_error.t)];
  print_endline
    (Json.to_string
       (Nix_gc_report.to_json
          (Or_error.ok_exn (Nix_gc_report.of_fields report))));
  print_s
    [%sexp
      (Nix_gc_report.of_fields "at=1\nby=x\n" : Nix_gc_report.t Or_error.t)];
  print_s
    [%sexp
      (Nix_store_status.Running.of_fields
         "by=schedule\nsince=1700000000\ngenerations=all\n"
       : Nix_store_status.Running.t Or_error.t)];
  print_s
    [%sexp
      (Nix_store_status.Schedule.of_fields
         "interval=86400\nkeep_days=14\nnext_at=1700086400\n"
       : Nix_store_status.Schedule.t Or_error.t)];
  print_s
    [%sexp
      (Nix_store_status.Schedule.of_fields
         "interval=0\nkeep_days=off\nnext_at=\n"
       : Nix_store_status.Schedule.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((at 1700000000.5) (by alice) (generations ((Older_than_days 14)))
      (generations_deleted 3) (paths_deleted 120) (freed_bytes 5000000)
      (seconds 12.25)))
    {"at_ms":1700000000500,"by":"alice","generations":14,"generations_deleted":3,"paths_deleted":120,"freed_bytes":5000000,"duration_ms":12250}
    (Error "no generations")
    (Ok ((by schedule) (since 1700000000) (generations (All))))
    (Ok ((interval_seconds 86400) (keep_days (14)) (next_at (1700086400))))
    (Ok ((interval_seconds 0) (keep_days ()) (next_at ())))
    |}]
;;

(* Fake ops: [collect] reports the lines it is given and finishes when
   [finish] is filled. *)
module Fake = struct
  type t =
    { mutable lines : string list
    ; mutable finish : Nix_gc_report.t Or_error.t Promise.t
    ; mutable available : unit Or_error.t
    ; mutable scheduled : Nix_store_status.Running.t option
    ; mutable helper_last : Nix_gc_report.t option
    ; calls : string Queue.t
    }

  let report ?(at = 100.) ~by generations =
    { Nix_gc_report.at
    ; by
    ; generations
    ; generations_deleted = 1
    ; paths_deleted = 2
    ; freed_bytes = 3000
    ; seconds = 4.
    }
  ;;

  let create () =
    { lines = []
    ; finish = Promise.create_resolved (Ok (report ~by:"x" None))
    ; available = Ok ()
    ; scheduled = None
    ; helper_last = None
    ; calls = Queue.create ()
    }
  ;;

  let ops t =
    { Nix_store_ops.check = (fun () -> t.available)
    ; store =
        (fun () ->
          Or_error.map t.available ~f:(fun () ->
            { Nix_store_status.Store.bytes = 5_000_000
            ; paths = 42
            ; fs_total_bytes = 100_000_000
            ; fs_free_bytes = 25_000_000
            }))
    ; last_gc = (fun () -> t.helper_last)
    ; running = (fun () -> t.scheduled)
    ; schedule =
        (fun () ->
          Some
            { interval_seconds = 86400
            ; keep_days = Some 14
            ; next_at = Some 200.
            })
    ; collect =
        (fun ~by generations ~on_line ->
          Queue.enqueue
            t.calls
            (sprintf
               "collect by %s, generations %s"
               by
               (Nix_generations.to_arg generations));
          List.iter t.lines ~f:on_line;
          Or_error.map (Promise.await t.finish) ~f:(fun (r : Nix_gc_report.t) ->
            { r with by; generations }))
    }
  ;;
end

let print_progress (p : Nix_gc_progress.t) =
  print_endline (Json.to_string (Nix_gc_progress.to_json p))
;;

let print_status store =
  match Nix_store.status store with
  | Ok status ->
    print_endline (Json.to_string (Nix_store_status.to_json status))
  | Error e -> print_endline (Error.to_string_hum e)
;;

let%expect_test "one garbage collection at a time, progress, last one" =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.create () in
  let clock = ref 0. in
  let now () =
    (* Each line is a tenth of a second later. *)
    clock := !clock +. 0.1;
    !clock
  in
  let store = Nix_store.create ~sw ~now (Fake.ops fake) in
  print_status store;
  [%expect
    {| {"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":null,"running":null,"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}} |}];
  let finish, resolve = Promise.create () in
  fake.finish <- finish;
  fake.lines
  <- [ "removing old generations of profile \
        /home/a/.local/state/nix/profiles/profile"
     ; "removing profile version 3"
     ; "finding garbage collector roots..."
     ; ""
     ]
     @ List.init 7 ~f:(sprintf "deleting '/nix/store/p%d'")
     @ [ "7 store paths deleted, 1.00 KiB freed" ];
  let first =
    Fiber.fork_promise ~sw (fun () ->
      Nix_store.gc
        store
        ~by:"s"
        (Some (Older_than_days 14))
        ~on_progress:print_progress)
  in
  Fiber.yield ();
  [%expect
    {|
    {"type":"event","event":"nix_gc_progress","paths_deleted":0,"generations_deleted":0,"message":"removing old generations of profile /home/a/.local/state/nix/profiles/profile"}
    {"type":"event","event":"nix_gc_progress","paths_deleted":0,"generations_deleted":1,"message":"removing profile version 3"}
    {"type":"event","event":"nix_gc_progress","paths_deleted":0,"generations_deleted":1,"message":"finding garbage collector roots..."}
    {"type":"event","event":"nix_gc_progress","paths_deleted":2,"generations_deleted":1,"message":null}
    {"type":"event","event":"nix_gc_progress","paths_deleted":5,"generations_deleted":1,"message":null}
    {"type":"event","event":"nix_gc_progress","paths_deleted":7,"generations_deleted":1,"message":"7 store paths deleted, 1.00 KiB freed"}
    |}];
  (* Meanwhile: shown as running, and a second one is refused. *)
  print_status store;
  print_s
    [%sexp
      (Nix_store.gc store ~by:"t" None ~on_progress:print_progress
       : Nix_gc_report.t Or_error.t)];
  [%expect
    {|
    {"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":null,"running":{"by":"s","since_ms":100,"generations":14},"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}}
    (Error
     "a Nix garbage collection is already running (started by s): wait for it to finish")
    |}];
  Promise.resolve resolve (Ok (Fake.report ~at:150. ~by:"" None));
  print_s [%sexp (Promise.await_exn first : Nix_gc_report.t Or_error.t)];
  print_status store;
  [%expect
    {|
    (Ok
     ((at 150) (by s) (generations ((Older_than_days 14)))
      (generations_deleted 1) (paths_deleted 2) (freed_bytes 3000) (seconds 4)))
    {"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":{"at_ms":150000,"by":"s","generations":14,"generations_deleted":1,"paths_deleted":2,"freed_bytes":3000,"duration_ms":4000},"running":null,"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}}
    |}];
  (* The helper's record wins when it is newer (the schedule ran since). *)
  fake.helper_last <- Some (Fake.report ~at:300. ~by:"schedule" (Some All));
  print_status store;
  [%expect
    {| {"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":{"at_ms":300000,"by":"schedule","generations":"all","generations_deleted":1,"paths_deleted":2,"freed_bytes":3000,"duration_ms":4000},"running":null,"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}} |}];
  (* A caller that goes away (its connection dropped) stops getting
     progress; the collection finishes and counts. *)
  let finish, resolve = Promise.create () in
  fake.finish <- finish;
  fake.lines <- [];
  let cancelled =
    Fiber.fork_promise ~sw (fun () ->
      Eio.Cancel.sub (fun cc ->
        Fiber.fork ~sw (fun () ->
          Eio.Cancel.cancel cc (Failure "connection dropped"));
        Nix_store.gc store ~by:"s" None ~on_progress:print_progress))
  in
  ignore (Promise.await cancelled : (Nix_gc_report.t Or_error.t, exn) result);
  print_status store;
  Promise.resolve resolve (Ok (Fake.report ~at:400. ~by:"" None));
  Fiber.yield ();
  print_status store;
  [%expect
    {|
    {"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":{"at_ms":300000,"by":"schedule","generations":"all","generations_deleted":1,"paths_deleted":2,"freed_bytes":3000,"duration_ms":4000},"running":{"by":"s","since_ms":1400,"generations":null},"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}}
    {"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":{"at_ms":400000,"by":"s","generations":null,"generations_deleted":1,"paths_deleted":2,"freed_bytes":3000,"duration_ms":4000},"running":null,"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}}
    |}];
  (* The schedule's collection blocks ours; failures and a missing nix are
     reported. *)
  fake.scheduled
  <- Some
       { by = "schedule"
       ; since = 500.
       ; generations = Some (Older_than_days 14)
       };
  print_s
    [%sexp
      (Nix_store.gc store ~by:"s" None ~on_progress:print_progress
       : Nix_gc_report.t Or_error.t)];
  fake.scheduled <- None;
  fake.finish
  <- Promise.create_resolved (Or_error.error_string "nix store gc failed");
  print_s
    [%sexp
      (Nix_store.gc store ~by:"s" None ~on_progress:print_progress
       : Nix_gc_report.t Or_error.t)];
  fake.available <- Or_error.error_string "no nix here";
  print_s
    [%sexp
      (Nix_store.gc store ~by:"s" None ~on_progress:print_progress
       : Nix_gc_report.t Or_error.t)];
  print_status store;
  print_s [%sexp (Queue.to_list fake.calls : string list)];
  [%expect
    {|
    (Error
     "a Nix garbage collection is already running (started by the automatic garbage collection): wait for it to finish")
    (Error "nix store gc failed")
    (Error "no nix here")
    no nix here
    ("collect by s, generations 14d" "collect by s, generations keep"
     "collect by s, generations keep")
    |}]
;;

let make_server t ~sw ?nix_store () =
  let provider = Faux_provider.create [] in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let home = Filename.concat t.dir "home" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home
      ?session
      ~cwd
      ()
  in
  Rpc_server.create
    ~env:t.env
    ~sw
    ?nix_store
    ~login:
      (Login_manager.create
         ~env:t.env
         ~sw
         ~getenv:(fun _ -> None)
         ~store:(Auth_store.create ~path:(Filename.concat home "auth.json"))
         ())
    ~sessions_dir
    ~cwd:t.dir
    ~new_agent
    ()
;;

let rpc server ?signed_in () =
  let sent = Queue.create () in
  let client =
    Rpc_server.connect server ?signed_in ~send:(Queue.enqueue sent)
  in
  ignore
    (Rpc_server.handle
       server
       client
       (Json.of_string {|{"id":0,"method":"hello","params":{}}|})
     : Json.t);
  Queue.clear sent;
  fun ?(params = "{}") meth ->
    let response =
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id":1,"method":"%s","params":%s}|} meth params))
    in
    Queue.iter sent ~f:(fun json -> print_endline (Json.to_string json));
    Queue.clear sent;
    print_endline (Json.to_string response)
;;

let%expect_test "rpc: only superusers, who see the store and collect" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = Fake.create () in
  fake.lines
  <- [ "deleting '/nix/store/p1'"; "1 store path deleted, 3.00 KiB freed" ];
  let nix_store = Nix_store.create ~sw ~now:(fun () -> 10.) (Fake.ops fake) in
  let server = make_server t ~sw ~nix_store () in
  let signed_in ?(superusers = [ "s"; "r" ]) user =
    let access =
      User_access.create
        (Or_error.ok_exn (Namespace.parse_spec "a=tok-a,r=tok-r,s=tok-s"))
        ~host_tokens:[]
        ~superusers
      |> Or_error.ok_exn
    in
    fst
      (Or_error.ok_exn (User_access.authenticate access (Some ("tok-" ^ user))))
  in
  let as_a = rpc server ~signed_in:(signed_in "a") () in
  as_a "nix_store_status";
  as_a "nix_store_gc";
  let lonely = rpc server ~signed_in:(signed_in ~superusers:[] "a") () in
  lonely "nix_store_status";
  let anonymous = rpc server () in
  anonymous "nix_store_gc";
  [%expect
    {|
    {"type":"response","id":1,"ok":false,"error":"cleaning the Nix store is for superusers: ask r or s to run /nix-gc"}
    {"type":"response","id":1,"ok":false,"error":"cleaning the Nix store is for superusers: ask r or s to run /nix-gc"}
    {"type":"response","id":1,"ok":false,"error":"cleaning the Nix store is for superusers, and there are none: ask whoever runs prigh to make a one (PRIGH_SUPERUSERS, or -superusers)"}
    {"type":"response","id":1,"ok":false,"error":"cleaning the Nix store from prigh is for superusers, and this backend has no users: run `nix store gc` in a shell, or serve with -tokens and -superusers"}
    |}];
  let as_s = rpc server ~signed_in:(signed_in "s") () in
  as_s "nix_store_status";
  as_s "nix_store_gc" ~params:{|{"delete_generations": 30}|};
  as_s "nix_store_gc" ~params:{|{"delete_generations": "some"}|};
  as_s "nix_store_status";
  print_s [%sexp (Queue.to_list fake.calls : string list)];
  [%expect
    {|
    {"type":"response","id":1,"ok":true,"result":{"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":null,"running":null,"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}}}
    {"type":"event","event":"nix_gc_progress","paths_deleted":1,"generations_deleted":0,"message":null}
    {"type":"event","event":"nix_gc_progress","paths_deleted":1,"generations_deleted":0,"message":"1 store path deleted, 3.00 KiB freed"}
    {"type":"response","id":1,"ok":true,"result":{"at_ms":100000,"by":"s","generations":30,"generations_deleted":1,"paths_deleted":2,"freed_bytes":3000,"duration_ms":4000}}
    {"type":"response","id":1,"ok":false,"error":"param \"delete_generations\" must be a number of days (0 for all old generations) or \"all\""}
    {"type":"response","id":1,"ok":true,"result":{"store_bytes":5000000,"store_paths":42,"fs_total_bytes":100000000,"fs_free_bytes":25000000,"last_gc":{"at_ms":100000,"by":"s","generations":30,"generations_deleted":1,"paths_deleted":2,"freed_bytes":3000,"duration_ms":4000},"running":null,"schedule":{"interval_seconds":86400,"keep_days":14,"next_at_ms":200000}}}
    ("collect by s, generations 30d")
    |}];
  (* Without a store to manage. *)
  let bare = make_server t ~sw () in
  rpc bare ~signed_in:(signed_in "s") () "nix_store_status";
  [%expect
    {| {"type":"response","id":1,"ok":false,"error":"this backend does not manage a Nix store"} |}]
;;

(* Executables in a directory of [t] that is all of PATH. *)
let with_path t scripts f =
  let bin = Filename.concat t.dir "bin" in
  Core_unix.mkdir_p bin;
  List.iter scripts ~f:(fun (name, body) ->
    let file = Filename.concat bin name in
    Out_channel.write_all file ~data:("#!/bin/sh\n" ^ body);
    Core_unix.chmod file ~perm:0o755);
  let old = Sys.getenv "PATH" in
  Core_unix.putenv ~key:"PATH" ~data:bin;
  Exn.protect ~f ~finally:(fun () ->
    match old with
    | Some path -> Core_unix.putenv ~key:"PATH" ~data:path
    | None -> Core_unix.unsetenv "PATH")
;;

let print_report ?(mask = Fn.id) (r : Nix_gc_report.t Or_error.t) =
  match r with
  | Ok r ->
    print_s
      [%message
        ""
          ~by:(r.by : string)
          ~generations:(r.generations : Nix_generations.t option)
          ~generations_deleted:(r.generations_deleted : int)
          ~paths_deleted:(r.paths_deleted : int)
          ~freed_bytes:(r.freed_bytes : int)]
  | Error e -> print_endline (mask (Error.to_string_hum e))
;;

let%expect_test "local: nix as the backend's user" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let log = Filename.concat t.dir "calls" in
  let record = sprintf "echo \"${0##*/} $*\" >> %s\n" log in
  with_path
    t
    [ ( "nix"
      , record
        ^ {|case "$1" in
path-info) printf '/nix/store/aaa-x\t100\n/nix/store/bbb-y\t2000\n' ;;
store)
  [ -e "${0%/*}/fail" ] && { echo "error: cannot connect to the daemon" >&2; exit 1; }
  echo "finding garbage collector roots..." >&2
  echo "deleting '/nix/store/aaa-x'" >&2
  echo "deleting unused links..." >&2
  echo "1 store paths deleted, 0.10 KiB freed" >&2 ;;
esac
|}
      )
    ; ( "nix-collect-garbage"
      , record
        ^ {|echo "removing old generations of profile /home/u/.local/state/nix/profiles/profile"
echo "removing profile version 1"
echo "removing profile version 2"
echo "deleting '/nix/store/aaa-x'" >&2
echo "deleting '/nix/store/bbb-y'" >&2
echo "2 store paths deleted, 2.00 KiB freed" >&2
|}
      )
    ; ( "df"
      , record
        ^ {|echo "Filesystem 1024-blocks Used Available Capacity Mounted on"
echo "/dev/x 1000 300 700 30% /nix"
|}
      )
    ]
  @@ fun () ->
  let store =
    Nix_store.create
      ~sw
      ~now:(fun () -> 0.)
      ~progress_interval:1000.
      (Nix_store_ops.local ~env:t.env)
  in
  print_status store;
  let gc generations =
    print_report
      (Nix_store.gc store ~by:"s" generations ~on_progress:(fun p ->
         Option.iter p.message ~f:(printf "  progress: %s\n")))
  in
  gc None;
  gc (Some (Older_than_days 14));
  gc (Some All);
  print_string (In_channel.read_all log);
  [%expect
    {|
    {"store_bytes":2100,"store_paths":2,"fs_total_bytes":1024000,"fs_free_bytes":716800,"last_gc":null,"running":null,"schedule":null}
      progress: finding garbage collector roots...
      progress: deleting unused links...
      progress: 1 store paths deleted, 0.10 KiB freed
    ((by s) (generations ()) (generations_deleted 0) (paths_deleted 1)
     (freed_bytes 102))
      progress: removing old generations of profile /home/u/.local/state/nix/profiles/profile
      progress: removing profile version 1
      progress: removing profile version 2
      progress: 2 store paths deleted, 2.00 KiB freed
    ((by s) (generations ((Older_than_days 14))) (generations_deleted 2)
     (paths_deleted 2) (freed_bytes 2048))
      progress: removing old generations of profile /home/u/.local/state/nix/profiles/profile
      progress: removing profile version 1
      progress: removing profile version 2
      progress: 2 store paths deleted, 2.00 KiB freed
    ((by s) (generations (All)) (generations_deleted 2) (paths_deleted 2)
     (freed_bytes 2048))
    nix path-info --all -s
    df -Pk /nix/store
    df -Pk /nix/store
    nix store gc -v
    df -Pk /nix/store
    df -Pk /nix/store
    nix-collect-garbage -v --delete-older-than 14d
    df -Pk /nix/store
    df -Pk /nix/store
    nix-collect-garbage -v -d
    df -Pk /nix/store
    |}];
  Out_channel.write_all (Filename.concat t.dir "bin/fail") ~data:"";
  gc None;
  [%expect
    {|
      progress: error: cannot connect to the daemon
    nix failed (exit code 1): error: cannot connect to the daemon
    |}];
  Core_unix.unlink (Filename.concat t.dir "bin/nix");
  print_status store;
  gc None;
  [%expect
    {|
    nix is not installed where the backend runs (no nix on its PATH), so there is no Nix store to clean
    nix is not installed where the backend runs (no nix on its PATH), so there is no Nix store to clean
    |}]
;;

(* A stand-in for the container's helper: takes one request from the FIFO,
   writes a log and the result the way docker/prigh-docker does. It gives up
   after 10 seconds without a request, and is killed after 20, so that a
   failing test leaves no process behind. *)
let fake_helper ~dir ~result =
  sprintf
    {|exec 3<>%s/requests
: > %s/ready
read -r -t 10 verb id gens by <&3 || exit 1
echo "$verb $gens $by" > %s/request
printf 'by=%%s\nsince=5\ngenerations=%%s\n' "$by" "$gens" > %s/running
echo "removing profile version 4" > %s/$id.log
echo "deleting '/nix/store/x'" >> %s/$id.log
echo "1 store path deleted, 1.00 KiB freed" >> %s/$id.log
: > %s/running
printf '%%b' "%s" > %s/$id.result
|}
    dir
    dir
    dir
    dir
    dir
    dir
    dir
    dir
    result
    dir
;;

let%expect_test "helper: requests through the FIFO" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let dir = Filename.concat t.dir "nix-gc" in
  Core_unix.mkdir_p dir;
  with_path t [ "nix", "exit 0\n" ]
  @@ fun () ->
  let ops = Nix_store_ops.helper ~env:t.env ~dir in
  let store =
    Nix_store.create ~sw ~now:(fun () -> 0.) ~progress_interval:1000. ops
  in
  let gc () =
    print_report
      ~mask:(mask t)
      (Nix_store.gc
         store
         ~by:"alice"
         (Some (Older_than_days 7))
         ~on_progress:(fun p ->
           Option.iter p.message ~f:(printf "  progress: %s\n")))
  in
  (* Not set up, then nobody listening. *)
  gc ();
  Core_unix.mkfifo (Filename.concat dir "requests") ~perm:0o620;
  gc ();
  [%expect
    {|
    the container's Nix garbage collector is not set up (no $DIR/nix-gc/requests): restart the container
    the container's Nix garbage collector is not running: restart the container
    |}];
  let run_helper result =
    let ready = Filename.concat dir "ready" in
    (try Core_unix.unlink ready with
     | _ -> ());
    let helper =
      Fiber.fork_promise ~sw (fun () ->
        Process.run
          ~env:t.env
          ~timeout:(Time_ns.Span.of_int_sec 20)
          ~prog:"/bin/sh"
          ~args:[ "-c"; fake_helper ~dir ~result ]
          ())
    in
    while
      (not (Sys_unix.file_exists_exn ready))
      && not (Promise.is_resolved helper)
    do
      Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.01
    done;
    gc ();
    ignore (Promise.await_exn helper : Process.Exit.t);
    print_string (In_channel.read_all (Filename.concat dir "request"))
  in
  run_helper
    "status=ok\\nat=1700000000\\nby=$by\\ngenerations=$gens\\ngenerations_deleted=1\\npaths_deleted=1\\nfreed_bytes=1024\\nseconds=2\\n";
  [%expect
    {|
      progress: removing profile version 4
      progress: 1 store path deleted, 1.00 KiB freed
    ((by alice) (generations ((Older_than_days 7))) (generations_deleted 1)
     (paths_deleted 1) (freed_bytes 1024))
    gc 7d alice
    |}];
  run_helper "status=error\\nerror=nix store gc failed: disk on fire\\n";
  [%expect
    {|
      progress: removing profile version 4
      progress: 1 store path deleted, 1.00 KiB freed
    nix store gc failed: disk on fire
    gc 7d alice
    |}];
  (* What the helper records shows in the status. *)
  Out_channel.write_all
    (Filename.concat dir "last-gc")
    ~data:
      "at=1700000100\n\
       by=schedule\n\
       generations=14d\n\
       generations_deleted=0\n\
       paths_deleted=9\n\
       freed_bytes=99\n\
       seconds=1\n";
  Out_channel.write_all
    (Filename.concat dir "schedule")
    ~data:"interval=86400\nkeep_days=14\nnext_at=1700086500\n";
  Out_channel.write_all
    (Filename.concat dir "running")
    ~data:"by=schedule\nsince=1700000200\ngenerations=14d\n";
  print_s
    [%sexp
      ((ops.last_gc (), ops.schedule (), ops.running ())
       : Nix_gc_report.t option
         * Nix_store_status.Schedule.t option
         * Nix_store_status.Running.t option)];
  gc ();
  [%expect
    {|
    ((((at 1700000100) (by schedule) (generations ((Older_than_days 14)))
       (generations_deleted 0) (paths_deleted 9) (freed_bytes 99) (seconds 1)))
     (((interval_seconds 86400) (keep_days (14)) (next_at (1700086500))))
     (((by schedule) (since 1700000200) (generations ((Older_than_days 14))))))
    a Nix garbage collection is already running (started by the automatic garbage collection): wait for it to finish
    |}]
;;
