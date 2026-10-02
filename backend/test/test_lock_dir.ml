open! Core
open! Prigh
open Tool_test_helpers

let kind path =
  match Core_unix.stat path with
  | exception Core_unix.Unix_error (ENOENT, _, _) -> "absent"
  | st ->
    (match st.st_kind with
     | S_DIR -> "directory"
     | S_REG -> "file"
     | _ -> "other")
;;

let%expect_test "lock is a directory next to the file, removed afterwards" =
  with_sandbox
  @@ fun t ->
  let file = Filename.concat t.dir "auth.json" in
  let lock = Lock_dir.lock_path ~file in
  printf "before: %s\n" (kind lock);
  Lock_dir.with_lock ~file () ~f:(fun () -> printf "inside: %s\n" (kind lock));
  printf "after: %s\n" (kind lock);
  [%expect
    {|
    before: absent
    inside: directory
    after: absent
    |}];
  (* Released on exceptions too. *)
  (try Lock_dir.with_lock ~file () ~f:(fun () -> failwith "boom") with
   | Failure m -> printf "raised %s\n" m);
  printf "after failure: %s\n" (kind lock);
  [%expect
    {|
    raised boom
    after failure: absent
    |}]
;;

let%expect_test "a live foreign lock is waited for; a stale one is broken" =
  with_sandbox
  @@ fun t ->
  let file = Filename.concat t.dir "auth.json" in
  let lock = Lock_dir.lock_path ~file in
  (* Another process holds the lock (proper-lockfile: a fresh directory). *)
  Core_unix.mkdir lock;
  let released = ref false in
  Eio.Fiber.both
    (fun () ->
       Eio_unix.sleep 0.1;
       released := true;
       Core_unix.rmdir lock)
    (fun () ->
       Lock_dir.with_lock ~file () ~f:(fun () ->
         printf "acquired after release: %b\n" !released));
  [%expect {| acquired after release: true |}];
  (* A live holder outlasting the timeout. *)
  Core_unix.mkdir lock;
  (match
     Lock_dir.with_lock ~timeout:(Time_ns.Span.of_ms 50.) ~file () ~f:(fun () ->
       print_endline "unexpected")
   with
   | () -> ()
   | exception e -> print_endline (mask t (Exn.to_string e)));
  [%expect
    {|
    ("lock is held by another process"
      (lock $DIR/auth.json.lock)
      (timeout 50ms))
    |}];
  (* The same lock becomes stale once its mtime is old enough. *)
  let old = Core_unix.gettimeofday () -. 60. in
  Core_unix.utimes lock ~access:old ~modif:old;
  Lock_dir.with_lock
    ~stale:(Time_ns.Span.of_sec 30.)
    ~timeout:(Time_ns.Span.of_ms 50.)
    ~file
    ()
    ~f:(fun () -> print_endline "acquired by breaking the stale lock");
  [%expect {| acquired by breaking the stale lock |}];
  (* A plain file at the lock path (the previous lockf scheme) is removed. *)
  Out_channel.write_all lock ~data:"";
  Lock_dir.with_lock ~timeout:(Time_ns.Span.of_ms 50.) ~file () ~f:(fun () ->
    printf "acquired over a stale lock file: %s\n" (kind lock));
  [%expect {| acquired over a stale lock file: directory |}]
;;

let%expect_test "the holder keeps the lock's mtime fresh" =
  with_sandbox
  @@ fun t ->
  let file = Filename.concat t.dir "auth.json" in
  let lock = Lock_dir.lock_path ~file in
  Lock_dir.with_lock
    ~touch_every:(Time_ns.Span.of_ms 20.)
    ~file
    ()
    ~f:(fun () ->
      let old = Core_unix.gettimeofday () -. 60. in
      Core_unix.utimes lock ~access:old ~modif:old;
      let age () =
        Core_unix.gettimeofday () -. (Core_unix.stat lock).st_mtime
      in
      printf "aged: %b\n" Float.(age () > 30.);
      Eio_unix.sleep 0.1;
      printf "touched: %b\n" Float.(age () < 30.));
  [%expect
    {|
    aged: true
    touched: true
    |}]
;;
