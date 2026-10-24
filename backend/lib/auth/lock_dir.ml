open! Core
open! Import

let lock_path ~file = file ^ ".lock"

let is_stale ~stale lock =
  match Core_unix.stat lock with
  | exception Core_unix.Unix_error (ENOENT, _, _) -> `Gone
  | st ->
    if
      not
        (match st.st_kind with
         | S_DIR -> true
         | _ -> false)
    then
      (* A leftover from the earlier lockf-based scheme; it never meant a
         live holder. *)
      `Stale `File
    else (
      let age =
        Time_ns.diff
          (Time_ns.now ())
          (Time_ns.of_span_since_epoch (Time_ns.Span.of_sec st.st_mtime))
      in
      if Time_ns.Span.( >= ) age stale then `Stale `Dir else `Held)
;;

let remove lock = function
  | `File ->
    (try Core_unix.unlink lock with
     | Core_unix.Unix_error (ENOENT, _, _) -> ())
  | `Dir ->
    (try Core_unix.rmdir lock with
     | Core_unix.Unix_error (ENOENT, _, _) -> ())
;;

let try_acquire lock =
  match Core_unix.mkdir lock ~perm:0o700 with
  | () -> true
  | exception Core_unix.Unix_error (EEXIST, _, _) -> false
;;

let acquire ~stale ~timeout lock =
  let deadline = Time_ns.add (Time_ns.now ()) timeout in
  let rec go ~delay =
    if try_acquire lock
    then ()
    else (
      match is_stale ~stale lock with
      | `Gone -> go ~delay
      | `Stale kind ->
        remove lock kind;
        go ~delay
      | `Held ->
        if Time_ns.( >= ) (Time_ns.now ()) deadline
        then
          raise_s
            [%message
              "lock is held by another process"
                (lock : string)
                (timeout : Time_ns.Span.t)];
        Eio_unix.sleep (Time_ns.Span.to_sec delay);
        go
          ~delay:
            (Time_ns.Span.min
               (Time_ns.Span.scale delay 2.)
               (Time_ns.Span.of_sec 1.)))
  in
  go ~delay:(Time_ns.Span.of_ms 10.)
;;

let touch lock =
  let now = Core_unix.gettimeofday () in
  try Core_unix.utimes lock ~access:now ~modif:now with
  | Core_unix.Unix_error _ -> ()
;;

let with_lock
      ?(stale = Time_ns.Span.of_sec 30.)
      ?(timeout = Time_ns.Span.of_sec 30.)
      ?(touch_every = Time_ns.Span.of_sec 3.)
      ~file
      ~f
      ()
  =
  let lock = lock_path ~file in
  acquire ~stale ~timeout lock;
  Exn.protect
    ~f:(fun () ->
      Eio.Switch.run
      @@ fun sw ->
      Eio.Fiber.fork_daemon ~sw (fun () ->
        while true do
          Eio_unix.sleep (Time_ns.Span.to_sec touch_every);
          touch lock
        done;
        `Stop_daemon);
      f ())
    ~finally:(fun () -> remove lock `Dir)
;;
