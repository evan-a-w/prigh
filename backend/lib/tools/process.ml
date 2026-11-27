open! Core
open! Import

module Exit = struct
  type t =
    | Exited of int
    | Signaled of Signal.t
    | Timed_out
    | Cancelled
  [@@deriving sexp_of]

  let is_success = function
    | Exited 0 -> true
    | Exited _ | Signaled _ | Timed_out | Cancelled -> false
  ;;
end

let pump source ~on_data =
  let buf = Cstruct.create 65536 in
  try
    while true do
      let n = Eio.Flow.single_read source buf in
      on_data (Cstruct.to_string buf ~len:n)
    done
  with
  | End_of_file -> ()
;;

let drain_after_kill = 1.

let run
      ~(env : Env.t)
      ?cwd
      ?(extra_env = [])
      ?(unset_env = [])
      ?(stdin = "")
      ?input
      ?timeout
      ?(cancel = Cancellation.never)
      ?(on_stdout = ignore)
      ?(on_stderr = ignore)
      ?(merge_stderr = false)
      ?term_grace
      ?(on_spawn = ignore)
      ?(extra_fds = [])
      ~prog
      ~args
      ()
  =
  Switch.run
  @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let stdin_r, stdin_w = Eio_unix.pipe sw in
  let stdout_r, stdout_w = Eio_unix.pipe sw in
  let stderr = if merge_stderr then None else Some (Eio_unix.pipe sw) in
  let process_env =
    if List.is_empty extra_env && List.is_empty unset_env
    then None
    else (
      let overridden =
        String.Set.of_list (unset_env @ List.map extra_env ~f:fst)
      in
      let inherited =
        Array.filter (Core_unix.environment ()) ~f:(fun entry ->
          match String.lsplit2 entry ~on:'=' with
          | Some (key, _) -> not (Set.mem overridden key)
          | None -> true)
      in
      let extra =
        let seen = String.Hash_set.create () in
        List.rev extra_env
        |> List.filter ~f:(fun (k, _) ->
          if Hash_set.mem seen k
          then false
          else (
            Hash_set.add seen k;
            true))
        |> List.rev
        |> List.map ~f:(fun (k, v) -> k ^ "=" ^ v)
        |> Array.of_list
      in
      Some (Array.append inherited extra))
  in
  (* The child leads its own process group so that on cancel or timeout its
     descendants die with it; otherwise they keep the pipes open and draining
     them would block until they exit on their own. *)
  let child =
    Eio_unix.Process.spawn_unix
      ~sw
      mgr
      ?cwd:(Option.map cwd ~f:(fun d -> Eio.Path.(Eio.Stdenv.fs env / d)))
      ~pgid:0
      ~fds:
        ([ 0, Eio_unix.Resource.fd stdin_r, `Blocking
         ; 1, Eio_unix.Resource.fd stdout_w, `Blocking
         ; ( 2
           , Eio_unix.Resource.fd
               (match stderr with
                | Some (_, stderr_w) -> stderr_w
                | None -> stdout_w)
           , `Blocking )
         ]
         @ List.map extra_fds ~f:(fun (n, fd) -> n, fd, `Blocking))
      ?env:process_env
      (prog :: args)
  in
  let group = `Group (Pid.of_int (Eio.Process.pid child)) in
  (* Until it is reaped: once it is, its pid (the group's id) may be
     another's. *)
  let reaped = ref false in
  (* Cancelled by Eio (the switch above ending, e.g. the process exiting),
     not by [cancel]: Eio kills only the child, not what it started. *)
  Switch.on_release sw (fun () ->
    if not !reaped then Signal_unix.send_i Signal.kill group);
  on_spawn (Pid.of_int (Eio.Process.pid child));
  Eio.Flow.close stdin_r;
  Eio.Flow.close stdout_w;
  Option.iter stderr ~f:(fun (_, stderr_w) -> Eio.Flow.close stderr_w);
  (* The child may exit without reading its input; that must not be an error. *)
  (match input with
   | None ->
     Fiber.fork ~sw (fun () ->
       (try Eio.Flow.copy_string stdin stdin_w with
        | Eio.Io _ -> ());
       Eio.Flow.close stdin_w)
   | Some input ->
     (* A daemon: the process may exit without its input being closed. *)
     Fiber.fork_daemon ~sw (fun () ->
       let rec loop ~open_ =
         match Stdin_feed.take input with
         | Data data ->
           if open_
           then (
             match Eio.Flow.copy_string data stdin_w with
             | () -> loop ~open_
             | exception Eio.Io _ -> loop ~open_:false)
           else loop ~open_
         | Close ->
           if open_ then Eio.Flow.close stdin_w;
           loop ~open_:false
       in
       loop ~open_:true));
  let drain () =
    Fiber.both
      (fun () -> pump stdout_r ~on_data:on_stdout)
      (fun () ->
         Option.iter stderr ~f:(fun (stderr_r, _) ->
           pump stderr_r ~on_data:on_stderr))
  in
  let wait () =
    drain ();
    Eio.Process.await child
  in
  let with_timeout f =
    match timeout with
    | None -> Ok (f ())
    | Some span ->
      Eio.Time.with_timeout
        (Eio.Stdenv.clock env)
        (Time_ns.Span.to_sec span)
        (fun () -> Ok (f ()))
  in
  let outcome =
    match Cancellation.protect cancel ~f:(fun () -> with_timeout wait) with
    | None -> `Cancelled
    | Some (Error `Timeout) -> `Timed_out
    | Some (Ok status) -> `Done status
  in
  (match outcome, term_grace with
   | `Cancelled, Some _ -> ()
   | (`Cancelled | `Timed_out | `Done _), _ ->
     Option.iter input ~f:Stdin_feed.finish);
  (* What escaped even the tree (a daemon reparented to init) may hold the
     pipes: the output stops being waited for after [drain_after_kill]. *)
  let kill_and_reap () =
    Process_tree.kill ~env ~root:(Pid.of_int (Eio.Process.pid child));
    ignore
      (Eio.Time.with_timeout (Eio.Stdenv.clock env) drain_after_kill (fun () ->
         Ok (drain ()))
       : (unit, [ `Timeout ]) Result.t);
    ignore (Eio.Process.await child : Eio.Process.exit_status)
  in
  (* Asked to stop: end of input and SIGTERM first, so that it may save its
     state; [kill_and_reap] whatever is left after [grace]. Once it exited
     and its output ended, nothing of it is left to kill. *)
  let terminated grace =
    Option.iter input ~f:(fun input ->
      ignore (Stdin_feed.write input "" ~close:true : unit Or_error.t));
    Signal_unix.send_i Signal.term group;
    match
      Eio.Time.with_timeout
        (Eio.Stdenv.clock env)
        (Time_ns.Span.to_sec grace)
        (fun () -> Ok (wait ()))
    with
    | Ok (_ : Eio.Process.exit_status) -> true
    | Error `Timeout -> false
  in
  let exit =
    match outcome with
    | `Cancelled ->
      if not (Option.value_map term_grace ~default:false ~f:terminated)
      then kill_and_reap ();
      Exit.Cancelled
    | `Timed_out ->
      kill_and_reap ();
      Timed_out
    | `Done (`Exited n) -> Exited n
    | `Done (`Signaled n) -> Signaled (Signal.of_caml_int n)
  in
  Option.iter input ~f:Stdin_feed.finish;
  reaped := true;
  exit
;;

module Output = struct
  type t =
    { exit : Exit.t
    ; stdout : string
    ; stderr : string
    }
  [@@deriving sexp_of]
end

let run_collect
      ~env
      ?cwd
      ?extra_env
      ?unset_env
      ?stdin
      ?timeout
      ?cancel
      ~prog
      ~args
      ()
  =
  let stdout = Buffer.create 256 in
  let stderr = Buffer.create 256 in
  let exit =
    run
      ~env
      ?cwd
      ?extra_env
      ?unset_env
      ?stdin
      ?timeout
      ?cancel
      ~on_stdout:(Buffer.add_string stdout)
      ~on_stderr:(Buffer.add_string stderr)
      ~prog
      ~args
      ()
  in
  { Output.exit
  ; stdout = Buffer.contents stdout
  ; stderr = Buffer.contents stderr
  }
;;
