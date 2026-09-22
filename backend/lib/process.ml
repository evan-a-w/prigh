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

let run
      ~(env : Env.t)
      ?cwd
      ?(extra_env = [])
      ?(stdin = "")
      ?timeout
      ?(cancel = Cancellation.never)
      ?(on_stdout = ignore)
      ?(on_stderr = ignore)
      ~prog
      ~args
      ()
  =
  Switch.run
  @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  let stdin_r, stdin_w = Eio_unix.pipe sw in
  let stdout_r, stdout_w = Eio_unix.pipe sw in
  let stderr_r, stderr_w = Eio_unix.pipe sw in
  let process_env =
    if List.is_empty extra_env
    then None
    else
      Some
        (Array.append
           (Core_unix.environment ())
           (Array.of_list (List.map extra_env ~f:(fun (k, v) -> k ^ "=" ^ v))))
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
        [ 0, Eio_unix.Resource.fd stdin_r, `Blocking
        ; 1, Eio_unix.Resource.fd stdout_w, `Blocking
        ; 2, Eio_unix.Resource.fd stderr_w, `Blocking
        ]
      ?env:process_env
      (prog :: args)
  in
  Eio.Flow.close stdin_r;
  Eio.Flow.close stdout_w;
  Eio.Flow.close stderr_w;
  (* The child may exit without reading its input; that must not be an error. *)
  Fiber.fork ~sw (fun () ->
    (try Eio.Flow.copy_string stdin stdin_w with
     | Eio.Io _ -> ());
    Eio.Flow.close stdin_w);
  let drain () =
    Fiber.both
      (fun () -> pump stdout_r ~on_data:on_stdout)
      (fun () -> pump stderr_r ~on_data:on_stderr)
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
  let kill_and_reap () =
    Signal_unix.send_i Signal.kill (`Group (Pid.of_int (Eio.Process.pid child)));
    drain ();
    ignore (Eio.Process.await child : Eio.Process.exit_status)
  in
  match outcome with
  | `Cancelled ->
    kill_and_reap ();
    Exit.Cancelled
  | `Timed_out ->
    kill_and_reap ();
    Timed_out
  | `Done (`Exited n) -> Exited n
  | `Done (`Signaled n) -> Signaled (Signal.of_caml_int n)
;;

module Output = struct
  type t =
    { exit : Exit.t
    ; stdout : string
    ; stderr : string
    }
  [@@deriving sexp_of]
end

let run_collect ~env ?cwd ?extra_env ?stdin ?timeout ?cancel ~prog ~args () =
  let stdout = Buffer.create 256 in
  let stderr = Buffer.create 256 in
  let exit =
    run
      ~env
      ?cwd
      ?extra_env
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
