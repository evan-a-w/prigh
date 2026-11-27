open! Core
open! Import

let exit_of (ended : Host_stream.Ended.t) : Line_transport.Exit.t =
  match ended with
  | Exited { exit = Exited code; stderr_tail; _ } ->
    { code = Some code; stderr_tail }
  | Exited { stderr_tail; _ } -> { code = None; stderr_tail }
  | Not_installed _ | Old_host _ | Host_gone _ | Broken _ | Stopped ->
    { code = None; stderr_tail = Host_stream.Ended.to_message ended }
;;

(* The MCP side channel: supports both legacy unrouted connections (which
   wait for [serve]) and per-session routed connections (announced by [Open],
   dispatched by route id without cross-talk). Each line is answered in a
   fiber of its own, until the process ends. *)
module Mcp_side = struct
  type t =
    { events : Host_stream.Mcp_event.t Eio.Stream.t
    ; tools : Injected_tools.t Promise.t
    ; set_tools : Injected_tools.t Promise.u
    ; routes : Injected_tools.t String.Table.t
    ; conn_route : string Int.Table.t
    }

  let create () =
    let tools, set_tools = Promise.create () in
    { events = Eio.Stream.create Int.max_value
    ; tools
    ; set_tools
    ; routes = String.Table.create ()
    ; conn_route = Int.Table.create ()
    }
  ;;

  let on_mcp t event = Eio.Stream.add t.events event

  let serve t injected =
    if not (Promise.is_resolved t.tools)
    then Promise.resolve t.set_tools injected
  ;;

  let register_route t ~stream ~base_bridge route injected =
    Hashtbl.set t.routes ~key:route ~data:injected;
    let bridge = Injected_tools.Bridge.with_route base_bridge ~route in
    let release () =
      Hashtbl.remove t.routes route;
      let to_close =
        Hashtbl.fold t.conn_route ~init:[] ~f:(fun ~key:conn ~data:r acc ->
          if String.equal r route then conn :: acc else acc)
      in
      (* Keep [conn_route] until [Closed conn] arrives so any [Line] already
         queued on [t.events] still sees the closed route instead of falling
         through to the legacy unrouted handler. *)
      List.iter to_close ~f:(fun conn -> Host_stream.mcp_close stream ~conn)
    in
    bridge, release
  ;;

  let run t ~sw stream =
    Guarded.fork ~sw ~what:"MCP side channel" (fun () ->
      Fiber.first
        (fun () ->
           ignore
             (Promise.await (Host_stream.ended stream) : Host_stream.Ended.t))
        (fun () ->
           Switch.run (fun mcp_sw ->
             let dispatch ~conn injected line =
               Guarded.fork ~sw:mcp_sw ~what:"MCP tool call" (fun () ->
                 Option.iter
                   (Injected_tools.handle_line injected line)
                   ~f:(fun reply ->
                     ignore
                       (Host_stream.mcp_write stream ~conn reply
                        : unit Or_error.t)))
             in
             let rec loop () =
               match Eio.Stream.take t.events with
               | Open { conn; route } ->
                 Hashtbl.set t.conn_route ~key:conn ~data:route;
                 if not (Hashtbl.mem t.routes route)
                 then Host_stream.mcp_close stream ~conn;
                 loop ()
               | Closed conn ->
                 Hashtbl.remove t.conn_route conn;
                 loop ()
               | Line { conn; line } ->
                 (match Hashtbl.find t.conn_route conn with
                  | Some route ->
                    (match Hashtbl.find t.routes route with
                     | Some injected -> dispatch ~conn injected line
                     | None ->
                       (* Closed or unknown session route: never cross-talk. *)
                       Host_stream.mcp_close stream ~conn)
                  | None ->
                    Guarded.fork ~sw:mcp_sw ~what:"MCP tool call" (fun () ->
                      let injected = Promise.await t.tools in
                      Option.iter
                        (Injected_tools.handle_line injected line)
                        ~f:(fun reply ->
                          ignore
                            (Host_stream.mcp_write stream ~conn reply
                             : unit Or_error.t))));
                 loop ()
             in
             loop ())))
  ;;
end

(* Starts [arguments] and waits until it started. *)
let start ~sw ~exec ~host ?on_stderr (arguments : Host_process.Arguments.t) =
  let side = Option.some_if arguments.mcp (Mcp_side.create ()) in
  let s =
    Host_stream.start
      ~sw
      ~exec
      ~host
      ?on_stderr
      ?on_mcp:(Option.map side ~f:Mcp_side.on_mcp)
      arguments
  in
  match Promise.await (Host_stream.started s) with
  | exception exn ->
    (* The turn was cancelled while it started: it would run for nobody. *)
    Host_stream.stop s;
    raise exn
  | Error ended -> Error (Error.of_string (Host_stream.Ended.to_message ended))
  | Ok started -> Ok (side, s, started)
;;

let secret_notice ~host ~program ~env =
  sprintf
    "%s's prigh cannot hand %s prigh's login privately, so it has it in %s, \
     which every command it runs inherits: update prigh on %s to keep it out."
    host
    program
    env
    host
;;

let spawn ~sw ~exec ~host ?on_stderr (launch : Adapter.Launch.t) =
  let arguments =
    { (Host_process.Arguments.create ~argv:launch.argv) with
      env = launch.env_set
    ; env_unset = launch.env_unset
    ; mcp = launch.mcp
    ; secret =
        Option.map launch.secret ~f:(fun { fd_env; value; env = _ } ->
          { Host_process.Secret.fd_env; value })
    }
  in
  (* Its tail comes with the exit: what it writes before is only worth
     sending when someone reads it. *)
  let arguments =
    if Option.is_none on_stderr
    then { arguments with stderr_rate = 0 }
    else arguments
  in
  let start = start ~sw ~exec ~host ?on_stderr in
  let started =
    match start arguments, launch.secret with
    | Ok (_, s, started), Some { env; value; fd_env = _ }
      when not started.secret ->
      (* A host too old to hand it on a descriptor started the program
         without it: again, with it in the environment. *)
      Host_stream.stop s;
      Or_error.map
        (start
           { arguments with
             secret = None
           ; env = arguments.env @ [ env, value ]
           })
        ~f:(fun started ->
          ( started
          , Some
              (secret_notice
                 ~host
                 ~program:(Filename.basename (List.hd_exn launch.argv))
                 ~env) ))
    | started, _ -> Or_error.map started ~f:(fun started -> started, None)
  in
  match started with
  | Error _ as e -> e
  | Ok ((side, s, started), notice) ->
    let mcp =
      match side, started.mcp_socket, started.mcp_token with
      | Some side, Some socket, Some token ->
        Mcp_side.run side ~sw s;
        let bridge =
          Injected_tools.Bridge.create ~prigh:started.prigh ~socket ~token
        in
        let route =
          if started.mcp_routes
          then Some (Mcp_side.register_route side ~stream:s ~base_bridge:bridge)
          else None
        in
        Some { Line_transport.Mcp.bridge; serve = Mcp_side.serve side; route }
      | _ -> None
    in
    Ok
      (Line_transport.create
         ~read_line:(fun () -> Host_stream.read_line s)
         ~write_line:(fun line ->
           match Host_stream.write_line s line with
           | Ok () -> ()
           | Error _ when Promise.is_resolved (Host_stream.ended s) -> ()
           | Error e -> Error.raise e)
         ~stop:(fun () -> Host_stream.stop s)
         ~ended:(fun () -> exit_of (Promise.await (Host_stream.ended s)))
         ~is_ended:(fun () -> Promise.is_resolved (Host_stream.ended s))
         ?mcp
         ?notice
         ())
;;
