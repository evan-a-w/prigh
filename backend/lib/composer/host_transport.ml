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

(* The MCP side channel: what arrives before the tools are given waits for
   them; each line is answered in a fiber of its own, until the process
   ends. *)
module Mcp_side = struct
  type t =
    { lines : (int * string) Eio.Stream.t
    ; tools : Injected_tools.t Promise.t
    ; set_tools : Injected_tools.t Promise.u
    }

  let create () =
    let tools, set_tools = Promise.create () in
    { lines = Eio.Stream.create Int.max_value; tools; set_tools }
  ;;

  let on_mcp t : Host_stream.Mcp_event.t -> unit = function
    | Line { conn; line } -> Eio.Stream.add t.lines (conn, line)
    | Closed _ -> ()
  ;;

  let serve t injected =
    if not (Promise.is_resolved t.tools)
    then Promise.resolve t.set_tools injected
  ;;

  let run t ~sw stream =
    Fiber.fork ~sw (fun () ->
      Fiber.first
        (fun () ->
           ignore
             (Promise.await (Host_stream.ended stream) : Host_stream.Ended.t))
        (fun () ->
           let injected = Promise.await t.tools in
           let rec loop () =
             let conn, line = Eio.Stream.take t.lines in
             Fiber.fork ~sw (fun () ->
               Option.iter
                 (Injected_tools.handle_line injected line)
                 ~f:(fun reply ->
                   ignore
                     (Host_stream.mcp_write stream ~conn reply
                      : unit Or_error.t)));
             loop ()
           in
           loop ()))
  ;;
end

let spawn ~sw ~exec ~host ?on_stderr (launch : Adapter.Launch.t) =
  let arguments =
    { (Host_process.Arguments.create ~argv:launch.argv) with
      env = launch.env_set
    ; mcp = launch.mcp
    }
  in
  let side = Option.some_if launch.mcp (Mcp_side.create ()) in
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
  | Error ended -> Error (Error.of_string (Host_stream.Ended.to_message ended))
  | Ok started ->
    let mcp =
      match side, started.mcp_socket with
      | Some side, Some socket ->
        Mcp_side.run side ~sw s;
        Some
          { Line_transport.Mcp.bridge =
              Injected_tools.Bridge.create ~prigh:started.prigh ~socket
          ; serve = Mcp_side.serve side
          }
      | _ -> None
    in
    Ok
      (Line_transport.create
         ~read_line:(fun () -> Host_stream.read_line s)
         ~write_line:(fun line ->
           ignore (Host_stream.write_line s line : unit Or_error.t))
         ~close_stdin:(fun () -> Host_stream.close_stdin s)
         ~interrupt:(fun () -> Host_stream.interrupt s)
         ~stop:(fun () -> Host_stream.stop s)
         ~ended:(fun () -> exit_of (Promise.await (Host_stream.ended s)))
         ~is_ended:(fun () -> Promise.is_resolved (Host_stream.ended s))
         ?mcp
         ())
;;
