open! Core
open! Async
open Prigh_client
open Prigh_protocol

type t =
  { client : Client.t
  ; backend : string
  ; mutable worker : Transport.t Deferred.Or_error.t option
  }

let create ~client ~backend = { client; backend; worker = None }

let fail t ~exec_id text =
  don't_wait_for
    (Deferred.ignore_m
       (Client.call
          t.client
          "tool_exec_result"
          [ "exec_id", `String exec_id
          ; "text", `String text
          ; "is_error", `True
          ]))
;;

let relay t line =
  match Jsonaf.parse line with
  | Error _ -> ()
  | Ok json ->
    let field name = Jsonaf.member name json in
    (match field "type", field "exec_id" with
     | Some (`String "output"), Some (`String exec_id) ->
       let chunk =
         match field "chunk" with
         | Some (`String s) -> s
         | _ -> ""
       in
       don't_wait_for
         (Deferred.ignore_m
            (Client.call
               t.client
               "tool_exec_output"
               [ "exec_id", `String exec_id; "chunk", `String chunk ]))
     | Some (`String "result"), Some (`String exec_id) ->
       let text =
         match field "text" with
         | Some (`String s) -> s
         | _ -> ""
       in
       let is_error =
         match field "is_error" with
         | Some `True -> true
         | _ -> false
       in
       don't_wait_for
         (Deferred.ignore_m
            (Client.call
               t.client
               "tool_exec_result"
               [ "exec_id", `String exec_id
               ; "text", `String text
               ; ("is_error", if is_error then `True else `False)
               ]))
     | _ -> ())
;;

let worker t =
  match t.worker with
  | Some w -> w
  | None ->
    let w =
      match%map
        Stdio_transport.spawn ~prog:t.backend ~args:[ "tool-host" ] ()
      with
      | Error _ as e ->
        t.worker <- None;
        e
      | Ok transport ->
        don't_wait_for (Pipe.iter_without_pushback transport.lines ~f:(relay t));
        don't_wait_for (Pipe.drain transport.stderr_lines);
        upon transport.closed (fun () -> t.worker <- None);
        Ok transport
    in
    t.worker <- Some w;
    w
;;

let send t json =
  don't_wait_for
    (match%map worker t with
     | Ok transport -> transport.send_line (Jsonaf.to_string json)
     | Error e ->
       (match Jsonaf.member "exec_id" json with
        | Some (`String exec_id) ->
          fail t ~exec_id ("cannot start tool host: " ^ Error.to_string_hum e)
        | _ -> ()))
;;

let handle t (event : Event.t) =
  match event with
  | Tool_exec { exec_id; call_id = _; name; arguments; cwd } ->
    send
      t
      (`Object
        [ "type", `String "exec"
        ; "exec_id", `String exec_id
        ; "name", `String name
        ; "arguments", arguments
        ; "cwd", `String cwd
        ])
  | Tool_exec_cancel exec_id ->
    send t (`Object [ "type", `String "cancel"; "exec_id", `String exec_id ])
  | _ -> ()
;;

let close t =
  match t.worker with
  | None -> ()
  | Some w ->
    upon w (function
      | Ok transport -> transport.close ()
      | Error _ -> ())
;;
