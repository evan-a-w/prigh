open! Core
open! Async
open Prigh_client
open Prigh_protocol

type t =
  { client : Client.t
  ; spawn : unit -> Transport.t Deferred.Or_error.t
  ; mutable worker : Transport.t Deferred.Or_error.t option
  ; sends : unit Sequencer.t
  }

let new_host_id () =
  let state = Random.State.make_self_init ~allow_in_tests:true () in
  "host-"
  ^ String.init 16 ~f:(fun _ -> "0123456789abcdef".[Random.State.int state 16])
;;

let spawn_worker ~backend () =
  Stdio_transport.spawn ~prog:backend ~args:[ "tool-host" ] ()
;;

let create ~client ~spawn =
  { client; spawn; worker = None; sends = Sequencer.create () }
;;

let call t method_ params =
  don't_wait_for (Deferred.ignore_m (Client.call t.client method_ params))
;;

let fail t ~exec_id text =
  call
    t
    "tool_exec_result"
    [ "exec_id", `String exec_id; "text", `String text; "is_error", `True ]
;;

let relay t line =
  match Jsonaf.parse line with
  | Error _ -> ()
  | Ok json ->
    let field name = Jsonaf.member name json in
    let string_field name =
      match field name with
      | Some (`String s) -> s
      | _ -> ""
    in
    (match field "type", field "exec_id", field "term_id" with
     | Some (`String "output"), Some (`String exec_id), _ ->
       call
         t
         "tool_exec_output"
         [ "exec_id", `String exec_id; "chunk", `String (string_field "chunk") ]
     | Some (`String "result"), Some (`String exec_id), _ ->
       let is_error =
         match field "is_error" with
         | Some `True -> true
         | _ -> false
       in
       let images =
         match field "images" with
         | Some (`Array (_ :: _) as images) -> [ "images", images ]
         | _ -> []
       in
       call
         t
         "tool_exec_result"
         ([ "exec_id", `String exec_id
          ; "text", `String (string_field "text")
          ; ("is_error", if is_error then `True else `False)
          ]
          @ images)
     | Some (`String "terminal_frame"), _, Some (`String term_id) ->
       call
         t
         "terminal_frame"
         [ "term_id", `String term_id
         ; "kind", `String (string_field "kind")
         ; "data", `String (string_field "data")
         ]
     | Some (`String "terminal_closed"), _, Some (`String term_id) ->
       call t "terminal_closed" [ "term_id", `String term_id ]
     | _ -> ())
;;

let worker t =
  match t.worker with
  | Some w -> w
  | None ->
    let w =
      match%map t.spawn () with
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
  @@ Throttle.enqueue t.sends (fun () ->
    match%map worker t with
    | Ok transport -> transport.send_line (Jsonaf.to_string json)
    | Error e ->
      (match Jsonaf.member "exec_id" json, Jsonaf.member "type" json with
       | Some (`String exec_id), _ ->
         fail t ~exec_id ("cannot start tool host: " ^ Error.to_string_hum e)
       | _, Some (`String "terminal_open") ->
         (match Jsonaf.member "term_id" json with
          | Some term_id -> call t "terminal_closed" [ "term_id", term_id ]
          | None -> ())
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
  | Terminal_open { term_id; key; cwd; cols; rows } ->
    send
      t
      (`Object
        [ "type", `String "terminal_open"
        ; "term_id", `String term_id
        ; "key", `String key
        ; "cwd", `String cwd
        ; "cols", `Number (Int.to_string cols)
        ; "rows", `Number (Int.to_string rows)
        ])
  | Terminal_frame { term_id; kind; data } ->
    send
      t
      (`Object
        [ "type", `String "terminal_frame"
        ; "term_id", `String term_id
        ; "kind", `String (Event.Frame_kind.to_string kind)
        ; "data", `String data
        ])
  | Terminal_close term_id ->
    send
      t
      (`Object [ "type", `String "terminal_close"; "term_id", `String term_id ])
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
