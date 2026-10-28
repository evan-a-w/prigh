open! Core
open! Import

let delta_kind : Assistant_event.t -> string = function
  | Text_delta _ -> "text"
  | Thinking_delta _ | Thinking_signature _ -> "thinking"
  | Tool_call_start _ | Tool_call_delta _ -> "tool_call"
  | Server_tool_use _ | Server_tool_result _ -> "server_tool"
;;

let stream (provider : Provider.t) (request : Provider.Request.t) ~cancel ~on_event
  =
  match
    Trace.start
      "model_request"
      ~phase:"connect"
      ~attrs:
        [ "model", String (Model.key request.model)
        ; "thinking", String (Thinking.to_string request.thinking)
        ]
  with
  | None -> provider.stream request ~cancel ~on_event
  | Some _ as span ->
    let detailed = Trace.enabled ~detailed:true () in
    let deltas = ref 0 in
    let last = ref None in
    let on_event delta =
      incr deltas;
      if detailed
      then
        Option.iter !last ~f:(fun at ->
          Trace.finish
            (Trace.start
               ~detailed:true
               ~at
               ~attrs:[ "kind", String (delta_kind delta) ]
               "delta_wait"));
      (match delta_kind delta with
       | "thinking" -> Trace.phase "thinking"
       | _ -> Trace.phase "stream");
      if detailed
      then (
        Trace.span ~detailed:true "delta_handle" (fun () -> on_event delta);
        last := Trace.now ())
      else on_event delta
    in
    (match
       Trace.within span (fun () -> provider.stream request ~cancel ~on_event)
     with
     | exception exn ->
       Trace.finish
         span
         ~status:
           (match exn with
            | Eio.Cancel.Cancelled _ -> Cancelled
            | _ -> Error);
       raise exn
     | (reply : Message.Assistant.t) ->
       let usage = reply.usage in
       let status, stop =
         match reply.stop_reason with
         | End_turn -> Trace.Status.Ok, [ "stop", Trace.Value.String "end_turn" ]
         | Tool_use -> Ok, [ "stop", String "tool_use" ]
         | Length -> Ok, [ "stop", String "length" ]
         | Aborted -> Cancelled, [ "stop", String "aborted" ]
         | Error e -> Error, [ "stop", String "error"; "error", Trace.short e ]
       in
       Trace.finish
         span
         ~status
         ~attrs:
           ([ "input_tokens", Trace.Value.Int usage.input
            ; "output_tokens", Int usage.output
            ; "cache_read_tokens", Int usage.cache_read
            ; "cost_usd", Float (Model.cost_usd request.model usage)
            ; "deltas", Int !deltas
            ]
            @ stop);
       reply)
;;
