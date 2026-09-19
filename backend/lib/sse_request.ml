open! Core
open! Import

module Outcome = struct
  type t =
    | Completed
    | Aborted
    | Failed of string
  [@@deriving sexp_of]
end

let member_string name json =
  match Json.member name json with
  | Some (`String s) -> Some s
  | _ -> None
;;

let error_message_of_body ~status body =
  let detail =
    match Json.parse body with
    | Ok json ->
      (match Json.member "error" json with
       | Some err -> Option.value (member_string "message" err) ~default:body
       | None -> body)
    | Error _ -> body
  in
  sprintf "HTTP %d: %s" status (String.strip detail)
;;

let run ~env ?timeout ~cancel ~url ~headers ~body ~on_event () =
  let sse = Sse.create () in
  let status = ref 0 in
  let error_body = Buffer.create 256 in
  let on_chunk chunk =
    if !status / 100 = 2
    then List.iter (Sse.feed sse chunk) ~f:on_event
    else Buffer.add_string error_body chunk
  in
  let result =
    Http_client.post_stream
      ~env
      ?timeout
      ~cancel
      ~url
      ~headers:
        ([ "Content-Type", "application/json"; "Accept", "text/event-stream" ]
         @ headers)
      ~body
      ~on_response:(fun r -> status := r.status)
      ~on_chunk
      ()
  in
  Option.iter (Sse.finish sse) ~f:on_event;
  match result with
  | Error Cancelled -> Outcome.Aborted
  | Error e -> Failed (Http_client.Error.to_string e)
  | Ok response when response.status / 100 <> 2 ->
    Failed
      (error_message_of_body
         ~status:response.status
         (Buffer.contents error_body))
  | Ok _ -> Completed
;;
