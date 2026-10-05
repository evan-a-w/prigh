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

let quota_codes =
  [ "usage_limit_reached"; "insufficient_quota"; "billing_hard_limit_reached" ]
;;

(* The message, plus [Usage_limit.marker] when the error's type or code (or,
   for Anthropic's subscriptions, [limit_rejected]) says the allowance is used
   up rather than briefly rate limited. *)
let error_message_of_body ?(limit_rejected = false) ~status body =
  let detail, code =
    match Json.parse body with
    | Ok json ->
      (match Json.member "error" json with
       | Some err ->
         ( Option.value (member_string "message" err) ~default:body
         , List.find_map [ "type"; "code" ] ~f:(fun key ->
             member_string key err) )
       | None -> body, None)
    | Error _ -> body, None
  in
  let exhausted =
    limit_rejected
    || Option.value_map code ~default:false ~f:(fun code ->
      List.mem quota_codes code ~equal:String.equal)
  in
  sprintf
    "HTTP %d: %s%s"
    status
    (String.strip detail)
    (if exhausted then " " ^ Usage_limit.marker else "")
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
         ~limit_rejected:
           (Option.equal
              String.equal
              (Http_client.Response.header
                 response
                 "anthropic-ratelimit-unified-status")
              (Some "rejected"))
         ~status:response.status
         (Buffer.contents error_body))
  | Ok _ -> Completed
;;
