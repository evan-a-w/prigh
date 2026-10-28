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
let error_message_of_body ?(limit_rejected = false) ?resets_at ~status body =
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
    (if exhausted
     then
       " "
       ^ Usage_limit.marker
       ^ Option.value_map resets_at ~default:"" ~f:Usage_limit.reset_note
     else "")
;;

let default_idle_timeout = Time_ns.Span.of_min 5.

let run
      ~env
      ?timeout
      ?(idle_timeout = default_idle_timeout)
      ~cancel
      ~url
      ~headers
      ~body
      ~on_event
      ()
  =
  let sse = Sse.create () in
  let status = ref 0 in
  let error_body = Buffer.create 256 in
  let last_chunk = ref None in
  let chunk_gap chunk =
    if Trace.enabled ~detailed:true ()
    then (
      Option.iter !last_chunk ~f:(fun at ->
        Trace.finish
          (Trace.start
             ~detailed:true
             ~at
             "chunk_gap"
             ~attrs:[ "bytes", Int (String.length chunk) ]));
      last_chunk := Trace.now ())
  in
  let on_chunk chunk =
    chunk_gap chunk;
    if !status / 100 = 2
    then
      List.iter
        (Trace.span
           ~detailed:true
           "sse_parse"
           ~attrs:[ "bytes", Int (String.length chunk) ]
           (fun () -> Sse.feed sse chunk))
        ~f:on_event
    else Buffer.add_string error_body chunk
  in
  Trace.add_attrs
    ~under:"model_request"
    [ "request_bytes", Int (String.length body) ];
  let result =
    Http_client.post_stream
      ~env
      ?timeout
      ~idle_timeout
      ~cancel
      ~url
      ~headers:
        ([ "Content-Type", "application/json"; "Accept", "text/event-stream" ]
         @ headers)
      ~body
      ~on_response:(fun r ->
        status := r.status;
        last_chunk := Trace.now ();
        Trace.add_attrs ~under:"model_request" [ "http_status", Int r.status ];
        Trace.phase "first_token")
      ~on_chunk
      ()
  in
  Option.iter (Sse.finish sse) ~f:on_event;
  match result with
  | Error Cancelled -> Outcome.Aborted
  | Error e -> Failed (Http_client.Error.to_string e)
  | Ok response when response.status / 100 <> 2 ->
    let body = Buffer.contents error_body in
    Failed
      (error_message_of_body
         ?resets_at:
           (Usage_limit.reset_time
              ~now:(Eio.Time.now (Eio.Stdenv.clock env))
              ~headers:response.headers
              ~body)
         ~limit_rejected:
           (Option.equal
              String.equal
              (Http_client.Response.header
                 response
                 "anthropic-ratelimit-unified-status")
              (Some "rejected"))
         ~status:response.status
         body)
  | Ok _ -> Completed
;;
