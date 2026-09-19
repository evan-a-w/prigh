open! Core
open! Prigh
open Eio.Std
module Http = Http_client
module Server = Fake_http_server

let run f = Eio_main.run @@ fun env -> Switch.run @@ fun sw -> f ~env ~sw

let show_request (r : Server.Request.t) =
  print_s
    [%sexp
      { request_line = (r.request_line : string)
      ; content_type = (Server.Request.header r "content-type" : string option)
      ; authorization =
          (Server.Request.header r "authorization" : string option)
      ; body = (r.body : string)
      }]
;;

let%expect_test
    "post: request is forwarded, response status/headers/body returned"
  =
  run
  @@ fun ~env ~sw ->
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      Server.Reply.simple ~headers:[ "X-Thing", "42" ] 200 "hello body")
  in
  let result =
    Http.post
      ~env
      ~url:(Server.url server "/v1/chat")
      ~headers:
        [ "Content-Type", "application/json"
        ; "Authorization", "Bearer sk-\"quoted\"\\slash"
        ]
      ~body:"{\"x\":1}"
      ()
  in
  (match result with
   | Ok (response, body) ->
     print_s
       [%sexp
         { status = (response.status : int)
         ; x_thing = (Http.Response.header response "x-thing" : string option)
         ; body : string
         }]
   | Error e -> print_s [%sexp (e : Http.Error.t)]);
  List.iter (Server.requests server) ~f:show_request;
  [%expect
    {|
    ((status 200) (x_thing (42)) (body "hello body"))
    ((request_line "POST /v1/chat HTTP/1.1") (content_type (application/json))
     (authorization ("Bearer sk-\"quoted\"\\slash")) (body "{\"x\":1}"))
    |}]
;;

let%expect_test "post_stream: chunks arrive incrementally, headers first" =
  run
  @@ fun ~env ~sw ->
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      { status = 200
      ; headers = [ "Content-Type", "text/event-stream" ]
      ; chunks = [ 0., "data: a\n\n"; 0.05, "data: b\n\n"; 0.05, "data: c\n\n" ]
      })
  in
  let events = ref [] in
  let sse = Sse.create () in
  let result =
    Http.post_stream
      ~env
      ~url:(Server.url server "/stream")
      ~headers:[]
      ~body:""
      ~on_response:(fun r ->
        events := sprintf "response %d" r.status :: !events)
      ~on_chunk:(fun chunk ->
        List.iter (Sse.feed sse chunk) ~f:(fun e ->
          events := ("event " ^ e.data) :: !events))
      ()
  in
  print_s [%sexp (result : (Http.Response.t, Http.Error.t) Result.t)];
  print_s [%sexp (List.rev !events : string list)];
  [%expect
    {|
    (Ok
     ((status 200)
      (headers ((Connection close) (Content-Type text/event-stream)))))
    ("response 200" "event a" "event b" "event c")
    |}]
;;

let%expect_test "non-2xx is not an error at this layer; body is still delivered"
  =
  run
  @@ fun ~env ~sw ->
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      Server.Reply.simple 429 "{\"error\":\"rate limited\"}")
  in
  let result =
    Http.post ~env ~url:(Server.url server "/") ~headers:[] ~body:"" ()
  in
  print_s [%sexp (result : (Http.Response.t * string, Http.Error.t) Result.t)];
  [%expect
    {|
    (Ok
     (((status 429) (headers ((Connection close))))
      "{\"error\":\"rate limited\"}"))
    |}]
;;

let%expect_test "connection refused" =
  run
  @@ fun ~env ~sw:_ ->
  let result =
    Http.post ~env ~url:"http://127.0.0.1:1/" ~headers:[] ~body:"" ()
  in
  (match result with
   | Error (Connection_failed msg) ->
     print_s
       [%sexp
         (String.is_substring (String.lowercase msg) ~substring:"refused"
          : bool)]
   | _ ->
     print_s
       [%sexp (result : (Http.Response.t * string, Http.Error.t) Result.t)]);
  [%expect {| true |}]
;;

let%expect_test "cancellation mid-stream" =
  run
  @@ fun ~env ~sw ->
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      { status = 200; headers = []; chunks = [ 0., "first"; 5., "never" ] })
  in
  let cancel = Cancellation.create () in
  let chunks = ref [] in
  let result =
    Http.post_stream
      ~env
      ~cancel
      ~url:(Server.url server "/")
      ~headers:[]
      ~body:""
      ~on_chunk:(fun c ->
        chunks := c :: !chunks;
        Cancellation.cancel cancel)
      ()
  in
  print_s [%sexp (result : (Http.Response.t, Http.Error.t) Result.t)];
  print_s [%sexp (List.rev !chunks : string list)];
  [%expect
    {|
    (Error Cancelled)
    (first)
    |}]
;;

let%expect_test "timeout" =
  run
  @@ fun ~env ~sw ->
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      { status = 200; headers = []; chunks = [ 0., "first"; 5., "never" ] })
  in
  let result =
    Http.post
      ~env
      ~timeout:(Time_ns.Span.of_ms 300.)
      ~url:(Server.url server "/")
      ~headers:[]
      ~body:""
      ()
  in
  print_s [%sexp (result : (Http.Response.t * string, Http.Error.t) Result.t)];
  [%expect {| (Error Timed_out) |}]
;;

let%expect_test "error messages" =
  List.iter
    ~f:(fun e -> print_endline (Http.Error.to_string e))
    [ Http.Error.Connection_failed "Eio.Io Net Connection_failure Refused"
    ; Cancelled
    ; Timed_out
    ];
  [%expect
    {|
    connection failed: Eio.Io Net Connection_failure Refused
    cancelled
    timed out
    |}]
;;
