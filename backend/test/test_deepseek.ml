open! Core
open! Prigh
open Eio.Std
module Server = Fake_http_server
module T = Deepseek.For_testing
module Json = Jsonaf

let model = Model.default

let request ?(thinking = Thinking.Off) ?(tools = []) ?system messages =
  { Provider.Request.model
  ; system
  ; messages
  ; tools
  ; thinking
  ; max_tokens = None
  }
;;

let%expect_test
    "request body: system, user, assistant with thinking and tool calls, tool \
     result, tools, thinking level"
  =
  let messages =
    [ Message.user "list files"
    ; Assistant
        { content =
            [ Content.thinking "I should list."
            ; Text "Listing."
            ; Tool_call
                { id = "call_1"; name = "bash"; arguments = "{\"cmd\":\"ls\"}" }
            ]
        ; stop_reason = Tool_use
        ; usage = Usage.zero
        ; model = model.id
        }
    ; Tool_result
        { tool_call_id = "call_1"
        ; tool_name = "bash"
        ; text = "a\nb"
        ; is_error = false
        }
    ]
  in
  let tools =
    [ { Tool_spec.name = "bash"
      ; description = "Run a command"
      ; parameters =
          `Object
            [ "type", `String "object"
            ; ( "properties"
              , `Object [ "cmd", `Object [ "type", `String "string" ] ] )
            ; "required", `Array [ `String "cmd" ]
            ]
      }
    ]
  in
  print_endline
    (Json.to_string_hum
       (T.request_body
          (request
             ~system:"Be terse."
             ~tools
             ~thinking:(On (Some High))
             messages)));
  [%expect
    {|
    {
      "model": "deepseek-flash",
      "messages": [
        {
          "role": "system",
          "content": "Be terse."
        },
        {
          "role": "user",
          "content": "list files"
        },
        {
          "role": "assistant",
          "content": "Listing.",
          "reasoning_content": "I should list.",
          "tool_calls": [
            {
              "id": "call_1",
              "type": "function",
              "function": {
                "name": "bash",
                "arguments": "{\"cmd\":\"ls\"}"
              }
            }
          ]
        },
        {
          "role": "tool",
          "tool_call_id": "call_1",
          "content": "a\nb"
        }
      ],
      "stream": true,
      "stream_options": {
        "include_usage": true
      },
      "tools": [
        {
          "type": "function",
          "function": {
            "name": "bash",
            "description": "Run a command",
            "parameters": {
              "type": "object",
              "properties": {
                "cmd": {
                  "type": "string"
                }
              },
              "required": [
                "cmd"
              ]
            }
          }
        }
      ],
      "thinking": {
        "type": "enabled"
      },
      "reasoning_effort": "high"
    }
    |}]
;;

let%expect_test "request body: thinking off, max_tokens" =
  let body =
    T.request_body
      { (request [ Message.user "hi" ]) with max_tokens = Some 100 }
  in
  print_endline (Json.to_string_hum body);
  [%expect
    {|
    {
      "model": "deepseek-flash",
      "messages": [
        {
          "role": "user",
          "content": "hi"
        }
      ],
      "stream": true,
      "stream_options": {
        "include_usage": true
      },
      "max_tokens": 100,
      "thinking": {
        "type": "disabled"
      }
    }
    |}]
;;

let parse s =
  print_s [%sexp (T.parse_chunk (Json.of_string s) : T.Chunk.t Or_error.t)]
;;

let%expect_test "parse_chunk" =
  parse {|{"choices":[{"delta":{"role":"assistant","content":""}}]}|};
  [%expect {| (Ok ((events ()) (finish_reason ()) (usage ()))) |}];
  parse {|{"choices":[{"delta":{"content":"Hel"},"finish_reason":null}]}|};
  [%expect
    {| (Ok ((events ((Text_delta Hel))) (finish_reason ()) (usage ()))) |}];
  parse {|{"choices":[{"delta":{"reasoning_content":"hmm","content":null}}]}|};
  [%expect
    {| (Ok ((events ((Thinking_delta hmm))) (finish_reason ()) (usage ()))) |}];
  parse
    {|{"choices":[{"delta":{"tool_calls":[{"index":0,"id":"call_x","type":"function","function":{"name":"read","arguments":""}}]}}]}|};
  [%expect
    {|
    (Ok
     ((events ((Tool_call_start (index 0) (id call_x) (name read))))
      (finish_reason ()) (usage ())))
    |}];
  parse
    {|{"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\"pa"}},{"index":1,"id":"call_y","function":{"name":"ls","arguments":"{}"}}]}}]}|};
  [%expect
    {|
    (Ok
     ((events
       ((Tool_call_delta (index 0) (arguments "{\"pa"))
        (Tool_call_start (index 1) (id call_y) (name ls))
        (Tool_call_delta (index 1) (arguments {}))))
      (finish_reason ()) (usage ())))
    |}];
  parse
    {|{"choices":[{"delta":{},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":10,"completion_tokens":5,"prompt_cache_hit_tokens":8,"prompt_cache_miss_tokens":2}}|};
  [%expect
    {|
    (Ok
     ((events ()) (finish_reason (tool_calls))
      (usage (((input 10) (output 5) (cache_read 8))))))
    |}];
  parse {|{"choices":[],"usage":{"prompt_tokens":1,"completion_tokens":2}}|};
  [%expect
    {|
    (Ok
     ((events ()) (finish_reason ())
      (usage (((input 1) (output 2) (cache_read 0))))))
    |}];
  parse {|{"error":{"message":"Insufficient Balance","type":"unknown_error"}}|};
  [%expect {| (Error "Insufficient Balance") |}]
;;

let%expect_test "error_message_of_body" =
  print_endline
    (Sse_request.error_message_of_body
       ~status:401
       {|{"error":{"message":"Authentication Fails","code":"invalid_request_error"}}|});
  print_endline
    (Sse_request.error_message_of_body ~status:502 "<html>bad gateway</html>\n");
  [%expect
    {|
    HTTP 401: Authentication Fails
    HTTP 502: <html>bad gateway</html>
    |}]
;;

let sse_lines lines =
  String.concat (List.map lines ~f:(fun l -> "data: " ^ l ^ "\n\n"))
;;

let stream_with_server ~env ~sw ?(delay = 0.) ?cancel_after chunks =
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      { status = 200
      ; headers = [ "Content-Type", "text/event-stream" ]
      ; chunks = List.map chunks ~f:(fun c -> delay, c)
      })
  in
  let provider =
    Deepseek.create ~env ~base_url:(Server.url server "") ~api_key:"sk-test" ()
  in
  let cancel = Cancellation.create () in
  let events = ref [] in
  let count = ref 0 in
  let message =
    provider.stream
      (request [ Message.user "hi" ])
      ~cancel
      ~on_event:(fun e ->
        events := e :: !events;
        incr count;
        match cancel_after with
        | Some n when !count = n -> Cancellation.cancel cancel
        | _ -> ())
  in
  print_s [%sexp (List.rev !events : Assistant_event.t list)];
  print_s [%sexp (message : Message.Assistant.t)];
  server
;;

let%expect_test "stream: text with thinking, usage and stop" =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let server =
    stream_with_server
      ~env
      ~sw
      [ sse_lines
          [ {|{"choices":[{"delta":{"reasoning_content":"think"}}]}|}
          ; {|{"choices":[{"delta":{"content":"Hel"}}]}|}
          ]
      ; sse_lines
          [ {|{"choices":[{"delta":{"content":"lo"},"finish_reason":"stop"}]}|}
          ; {|{"choices":[],"usage":{"prompt_tokens":7,"completion_tokens":3,"prompt_cache_hit_tokens":0}}|}
          ; "[DONE]"
          ]
      ]
  in
  [%expect
    {|
    ((Thinking_delta think) (Text_delta Hel) (Text_delta lo))
    ((content ((Thinking ((text think) (signature ()))) (Text Hello)))
     (stop_reason End_turn) (usage ((input 7) (output 3) (cache_read 0)))
     (model deepseek/deepseek-flash))
    |}];
  let request = List.hd_exn (Server.requests server) in
  print_s
    [%sexp
      { request_line = (request.request_line : string)
      ; authorization =
          (Server.Request.header request "authorization" : string option)
      ; body = (Json.of_string request.body : Json.t)
      }];
  [%expect
    {|
    ((request_line "POST /chat/completions HTTP/1.1")
     (authorization ("Bearer sk-test"))
     (body
      (Object
       ((model (String deepseek-flash))
        (messages
         (Array ((Object ((role (String user)) (content (String hi)))))))
        (stream True) (stream_options (Object ((include_usage True))))
        (thinking (Object ((type (String disabled)))))))))
    |}]
;;

let%expect_test "stream: tool calls split across chunks" =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let (_ : Server.t) =
    stream_with_server
      ~env
      ~sw
      [ sse_lines
          [ {|{"choices":[{"delta":{"content":"Let me look."}}]}|}
          ; {|{"choices":[{"delta":{"tool_calls":[{"index":0,"id":"c1","type":"function","function":{"name":"read","arguments":""}}]}}]}|}
          ; {|{"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\"path\":"}}]}}]}|}
          ]
      ; sse_lines
          [ {|{"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\"a.txt\"}"}}]}}]}|}
          ; {|{"choices":[{"delta":{"tool_calls":[{"index":1,"id":"c2","type":"function","function":{"name":"ls","arguments":"{}"}}]}}]}|}
          ; {|{"choices":[{"delta":{},"finish_reason":"tool_calls"}]}|}
          ; "[DONE]"
          ]
      ]
  in
  [%expect
    {|
    ((Text_delta "Let me look.") (Tool_call_start (index 0) (id c1) (name read))
     (Tool_call_delta (index 0) (arguments "{\"path\":"))
     (Tool_call_delta (index 0) (arguments "\"a.txt\"}"))
     (Tool_call_start (index 1) (id c2) (name ls))
     (Tool_call_delta (index 1) (arguments {})))
    ((content
      ((Text "Let me look.")
       (Tool_call ((id c1) (name read) (arguments "{\"path\":\"a.txt\"}")))
       (Tool_call ((id c2) (name ls) (arguments {})))))
     (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
     (model deepseek/deepseek-flash))
    |}]
;;

let%expect_test "stream: length finish reason and mid-stream API error" =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let (_ : Server.t) =
    stream_with_server
      ~env
      ~sw
      [ sse_lines
          [ {|{"choices":[{"delta":{"content":"partial"},"finish_reason":"length"}]}|}
          ; "[DONE]"
          ]
      ]
  in
  [%expect
    {|
    ((Text_delta partial))
    ((content ((Text partial))) (stop_reason Length)
     (usage ((input 0) (output 0) (cache_read 0)))
     (model deepseek/deepseek-flash))
    |}];
  let (_ : Server.t) =
    stream_with_server
      ~env
      ~sw
      [ sse_lines
          [ {|{"choices":[{"delta":{"content":"a"}}]}|}
          ; {|{"error":{"message":"boom"}}|}
          ]
      ]
  in
  [%expect
    {|
    ((Text_delta a))
    ((content ((Text a))) (stop_reason (Error boom))
     (usage ((input 0) (output 0) (cache_read 0)))
     (model deepseek/deepseek-flash))
    |}];
  let (_ : Server.t) = stream_with_server ~env ~sw [ "data: {not json\n\n" ] in
  [%expect
    {|
    ()
    ((content ())
     (stop_reason (Error "bad JSON in stream: json > object: char '}'"))
     (usage ((input 0) (output 0) (cache_read 0)))
     (model deepseek/deepseek-flash))
    |}]
;;

let%expect_test "stream: HTTP error status" =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      Server.Reply.simple 401 {|{"error":{"message":"Authentication Fails"}}|})
  in
  let provider =
    Deepseek.create ~env ~base_url:(Server.url server "") ~api_key:"bad" ()
  in
  let message =
    provider.stream
      (request [ Message.user "hi" ])
      ~cancel:Cancellation.never
      ~on_event:ignore
  in
  print_s [%sexp (message.stop_reason : Stop_reason.t)];
  [%expect {| (Error "HTTP 401: Authentication Fails") |}]
;;

let%expect_test "stream: connection failure" =
  Eio_main.run
  @@ fun env ->
  let provider =
    Deepseek.create ~env ~base_url:"http://127.0.0.1:1" ~api_key:"k" ()
  in
  let message =
    provider.stream
      (request [ Message.user "hi" ])
      ~cancel:Cancellation.never
      ~on_event:ignore
  in
  (match message.stop_reason with
   | Error e -> print_endline (String.prefix e 18)
   | _ -> print_s [%sexp (message.stop_reason : Stop_reason.t)]);
  [%expect {| connection failed: |}]
;;

let%expect_test "stream: cancellation keeps partial content" =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let (_ : Server.t) =
    stream_with_server
      ~env
      ~sw
      ~delay:0.05
      ~cancel_after:1
      [ sse_lines [ {|{"choices":[{"delta":{"content":"first"}}]}|} ]
      ; sse_lines [ {|{"choices":[{"delta":{"content":"second"}}]}|} ]
      ; sse_lines [ {|{"choices":[{"delta":{"content":"third"}}]}|} ]
      ]
  in
  [%expect
    {|
    ((Text_delta first))
    ((content ((Text first))) (stop_reason Aborted)
     (usage ((input 0) (output 0) (cache_read 0)))
     (model deepseek/deepseek-flash))
    |}]
;;
