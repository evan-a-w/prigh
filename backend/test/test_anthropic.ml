open! Core
open! Prigh
open Eio.Std
module Server = Fake_http_server
module T = Anthropic.For_testing
module Json = Jsonaf

let model = Model.default_for Anthropic

let tools =
  [ { Tool_spec.name = "bash"
    ; parallel_safe = false
    ; destructive = true
    ; description = "Run a command"
    ; parameters =
        `Object
          [ "type", `String "object"
          ; ( "properties"
            , `Object [ "cmd", `Object [ "type", `String "string" ] ] )
          ]
    }
  ; { Tool_spec.name = "subagent"
    ; parallel_safe = true
    ; destructive = false
    ; description = "Delegate"
    ; parameters = `Object [ "type", `String "object" ]
    }
  ]
;;

let request
      ?(thinking = Thinking.Off)
      ?(tools = tools)
      ?system
      ?max_tokens
      messages
  =
  { Provider.Request.model; system; messages; tools; thinking; max_tokens }
;;

let conversation =
  [ Message.user "list files"
  ; Assistant
      { content =
          [ Content.thinking ~signature:"sig-1" "I should list."
          ; Text ""
          ; Text "Listing."
          ; Tool_call
              { id = "toolu_1"; name = "bash"; arguments = {|{"cmd":"ls"}|} }
          ; Tool_call { id = "toolu_2"; name = "subagent"; arguments = "" }
          ]
      ; stop_reason = Tool_use
      ; usage = Usage.zero
      ; model = Model.key model
      }
  ; Tool_result
      { tool_call_id = "toolu_1"
      ; tool_name = "bash"
      ; text = "a\nb"
      ; is_error = false
      }
  ; Tool_result
      { tool_call_id = "toolu_2"
      ; tool_name = "subagent"
      ; text = ""
      ; is_error = true
      }
  ; Assistant
      { content =
          [ Content.thinking ~signature:"other-provider-sig" "hmm"
          ; Text "Done."
          ]
      ; stop_reason = End_turn
      ; usage = Usage.zero
      ; model = "deepseek/deepseek-flash"
      }
  ; Message.user "thanks"
  ; Message.user "and again"
  ]
;;

let%expect_test
    "request body with an API key: merged turns, cache breakpoints, thinking \
     replay"
  =
  print_endline
    (Json.to_string_hum
       (T.request_body
          ~oauth:false
          (request ~system:"Be brief." ~thinking:(On (Some High)) conversation)));
  [%expect
    {|
    {
      "model": "claude-opus-4-6",
      "max_tokens": 128000,
      "stream": true,
      "system": [
        {
          "type": "text",
          "text": "Be brief.",
          "cache_control": {
            "type": "ephemeral"
          }
        }
      ],
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "list files"
            }
          ]
        },
        {
          "role": "assistant",
          "content": [
            {
              "type": "thinking",
              "thinking": "I should list.",
              "signature": "sig-1"
            },
            {
              "type": "text",
              "text": "Listing."
            },
            {
              "type": "tool_use",
              "id": "toolu_1",
              "name": "bash",
              "input": {
                "cmd": "ls"
              }
            },
            {
              "type": "tool_use",
              "id": "toolu_2",
              "name": "subagent",
              "input": {}
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "tool_result",
              "tool_use_id": "toolu_1",
              "content": "a\nb",
              "is_error": false
            },
            {
              "type": "tool_result",
              "tool_use_id": "toolu_2",
              "content": "(no output)",
              "is_error": true
            }
          ]
        },
        {
          "role": "assistant",
          "content": [
            {
              "type": "text",
              "text": "Done."
            }
          ]
        },
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "thanks"
            },
            {
              "type": "text",
              "text": "and again",
              "cache_control": {
                "type": "ephemeral"
              }
            }
          ]
        }
      ],
      "tools": [
        {
          "name": "bash",
          "description": "Run a command",
          "input_schema": {
            "type": "object",
            "properties": {
              "cmd": {
                "type": "string"
              }
            }
          }
        },
        {
          "name": "subagent",
          "description": "Delegate",
          "input_schema": {
            "type": "object"
          }
        }
      ],
      "thinking": {
        "type": "enabled",
        "budget_tokens": 16384
      }
    }
    |}]
;;

let%expect_test
    "request body with OAuth: Claude Code identity first, Claude Code tool \
     names"
  =
  let body =
    T.request_body
      ~oauth:true
      (request
         ~system:"Be brief."
         ~max_tokens:2000
         ~thinking:(On (Some Max))
         conversation)
  in
  let field name = Option.value_exn (Json.member name body) in
  print_endline (Json.to_string_hum (field "system"));
  print_endline (Json.to_string (field "thinking"));
  print_endline
    (Json.to_string
       (`Array
           (List.map
              (Json.list_exn (field "tools"))
              ~f:(fun t -> Option.value_exn (Json.member "name" t)))));
  (match Json.list_exn (field "messages") with
   | _ :: assistant :: _ ->
     List.iter
       (Json.list_exn (Option.value_exn (Json.member "content" assistant)))
       ~f:(fun block ->
         Option.iter (Json.member "name" block) ~f:(fun n ->
           print_endline (Json.to_string n)))
   | _ -> ());
  [%expect
    {|
    [
      {
        "type": "text",
        "text": "You are Claude Code, Anthropic's official CLI for Claude.",
        "cache_control": {
          "type": "ephemeral"
        }
      },
      {
        "type": "text",
        "text": "Be brief.",
        "cache_control": {
          "type": "ephemeral"
        }
      }
    ]
    {"type":"enabled","budget_tokens":1024}
    ["Bash","subagent"]
    "Bash"
    "subagent"
    |}]
;;

let%expect_test "headers: api key vs oauth token, betas" =
  let show ~auth ~thinking_on =
    List.iter (T.headers ~auth ~thinking_on) ~f:(fun (k, v) ->
      printf "%s: %s\n" k v)
  in
  show ~auth:(Api_key "sk-ant-api03-x") ~thinking_on:false;
  print_endline "--";
  show ~auth:(Oauth "sk-ant-oat01-x") ~thinking_on:true;
  print_endline "--";
  print_s
    [%sexp
      (( (match
            Anthropic.auth_of_token ~method_:Api_key "sk-ant-oat01-from-env"
          with
          | Oauth _ -> "oauth"
          | Api_key _ -> "api_key")
       , (match Anthropic.auth_of_token ~method_:Api_key "sk-ant-api03-x" with
          | Oauth _ -> "oauth"
          | Api_key _ -> "api_key")
       , match Anthropic.auth_of_token ~method_:Oauth "whatever" with
         | Oauth _ -> "oauth"
         | Api_key _ -> "api_key" )
       : string * string * string)];
  [%expect
    {|
    anthropic-version: 2023-06-01
    x-api-key: sk-ant-api03-x
    --
    anthropic-version: 2023-06-01
    Authorization: Bearer sk-ant-oat01-x
    user-agent: claude-cli/2.1.251
    x-app: cli
    anthropic-beta: claude-code-20250219,oauth-2025-04-20,interleaved-thinking-2025-05-14
    --
    (oauth api_key oauth)
    |}]
;;

let stream_fixture =
  [ {|{"type":"message_start","message":{"usage":{"input_tokens":10,"cache_read_input_tokens":90,"cache_creation_input_tokens":5,"output_tokens":1}}}|}
  ; {|{"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":""}}|}
  ; {|{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"Let me "}}|}
  ; {|{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"look."}}|}
  ; {|{"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"SIG=="}}|}
  ; {|{"type":"content_block_stop","index":0}|}
  ; {|{"type":"content_block_start","index":1,"content_block":{"type":"text","text":""}}|}
  ; {|{"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"Running"}}|}
  ; {|{"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":" ls."}}|}
  ; {|{"type":"content_block_stop","index":1}|}
  ; {|{"type":"content_block_start","index":2,"content_block":{"type":"tool_use","id":"toolu_9","name":"Bash","input":{}}}|}
  ; {|{"type":"content_block_delta","index":2,"delta":{"type":"input_json_delta","partial_json":"{\"cmd\":"}}|}
  ; {|{"type":"content_block_delta","index":2,"delta":{"type":"input_json_delta","partial_json":"\"ls\"}"}}|}
  ; {|{"type":"content_block_stop","index":2}|}
  ; {|{"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":42}}|}
  ; {|{"type":"message_stop"}|}
  ]
;;

let%expect_test
    "parse events: thinking with signature, text, tool use mapped back to our \
     name, usage"
  =
  let events, `Stop_reason stop, `Usage usage, `Error error =
    T.parse_events ~tools stream_fixture
  in
  print_s [%sexp (events : Assistant_event.t list)];
  print_s
    [%sexp (stop : string option), (usage : Usage.t), (error : string option)];
  [%expect
    {|
    ((Thinking_delta "Let me ") (Thinking_delta look.) (Thinking_signature SIG==)
     (Text_delta Running) (Text_delta " ls.")
     (Tool_call_start (index 0) (id toolu_9) (name bash))
     (Tool_call_delta (index 0) (arguments "{\"cmd\":"))
     (Tool_call_delta (index 0) (arguments "\"ls\"}")))
    ((tool_use) ((input 105) (output 42) (cache_read 90)) ())
    |}];
  let events, _, _, `Error error =
    T.parse_events
      ~tools
      [ {|{"type":"content_block_start","index":0,"content_block":{"type":"redacted_thinking","data":"OPAQUE"}}|}
      ; {|{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}|}
      ; "{bad"
      ]
  in
  print_s [%sexp (events : Assistant_event.t list), (error : string option)];
  [%expect
    {|
    (((Thinking_delta "") (Thinking_signature redacted:OPAQUE))
     ("bad JSON in stream: json > object: char '}'"))
    |}]
;;

let sse events =
  String.concat
    (List.map events ~f:(fun data ->
       let type_ =
         Option.value_exn (Json.member "type" (Json.of_string data))
       in
       sprintf "event: %s\ndata: %s\n\n" (Json.string_exn type_) data))
;;

let stream_with_server
      ~env
      ~sw
      ~auth
      ?(status = 200)
      ?(request = request ~thinking:(On None) [ Message.user "hi" ])
      body
  =
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      { status
      ; headers = [ "Content-Type", "text/event-stream" ]
      ; chunks = [ 0., body ]
      })
  in
  let provider =
    Anthropic.create ~env ~base_url:(Server.url server "") ~auth ()
  in
  let events = ref [] in
  let message =
    provider.stream request ~cancel:Cancellation.never ~on_event:(fun e ->
      events := e :: !events)
  in
  print_s [%sexp (message : Message.Assistant.t)];
  server
;;

let%expect_test "stream end to end: request shape, headers, assembled message" =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let server =
    stream_with_server
      ~env
      ~sw
      ~auth:(Oauth "sk-ant-oat01-t")
      (sse stream_fixture)
  in
  [%expect
    {|
    ((content
      ((Thinking ((text "Let me look.") (signature (SIG==))))
       (Text "Running ls.")
       (Tool_call ((id toolu_9) (name bash) (arguments "{\"cmd\":\"ls\"}")))))
     (stop_reason Tool_use) (usage ((input 105) (output 42) (cache_read 90)))
     (model anthropic/claude-opus-4-6))
    |}];
  let r = List.hd_exn (Server.requests server) in
  print_endline r.request_line;
  List.iter
    [ "authorization"; "anthropic-beta"; "x-app"; "user-agent"; "accept" ]
    ~f:(fun h ->
      printf
        "%s: %s\n"
        h
        (Option.value (Server.Request.header r h) ~default:"-"));
  let body = Json.of_string r.body in
  print_endline (Json.to_string (Option.value_exn (Json.member "system" body)));
  print_endline
    (Json.to_string (Option.value_exn (Json.member "thinking" body)));
  [%expect
    {|
    POST /v1/messages HTTP/1.1
    authorization: Bearer sk-ant-oat01-t
    anthropic-beta: claude-code-20250219,oauth-2025-04-20,interleaved-thinking-2025-05-14
    x-app: cli
    user-agent: claude-cli/2.1.251
    accept: text/event-stream
    [{"type":"text","text":"You are Claude Code, Anthropic's official CLI for Claude.","cache_control":{"type":"ephemeral"}}]
    {"type":"enabled","budget_tokens":8192}
    |}];
  let (_ : Server.t) =
    stream_with_server
      ~env
      ~sw
      ~auth:(Api_key "sk")
      ~status:401
      {|{"type":"error","error":{"type":"authentication_error","message":"invalid x-api-key"}}|}
  in
  let (_ : Server.t) =
    stream_with_server
      ~env
      ~sw
      ~auth:(Api_key "sk")
      (sse
         [ {|{"type":"message_start","message":{"usage":{"input_tokens":3}}}|}
         ; {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}|}
         ; {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"partial"}}|}
         ; {|{"type":"message_delta","delta":{"stop_reason":"max_tokens"},"usage":{"output_tokens":9}}|}
         ])
  in
  [%expect
    {|
    ((content ()) (stop_reason (Error "HTTP 401: invalid x-api-key"))
     (usage ((input 0) (output 0) (cache_read 0)))
     (model anthropic/claude-opus-4-6))
    ((content ((Text partial))) (stop_reason Length)
     (usage ((input 3) (output 9) (cache_read 0)))
     (model anthropic/claude-opus-4-6))
    |}]
;;
