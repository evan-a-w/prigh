open! Core
open! Prigh
open Eio.Std
module Server = Fake_http_server
module T = Openai_responses.For_testing
module Json = Jsonaf

let codex_model = Model.default_for Openai_codex
let openai_model = Model.default_for Openai

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
  ]
;;

let request
      ?(model = codex_model)
      ?(thinking = Thinking.Off)
      ?(tools = tools)
      ?system
      ?max_tokens
      messages
  =
  { Provider.Request.model; system; messages; tools; thinking; max_tokens }
;;

let reasoning_item =
  {|{"type":"reasoning","id":"rs_1","summary":[{"type":"summary_text","text":"Plan"}],"encrypted_content":"ENC"}|}
;;

let conversation =
  [ Message.user "list files"
  ; Assistant
      { content =
          [ Content.thinking ~signature:reasoning_item "Plan"
          ; Text "Listing."
          ; Tool_call
              { id = "call_1"; name = "bash"; arguments = {|{"cmd":"ls"}|} }
          ]
      ; stop_reason = Tool_use
      ; usage = Usage.zero
      ; model = Model.key codex_model
      }
  ; Tool_result
      { tool_call_id = "call_1"
      ; tool_name = "bash"
      ; text = "a\nb"
      ; is_error = false
      }
  ; Assistant
      { content =
          [ Content.thinking ~signature:"foreign" "x"
          ; Content.thinking "unsigned"
          ; Text "Done."
          ; Text "Really."
          ]
      ; stop_reason = End_turn
      ; usage = Usage.zero
      ; model = "anthropic/claude-opus-4-6"
      }
  ]
;;

let%expect_test
    "codex request body: instructions, replayed reasoning, function calls, \
     tools"
  =
  print_endline
    (Json.to_string_hum
       (T.request_body
          ~endpoint:(Codex { access_token = "t"; account_id = "acct" })
          (request ~system:"Be brief." ~thinking:(On (Some Max)) conversation)));
  [%expect
    {|
    {
      "model": "gpt-5.5",
      "stream": true,
      "store": false,
      "instructions": "Be brief.",
      "input": [
        {
          "type": "message",
          "role": "user",
          "content": [
            {
              "type": "input_text",
              "text": "list files"
            }
          ]
        },
        {
          "type": "reasoning",
          "id": "rs_1",
          "summary": [
            {
              "type": "summary_text",
              "text": "Plan"
            }
          ],
          "encrypted_content": "ENC"
        },
        {
          "type": "message",
          "role": "assistant",
          "id": "msg_prigh_1",
          "status": "completed",
          "content": [
            {
              "type": "output_text",
              "text": "Listing.",
              "annotations": []
            }
          ]
        },
        {
          "type": "function_call",
          "call_id": "call_1",
          "name": "bash",
          "arguments": "{\"cmd\":\"ls\"}"
        },
        {
          "type": "function_call_output",
          "call_id": "call_1",
          "output": "a\nb"
        },
        {
          "type": "message",
          "role": "assistant",
          "id": "msg_prigh_3",
          "status": "completed",
          "content": [
            {
              "type": "output_text",
              "text": "Done.",
              "annotations": []
            }
          ]
        },
        {
          "type": "message",
          "role": "assistant",
          "id": "msg_prigh_3_1",
          "status": "completed",
          "content": [
            {
              "type": "output_text",
              "text": "Really.",
              "annotations": []
            }
          ]
        }
      ],
      "include": [
        "reasoning.encrypted_content"
      ],
      "tool_choice": "auto",
      "parallel_tool_calls": true,
      "tools": [
        {
          "type": "function",
          "name": "bash",
          "description": "Run a command",
          "parameters": {
            "type": "object",
            "properties": {
              "cmd": {
                "type": "string"
              }
            }
          },
          "strict": false
        }
      ],
      "reasoning": {
        "effort": "xhigh",
        "summary": "auto"
      },
      "text": {
        "verbosity": "low"
      }
    }
    |}]
;;

let%expect_test
    "openai request body: max_output_tokens, default instructions, thinking off"
  =
  let body =
    T.request_body
      ~endpoint:(Openai { api_key = "k" })
      (request
         ~model:openai_model
         ~max_tokens:500
         ~tools:[]
         [ Message.user "hi" ])
  in
  print_endline (Json.to_string body);
  [%expect
    {| {"model":"gpt-5.5","stream":true,"store":false,"instructions":"You are a helpful assistant.","input":[{"type":"message","role":"user","content":[{"type":"input_text","text":"hi"}]}],"include":["reasoning.encrypted_content"],"tool_choice":"auto","parallel_tool_calls":true,"max_output_tokens":500} |}]
;;

let%expect_test "headers" =
  List.iter
    (T.headers (Openai { api_key = "sk-k" }))
    ~f:(fun (k, v) -> printf "%s: %s\n" k v);
  print_endline "--";
  List.iter
    (T.headers (Codex { access_token = "tok"; account_id = "acct-1" }))
    ~f:(fun (k, v) -> printf "%s: %s\n" k v);
  [%expect
    {|
    Authorization: Bearer sk-k
    --
    Authorization: Bearer tok
    chatgpt-account-id: acct-1
    originator: prigh
    OpenAI-Beta: responses=experimental
    User-Agent: prigh/0.1.0
    |}]
;;

let stream_fixture =
  [ {|{"type":"response.created","response":{"id":"resp_1"}}|}
  ; {|{"type":"response.output_item.added","output_index":0,"item":{"type":"reasoning","id":"rs_1","summary":[]}}|}
  ; {|{"type":"response.reasoning_summary_part.added","output_index":0,"summary_index":0}|}
  ; {|{"type":"response.reasoning_summary_text.delta","output_index":0,"delta":"Think"}|}
  ; {|{"type":"response.reasoning_summary_part.added","output_index":0,"summary_index":1}|}
  ; {|{"type":"response.reasoning_summary_text.delta","output_index":0,"delta":"more"}|}
  ; {|{"type":"response.output_item.done","output_index":0,"item":{"type":"reasoning","id":"rs_1","summary":[{"type":"summary_text","text":"Think"},{"type":"summary_text","text":"more"}],"encrypted_content":"ENC1"}}|}
  ; {|{"type":"response.output_item.added","output_index":1,"item":{"type":"message","id":"msg_1","role":"assistant"}}|}
  ; {|{"type":"response.output_text.delta","output_index":1,"delta":"Running"}|}
  ; {|{"type":"response.output_text.delta","output_index":1,"delta":" ls."}|}
  ; {|{"type":"response.output_item.done","output_index":1,"item":{"type":"message"}}|}
  ; {|{"type":"response.output_item.added","output_index":2,"item":{"type":"function_call","id":"fc_1","call_id":"call_9","name":"bash","arguments":""}}|}
  ; {|{"type":"response.function_call_arguments.delta","output_index":2,"delta":"{\"cmd\":"}|}
  ; {|{"type":"response.function_call_arguments.delta","output_index":2,"delta":"\"ls\"}"}|}
  ; {|{"type":"response.output_item.done","output_index":2,"item":{"type":"function_call","call_id":"call_9","name":"bash","arguments":"{\"cmd\":\"ls\"}"}}|}
  ; {|{"type":"response.output_item.added","output_index":3,"item":{"type":"function_call","id":"fc_2","call_id":"call_10","name":"bash","arguments":""}}|}
  ; {|{"type":"response.output_item.done","output_index":3,"item":{"type":"function_call","call_id":"call_10","name":"bash","arguments":"{\"cmd\":\"pwd\"}"}}|}
  ; {|{"type":"response.completed","response":{"id":"resp_1","usage":{"input_tokens":100,"input_tokens_details":{"cached_tokens":80},"output_tokens":20}}}|}
  ]
;;

let%expect_test
    "parse events: reasoning summary parts, signature, text, tool calls with \
     and without deltas"
  =
  let events, stop, usage = T.parse_events stream_fixture in
  print_s [%sexp (events : Assistant_event.t list)];
  print_s [%sexp (stop : Stop_reason.t), (usage : Usage.t)];
  [%expect
    {|
    ((Thinking_delta Think) (Thinking_delta  "\
                                            \n\
                                            \n")
     (Thinking_delta more)
     (Thinking_signature
      "{\"type\":\"reasoning\",\"id\":\"rs_1\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"Think\"},{\"type\":\"summary_text\",\"text\":\"more\"}],\"encrypted_content\":\"ENC1\"}")
     (Text_delta Running) (Text_delta " ls.")
     (Tool_call_start (index 0) (id call_9) (name bash))
     (Tool_call_delta (index 0) (arguments "{\"cmd\":"))
     (Tool_call_delta (index 0) (arguments "\"ls\"}"))
     (Tool_call_start (index 1) (id call_10) (name bash))
     (Tool_call_delta (index 1) (arguments "{\"cmd\":\"pwd\"}")))
    (Tool_use ((input 100) (output 20) (cache_read 80)))
    |}];
  let show events =
    let events, stop, _ = T.parse_events events in
    print_s [%sexp (events : Assistant_event.t list), (stop : Stop_reason.t)]
  in
  show
    [ {|{"type":"response.output_text.delta","output_index":0,"delta":"partial"}|}
    ; {|{"type":"response.incomplete","response":{"incomplete_details":{"reason":"max_output_tokens"},"usage":{"input_tokens":1,"output_tokens":2}}}|}
    ];
  show
    [ {|{"type":"response.failed","response":{"error":{"code":"server_error","message":"boom"}}}|}
    ];
  show [ {|{"type":"error","code":"usage_limit_reached","message":"limit"}|} ];
  [%expect
    {|
    (((Text_delta partial)) Length)
    (() (Error boom))
    (() (Error limit))
    |}]
;;

let sse events =
  String.concat
    (List.map events ~f:(fun data ->
       let type_ =
         Json.string_exn
           (Option.value_exn (Json.member "type" (Json.of_string data)))
       in
       sprintf "event: %s\ndata: %s\n\n" type_ data))
;;

let%expect_test "stream end to end against the codex endpoint" =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let server =
    Server.start ~sw ~env ~handler:(fun _ ->
      { status = 200
      ; headers = [ "Content-Type", "text/event-stream" ]
      ; chunks = [ 0., sse stream_fixture ]
      })
  in
  let provider =
    Openai_responses.create
      ~env
      ~base_url:(Server.url server "")
      ~endpoint:(Codex { access_token = "tok"; account_id = "acct-1" })
      ()
  in
  let message =
    provider.stream
      (request ~thinking:(On None) [ Message.user "hi" ])
      ~cancel:Cancellation.never
      ~on_event:ignore
  in
  print_s [%sexp (message : Message.Assistant.t)];
  let r = List.hd_exn (Server.requests server) in
  print_endline r.request_line;
  List.iter
    [ "authorization"; "chatgpt-account-id"; "openai-beta"; "originator" ]
    ~f:(fun h ->
      printf
        "%s: %s\n"
        h
        (Option.value (Server.Request.header r h) ~default:"-"));
  print_endline
    (Json.to_string
       (Option.value_exn (Json.member "reasoning" (Json.of_string r.body))));
  [%expect
    {|
    ((content
      ((Thinking
        ((text  "Think\
               \n\
               \nmore")
         (signature
          ("{\"type\":\"reasoning\",\"id\":\"rs_1\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"Think\"},{\"type\":\"summary_text\",\"text\":\"more\"}],\"encrypted_content\":\"ENC1\"}"))))
       (Text "Running ls.")
       (Tool_call ((id call_9) (name bash) (arguments "{\"cmd\":\"ls\"}")))
       (Tool_call ((id call_10) (name bash) (arguments "{\"cmd\":\"pwd\"}")))))
     (stop_reason Tool_use) (usage ((input 100) (output 20) (cache_read 80)))
     (model openai-codex/gpt-5.5))
    POST /codex/responses HTTP/1.1
    authorization: Bearer tok
    chatgpt-account-id: acct-1
    openai-beta: responses=experimental
    originator: prigh
    {"effort":"medium","summary":"auto"}
    |}]
;;
