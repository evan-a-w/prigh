open! Core
open! Prigh
module Json = Jsonaf

let anthropic_model = Option.value_exn (Model.default_for Anthropic)
let codex_model = Option.value_exn (Model.default_for Openai_codex)
let deepseek_model = Option.value_exn (Model.default_for Deepseek)

let request ~model messages =
  { Provider.Request.model
  ; system = None
  ; messages
  ; tools = [ Tool_web_search.tool.spec ]
  ; thinking = Off
  ; max_tokens = None
  ; cache_key = None
  }
;;

let print_events events =
  List.iter events ~f:(fun e -> print_s [%sexp (e : Assistant_event.t)])
;;

let build ~model events =
  let builder = Assistant_builder.create ~model:(Model.key model) in
  List.iter events ~f:(Assistant_builder.apply builder);
  Assistant_builder.finish builder ~stop_reason:End_turn ~usage:Usage.zero
;;

let print_member json name =
  print_endline (Json.to_string (Option.value_exn (Json.member name json)))
;;

let anthropic_stream =
  [ {|{"type":"message_start","message":{"usage":{"input_tokens":10}}}|}
  ; {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}|}
  ; {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Let me search."}}|}
  ; {|{"type":"content_block_stop","index":0}|}
  ; {|{"type":"content_block_start","index":1,"content_block":{"type":"server_tool_use","id":"srvtoolu_1","name":"web_search","input":{}}}|}
  ; {|{"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\"query\": \"ocaml "}}|}
  ; {|{"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"eio\"}"}}|}
  ; {|{"type":"content_block_stop","index":1}|}
  ; {|{"type":"content_block_start","index":2,"content_block":{"type":"web_search_tool_result","tool_use_id":"srvtoolu_1","content":[{"type":"web_search_result","title":"Eio docs","url":"https://ocaml.org/p/eio","encrypted_content":"ENC1","page_age":"2 days ago"},{"type":"web_search_result","title":"","url":"https://github.com/ocaml-multicore/eio","encrypted_content":"ENC2"}]}}|}
  ; {|{"type":"content_block_stop","index":2}|}
  ; {|{"type":"content_block_start","index":3,"content_block":{"type":"text","text":"","citations":[]}}|}
  ; {|{"type":"content_block_delta","index":3,"delta":{"type":"citations_delta","citation":{"type":"web_search_result_location","url":"https://ocaml.org/p/eio"}}}|}
  ; {|{"type":"content_block_delta","index":3,"delta":{"type":"text_delta","text":"Eio is an effects-based IO library."}}|}
  ; {|{"type":"content_block_stop","index":3}|}
  ; {|{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":20}}|}
  ]
;;

let%expect_test "anthropic: a search streams as a server tool between text" =
  let events, stop, _, error =
    Anthropic.For_testing.parse_events
      ~tools:[ Tool_web_search.tool.spec ]
      anthropic_stream
  in
  print_events events;
  print_s [%sexp (stop : [ `Stop_reason of string option ])];
  print_s [%sexp (error : [ `Error of string option ])];
  [%expect
    {|
    (Text_delta "Let me search.")
    (Server_tool_use (id srvtoolu_1) (name web_search)
     (input "{\"query\": \"ocaml eio\"}"))
    (Server_tool_result (id srvtoolu_1)
     (output
       "1. Eio docs \226\128\148 https://ocaml.org/p/eio (2 days ago)\
      \n2. https://github.com/ocaml-multicore/eio \226\128\148 https://github.com/ocaml-multicore/eio")
     (is_error false)
     (raw
      "{\"type\":\"web_search_tool_result\",\"tool_use_id\":\"srvtoolu_1\",\"content\":[{\"type\":\"web_search_result\",\"title\":\"Eio docs\",\"url\":\"https://ocaml.org/p/eio\",\"encrypted_content\":\"ENC1\",\"page_age\":\"2 days ago\"},{\"type\":\"web_search_result\",\"title\":\"\",\"url\":\"https://github.com/ocaml-multicore/eio\",\"encrypted_content\":\"ENC2\"}]}"))
    (Text_delta "Eio is an effects-based IO library.")
    (Stop_reason (end_turn))
    (Error ())
    |}];
  let message = build ~model:anthropic_model events in
  List.iter
    (Json.list_exn
       (Option.value_exn
          (Json.member "content" (Rpc_json.message (Assistant message)))))
    ~f:(fun c -> print_endline (Json.to_string c));
  (* pi-web sees it as text. *)
  print_endline
    (Json.to_string
       (Option.value_exn
          (Json.member
             "content"
             (Pi_protocol.message
                ~timestamp:0
                (Rpc_json.message (Assistant message))))));
  List.iter events ~f:(fun e ->
    print_endline (Json.to_string (Rpc_json.delta e)));
  [%expect
    {|
    {"type":"text","text":"Let me search."}
    {"type":"server_tool","id":"srvtoolu_1","name":"web_search","input":"{\"query\": \"ocaml eio\"}","output":"1. Eio docs — https://ocaml.org/p/eio (2 days ago)\n2. https://github.com/ocaml-multicore/eio — https://github.com/ocaml-multicore/eio","is_error":false}
    {"type":"text","text":"Eio is an effects-based IO library."}
    [{"type":"text","text":"Let me search."},{"type":"text","text":"[web_search {\"query\": \"ocaml eio\"}]\n1. Eio docs — https://ocaml.org/p/eio (2 days ago)\n2. https://github.com/ocaml-multicore/eio — https://github.com/ocaml-multicore/eio"},{"type":"text","text":"Eio is an effects-based IO library."}]
    {"type":"text_delta","text":"Let me search."}
    {"type":"server_tool_use","id":"srvtoolu_1","name":"web_search","input":"{\"query\": \"ocaml eio\"}"}
    {"type":"server_tool_result","id":"srvtoolu_1","output":"1. Eio docs — https://ocaml.org/p/eio (2 days ago)\n2. https://github.com/ocaml-multicore/eio — https://github.com/ocaml-multicore/eio","is_error":false}
    {"type":"text_delta","text":"Eio is an effects-based IO library."}
    |}];
  (* Replayed to Anthropic as it came; to others as text. *)
  let body =
    Anthropic.For_testing.request_body
      ~oauth:false
      (request
         ~model:anthropic_model
         [ Message.user "eio?"; Assistant message ])
  in
  print_member body "tools";
  print_endline
    (Json.to_string_hum
       (List.last_exn
          (Json.list_exn (Option.value_exn (Json.member "messages" body)))));
  [%expect
    {|
    [{"type":"web_search_20250305","name":"web_search","max_uses":5}]
    {
      "role": "assistant",
      "content": [
        {
          "type": "text",
          "text": "Let me search."
        },
        {
          "type": "server_tool_use",
          "id": "srvtoolu_1",
          "name": "web_search",
          "input": {
            "query": "ocaml eio"
          }
        },
        {
          "type": "web_search_tool_result",
          "tool_use_id": "srvtoolu_1",
          "content": [
            {
              "type": "web_search_result",
              "title": "Eio docs",
              "url": "https://ocaml.org/p/eio",
              "encrypted_content": "ENC1",
              "page_age": "2 days ago"
            },
            {
              "type": "web_search_result",
              "title": "",
              "url": "https://github.com/ocaml-multicore/eio",
              "encrypted_content": "ENC2"
            }
          ]
        },
        {
          "type": "text",
          "text": "Eio is an effects-based IO library.",
          "cache_control": {
            "type": "ephemeral"
          }
        }
      ]
    }
    |}];
  let body =
    Openai_responses.For_testing.request_body
      ~endpoint:(Codex { access_token = "t"; account_id = "a" })
      (request ~model:codex_model [ Message.user "eio?"; Assistant message ])
  in
  print_endline
    (Json.to_string_hum
       (`Array
           (List.drop
              (Json.list_exn (Option.value_exn (Json.member "input" body)))
              1)));
  [%expect
    {|
    [
      {
        "type": "message",
        "role": "assistant",
        "id": "msg_prigh_1",
        "status": "completed",
        "content": [
          {
            "type": "output_text",
            "text": "Let me search.",
            "annotations": []
          }
        ]
      },
      {
        "type": "message",
        "role": "assistant",
        "id": "msg_prigh_1_srvtoolu_1",
        "status": "completed",
        "content": [
          {
            "type": "output_text",
            "text": "[web_search {\"query\": \"ocaml eio\"}]\n1. Eio docs — https://ocaml.org/p/eio (2 days ago)\n2. https://github.com/ocaml-multicore/eio — https://github.com/ocaml-multicore/eio",
            "annotations": []
          }
        ]
      },
      {
        "type": "message",
        "role": "assistant",
        "id": "msg_prigh_1_1",
        "status": "completed",
        "content": [
          {
            "type": "output_text",
            "text": "Eio is an effects-based IO library.",
            "annotations": []
          }
        ]
      }
    ]
    |}]
;;

let%expect_test "anthropic: a failed search" =
  let events, _, _, _ =
    Anthropic.For_testing.parse_events
      ~tools:[ Tool_web_search.tool.spec ]
      [ {|{"type":"content_block_start","index":0,"content_block":{"type":"server_tool_use","id":"srvtoolu_2","name":"web_search","input":{"query":"x"}}}|}
      ; {|{"type":"content_block_stop","index":0}|}
      ; {|{"type":"content_block_start","index":1,"content_block":{"type":"web_search_tool_result","tool_use_id":"srvtoolu_2","content":{"type":"web_search_tool_result_error","error_code":"max_uses_exceeded"}}}|}
      ; {|{"type":"message_delta","delta":{"stop_reason":"pause_turn"}}|}
      ]
  in
  let message = build ~model:anthropic_model events in
  print_s [%sexp (message.content : Content.t list)];
  [%expect
    {|
    ((Server_tool
      ((id srvtoolu_2) (name web_search) (input "{\"query\":\"x\"}")
       (output ("search failed: max_uses_exceeded")) (is_error true)
       (raw
        ("{\"type\":\"web_search_tool_result\",\"tool_use_id\":\"srvtoolu_2\",\"content\":{\"type\":\"web_search_tool_result_error\",\"error_code\":\"max_uses_exceeded\"}}")))))
    |}]
;;

let%expect_test "openai responses: web_search_call items" =
  let item =
    {|{"type":"web_search_call","id":"ws_1","status":"completed","action":{"type":"search","query":"ocaml eio","sources":[{"type":"url","url":"https://ocaml.org/p/eio"}]}}|}
  in
  let events, stop, _ =
    Openai_responses.For_testing.parse_events
      [ {|{"type":"response.output_item.added","output_index":0,"item":{"type":"web_search_call","id":"ws_1","status":"in_progress"}}|}
      ; sprintf
          {|{"type":"response.output_item.done","output_index":0,"item":%s}|}
          item
      ; {|{"type":"response.output_item.done","output_index":1,"item":{"type":"web_search_call","id":"ws_2","status":"completed","action":{"type":"open_page","url":"https://ocaml.org/p/eio"}}}|}
      ; {|{"type":"response.output_item.done","output_index":2,"item":{"type":"web_search_call","id":"ws_3","status":"completed","action":{"type":"search","query":"nothing"}}}|}
      ; {|{"type":"response.output_item.done","output_index":3,"item":{"type":"web_search_call","id":"ws_4","status":"failed","action":{"type":"search","query":"broken"}}}|}
      ; {|{"type":"response.output_text.delta","delta":"Eio is IO."}|}
      ; {|{"type":"response.completed","response":{"status":"completed","usage":{"input_tokens":5,"output_tokens":3}}}|}
      ]
  in
  print_s [%sexp (stop : Stop_reason.t)];
  let message = build ~model:codex_model events in
  List.iter message.content ~f:(fun c ->
    match c with
    | Server_tool tool ->
      print_s
        [%sexp
          (tool.id : string)
        , (tool.input : string)
        , (tool.output : string option)
        , (tool.is_error : bool)]
    | c -> print_s [%sexp (c : Content.t)]);
  [%expect
    {|
    End_turn
    (ws_1 "{\"type\":\"search\",\"query\":\"ocaml eio\"}"
     ("1. https://ocaml.org/p/eio \226\128\148 https://ocaml.org/p/eio") false)
    (ws_2 "{\"type\":\"open_page\",\"url\":\"https://ocaml.org/p/eio\"}"
     ("opened https://ocaml.org/p/eio") false)
    (ws_3 "{\"type\":\"search\",\"query\":\"nothing\"}" (searched) false)
    (ws_4 "{\"type\":\"search\",\"query\":\"broken\"}" ("search failed") true)
    (Text "Eio is IO.")
    |}];
  (* Replayed verbatim to OpenAI; the request asks for the sources. *)
  let body =
    Openai_responses.For_testing.request_body
      ~endpoint:(Codex { access_token = "t"; account_id = "a" })
      (request ~model:codex_model [ Message.user "eio?"; Assistant message ])
  in
  print_member body "tools";
  print_member body "include";
  print_endline
    (Json.to_string
       (List.nth_exn
          (Json.list_exn (Option.value_exn (Json.member "input" body)))
          1));
  [%expect
    {|
    [{"type":"web_search"}]
    ["reasoning.encrypted_content","web_search_call.action.sources"]
    {"type":"web_search_call","id":"ws_1","status":"completed","action":{"type":"search","query":"ocaml eio","sources":[{"type":"url","url":"https://ocaml.org/p/eio"}]}}
    |}];
  (* DeepSeek (chat completions) sees it as text. *)
  let body =
    Openai_chat.For_testing.request_body
      ~quirks:Openai_chat.Quirks.deepseek
      (request ~model:deepseek_model [ Message.user "eio?"; Assistant message ])
  in
  print_endline
    (Json.to_string_hum
       (List.last_exn
          (Json.list_exn (Option.value_exn (Json.member "messages" body)))));
  [%expect
    {|
    {
      "role": "assistant",
      "content": "\n[web_search {\"type\":\"search\",\"query\":\"ocaml eio\"}]\n1. https://ocaml.org/p/eio — https://ocaml.org/p/eio\n\n[web_search {\"type\":\"open_page\",\"url\":\"https://ocaml.org/p/eio\"}]\nopened https://ocaml.org/p/eio\n\n[web_search {\"type\":\"search\",\"query\":\"nothing\"}]\nsearched\n\n[web_search {\"type\":\"search\",\"query\":\"broken\"}]\nsearch failed\nEio is IO."
    }
    |}]
;;

let%expect_test "only models whose provider searches get web_search" =
  List.iter
    ([ anthropic_model
     ; codex_model
     ; Option.value_exn (Model.default_for Openai)
     ; deepseek_model
     ]
     @ List.map
         [ "gpt-4"
         ; "gpt-4-turbo"
         ; "gpt-4.1"
         ; "gpt-4.1-nano"
         ; "gpt-4o"
         ; "o1"
         ; "o1-pro"
         ; "o3"
         ; "o3-mini"
         ; "o4-mini"
         ]
         ~f:(fun id -> Option.value_exn (Model.find ("openai/" ^ id))))
    ~f:(fun model ->
      printf
        "%s: %s\n"
        (Model.key model)
        (String.concat
           ~sep:" "
           (List.map (Tools.for_model model Tools.all) ~f:Tool.name)));
  [%expect
    {|
    anthropic/claude-opus-4-6: bash read write edit ls grep find web_fetch web_search
    openai-codex/gpt-5.5: bash read write apply_patch ls grep find web_fetch web_search
    openai/gpt-5.5: bash read write apply_patch ls grep find web_fetch web_search
    deepseek/deepseek-flash: bash read write edit ls grep find web_fetch
    openai/gpt-4: bash read write edit ls grep find web_fetch
    openai/gpt-4-turbo: bash read write edit ls grep find web_fetch
    openai/gpt-4.1: bash read write edit ls grep find web_fetch web_search
    openai/gpt-4.1-nano: bash read write edit ls grep find web_fetch
    openai/gpt-4o: bash read write edit ls grep find web_fetch web_search
    openai/o1: bash read write edit ls grep find web_fetch
    openai/o1-pro: bash read write edit ls grep find web_fetch
    openai/o3: bash read write edit ls grep find web_fetch web_search
    openai/o3-mini: bash read write edit ls grep find web_fetch
    openai/o4-mini: bash read write edit ls grep find web_fetch web_search
    |}]
;;

let%expect_test "a web_search call that reaches the harness" =
  Eio_main.run
  @@ fun env ->
  let result =
    Tool.execute
      Tool_web_search.tool
      (Tool.Context.create ~env ~cwd:"/" ())
      (`Object [ "query", `String "x" ])
  in
  print_s [%sexp (result : Tool_result.t)];
  [%expect
    {|
    ((text
      "web_search runs on the model provider's servers and is not available with this model; use web_fetch with a search engine's URL instead")
     (is_error true))
    |}]
;;

let%expect_test "a search in a session: streamed to clients, saved, reloaded" =
  let reply =
    { Faux_provider.Reply.events =
        [ Text_delta "Searching."
        ; Server_tool_use
            { id = "s1"; name = "web_search"; input = {|{"query":"eio"}|} }
        ; Server_tool_result
            { id = "s1"
            ; output = "1. Eio — https://eio"
            ; is_error = false
            ; raw = Some "{}"
            }
        ; Text_delta "Found."
        ]
    ; stop_reason = End_turn
    ; usage = Usage.zero
    }
  in
  Test_rpc.with_agent [ reply ]
  @@ fun t agent h ->
  Queue.clear h.sent;
  Test_rpc.call t h ~params:{|{"text": "find eio"}|} "prompt";
  Agent.wait_idle agent;
  Queue.iter h.sent ~f:(fun json ->
    match Json.member "event" json with
    | Some (`String "message_update") ->
      print_endline
        (Json.to_string (Option.value_exn (Json.member "delta" json)))
    | _ -> ());
  let reloaded =
    Or_error.ok_exn (Session.load (Agent.state agent).session_path)
  in
  List.iter (Session.messages reloaded) ~f:(function
    | Assistant a -> print_s [%sexp (a.content : Content.t list)]
    | _ -> ());
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"text_delta","text":"Searching."}
    {"type":"server_tool_use","id":"s1","name":"web_search","input":"{\"query\":\"eio\"}"}
    {"type":"server_tool_result","id":"s1","output":"1. Eio — https://eio","is_error":false}
    {"type":"text_delta","text":"Found."}
    ((Text Searching.)
     (Server_tool
      ((id s1) (name web_search) (input "{\"query\":\"eio\"}")
       (output ("1. Eio \226\128\148 https://eio")) (is_error false) (raw ({}))))
     (Text Found.))
    |}]
;;

let%expect_test "faux scripts can play searches" =
  let reply =
    Or_error.ok_exn
      (Faux_provider.Reply.of_json
         (Json.of_string
            {|{"searches": [{"query": "eio", "output": "1. Eio — https://eio"}, {"query": "bad", "output": "search failed: too_many_requests", "error": true}], "text": "Found."}|}))
  in
  print_s [%sexp (reply : Faux_provider.Reply.t)];
  [%expect
    {|
    ((events
      ((Server_tool_use (id srvtoolu_faux_1) (name web_search)
        (input "{\"query\":\"eio\"}"))
       (Server_tool_result (id srvtoolu_faux_1)
        (output "1. Eio \226\128\148 https://eio") (is_error false))
       (Server_tool_use (id srvtoolu_faux_2) (name web_search)
        (input "{\"query\":\"bad\"}"))
       (Server_tool_result (id srvtoolu_faux_2)
        (output "search failed: too_many_requests") (is_error true))
       (Text_delta Found.)))
     (stop_reason End_turn) (usage ((input 0) (output 0) (cache_read 0))))
    |}]
;;
