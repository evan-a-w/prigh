open! Core
open! Prigh
module Json = Jsonaf
module A = Anthropic.For_testing
module O = Openai_responses.For_testing

let model id = Or_error.ok_exn (Model.resolve id)
let print_json j = print_endline (Json.to_string_hum j)

let computer_spec =
  (Tool_computer.for_screen
     ~native:true
     { Computer_info.backend = "fake"
     ; detail = None
     ; platform = "linux"
     ; output = None
     ; width = 2560
     ; height = 1600
     ; scaled_width = 1280
     ; scaled_height = 800
     ; scale = 2.
     })
    .spec
;;

let bash_spec = Tool_bash.tool.spec

let request
      ?(tools = [ bash_spec; computer_spec ])
      ?(thinking = Thinking.Off)
      m
      messages
  =
  { Provider.Request.model = model m
  ; system = Some "sys"
  ; messages
  ; tools
  ; thinking
  ; max_tokens = None
  ; cache_key = None
  }
;;

let field j name =
  match j with
  | `Object fields -> List.Assoc.find fields name ~equal:String.equal
  | _ -> None
;;

let list j name =
  match field j name with
  | Some (`Array l) -> l
  | _ -> []
;;

let%expect_test "which models have a built-in computer tool" =
  List.iter
    [ "anthropic/claude-opus-5-5"
    ; "anthropic/claude-sonnet-5-5"
    ; "anthropic/claude-fable-5"
    ; "anthropic/claude-fable-5-1"
    ; "anthropic/claude-fable-5-2"
    ; "anthropic/claude-opus-4-6"
    ; "anthropic/claude-sonnet-4-5"
    ; "anthropic/claude-haiku-4-5"
    ; "openai/gpt-6.1-sol"
    ; "openai/gpt-4.1"
    ; "openai-codex/gpt-6.1-sol"
    ; "deepseek/deepseek-flash"
    ]
    ~f:(fun id ->
      print_s
        [%sexp
          (id : string), ((model id).native_computer : Native_computer.t option)]);
  [%expect
    {|
    (anthropic/claude-opus-5-5 (Anthropic_toolset))
    (anthropic/claude-sonnet-5-5 (Anthropic_toolset))
    (anthropic/claude-fable-5
     ((Anthropic_tool (version computer_20251124) (beta computer-use-2025-11-24)
       (zoom true))))
    (anthropic/claude-fable-5-1
     ((Anthropic_tool (version computer_20251124) (beta computer-use-2025-11-24)
       (zoom true))))
    (anthropic/claude-fable-5-2 (Anthropic_toolset))
    (anthropic/claude-opus-4-6
     ((Anthropic_tool (version computer_20251124) (beta computer-use-2025-11-24)
       (zoom true))))
    (anthropic/claude-sonnet-4-5
     ((Anthropic_tool (version computer_20250124) (beta computer-use-2025-01-24)
       (zoom false))))
    (anthropic/claude-haiku-4-5
     ((Anthropic_tool (version computer_20250124) (beta computer-use-2025-01-24)
       (zoom false))))
    (openai/gpt-6.1-sol (Openai))
    (openai/gpt-4.1 ())
    (openai-codex/gpt-6.1-sol ())
    (deepseek/deepseek-flash ())
    |}];
  List.iter
    [ "claude-mythos-5"; "claude-mythos-5-1"; "claude-mythos-5-2" ]
    ~f:(fun id ->
      print_s
        [%sexp
          (id : string)
        , (Native_computer.of_model Anthropic id : Native_computer.t option)]);
  [%expect
    {|
    (claude-mythos-5
     ((Anthropic_tool (version computer_20251124) (beta computer-use-2025-11-24)
       (zoom true))))
    (claude-mythos-5-1
     ((Anthropic_tool (version computer_20251124) (beta computer-use-2025-11-24)
       (zoom true))))
    (claude-mythos-5-2 (Anthropic_toolset))
    |}]
;;

let%expect_test "the tool is offered natively only with [computer_native]" =
  let tool (spec : Tool_spec.t) =
    print_s [%sexp (spec.native : Tool_spec.Native.t option)]
  in
  tool computer_spec;
  tool
    (Tool_computer.for_screen
       ~native:false
       { Computer_info.backend = "fake"
       ; detail = None
       ; platform = "linux"
       ; output = None
       ; width = 100
       ; height = 100
       ; scaled_width = 100
       ; scaled_height = 100
       ; scale = 1.
       })
      .spec;
  [%expect
    {|
    ((Computer (width 1280) (height 800)))
    ()
    |}]
;;

(* --- Anthropic --- *)

let anthropic_tools m =
  let r = request m [ Message.user "go" ] in
  let body = A.request_body ~oauth:false r in
  List.iter (list body "tools") ~f:(fun t ->
    print_json
      (`Object
          (List.filter
             (match t with
              | `Object f -> f
              | _ -> [])
             ~f:(fun (k, _) -> not (String.equal k "input_schema")))));
  printf
    "betas: %s\n"
    (String.concat
       ~sep:","
       (List.filter_map
          (A.headers
             ~betas:
               (Option.value_map (A.native_computer r) ~default:[] ~f:(function
                  | Anthropic_tool { beta; _ } -> [ beta ]
                  | Anthropic_toolset | Openai -> []))
             ~auth:(Api_key "k")
             ~thinking_on:false
             ())
          ~f:(fun (k, v) -> Option.some_if (String.equal k "anthropic-beta") v)))
;;

let%expect_test "Anthropic: the tools offered" =
  print_endline "== claude-opus-5-5 (toolset)";
  anthropic_tools "anthropic/claude-opus-5-5";
  print_endline "== claude-opus-4-6 (computer_20251124)";
  anthropic_tools "anthropic/claude-opus-4-6";
  print_endline "== claude-sonnet-4-5 (computer_20250124)";
  anthropic_tools "anthropic/claude-sonnet-4-5";
  [%expect
    {|
    == claude-opus-5-5 (toolset)
    {
      "name": "bash",
      "description": "Run a shell command with bash in the working directory. Returns combined stdout and stderr (interleaved) and the exit code if non-zero. Long output keeps its beginning and end, and the full output is saved to a file you can read. Do not use for reading or editing files; use the dedicated tools. A command still running after a while (about 30 seconds by default) is moved to the background as a job, without being restarted: the call returns with its job id and output so far, and its report arrives when it exits."
    }
    {
      "type": "computer_toolset_20260801"
    }
    betas:
    == claude-opus-4-6 (computer_20251124)
    {
      "name": "bash",
      "description": "Run a shell command with bash in the working directory. Returns combined stdout and stderr (interleaved) and the exit code if non-zero. Long output keeps its beginning and end, and the full output is saved to a file you can read. Do not use for reading or editing files; use the dedicated tools. A command still running after a while (about 30 seconds by default) is moved to the background as a job, without being restarted: the call returns with its job id and output so far, and its report arrives when it exits."
    }
    {
      "type": "computer_20251124",
      "name": "computer",
      "display_width_px": 1280,
      "display_height_px": 800,
      "enable_zoom": true
    }
    betas: computer-use-2025-11-24
    == claude-sonnet-4-5 (computer_20250124)
    {
      "name": "bash",
      "description": "Run a shell command with bash in the working directory. Returns combined stdout and stderr (interleaved) and the exit code if non-zero. Long output keeps its beginning and end, and the full output is saved to a file you can read. Do not use for reading or editing files; use the dedicated tools. A command still running after a while (about 30 seconds by default) is moved to the background as a job, without being restarted: the call returns with its job id and output so far, and its report arrives when it exits."
    }
    {
      "type": "computer_20250124",
      "name": "computer",
      "display_width_px": 1280,
      "display_height_px": 800
    }
    betas: computer-use-2025-01-24
    |}]
;;

let anthropic_parse ?computer events =
  let events, _, _, _ =
    A.parse_events ?computer ~tools:[ bash_spec; computer_spec ] events
  in
  let builder = Assistant_builder.create ~model:"m" in
  List.iter events ~f:(Assistant_builder.apply builder);
  List.filter_map
    (Assistant_builder.finish builder ~stop_reason:Tool_use ~usage:Usage.zero)
      .content
    ~f:(function
    | Tool_call c -> Some c
    | _ -> None)
;;

let tool_use ?(toolset = "") ~index ~id ~name input =
  [ sprintf
      {|{"type":"content_block_start","index":%d,"content_block":{"type":"tool_use","id":"%s","name":"%s"%s,"input":{}}}|}
      index
      id
      name
      (if String.is_empty toolset
       then ""
       else sprintf {|,"toolset_name":"%s"|} toolset)
  ; sprintf
      {|{"type":"content_block_delta","index":%d,"delta":{"type":"input_json_delta","partial_json":%s}}|}
      index
      (Json.to_string (`String input))
  ; sprintf {|{"type":"content_block_stop","index":%d}|} index
  ]
;;

let show_calls calls =
  List.iter calls ~f:(fun (c : Content.Tool_call.t) ->
    printf "%s %s: %s\n" c.id c.name c.arguments;
    match
      Computer_action.of_json (Or_error.ok_exn (Json.parse c.arguments))
    with
    | Ok action -> printf "  runs: %s\n" (Computer_action.describe action)
    | Error e -> printf "  error: %s\n" (Error.to_string_hum e))
;;

let%expect_test "Anthropic toolset: calls are stored as the tool's own actions" =
  let calls =
    anthropic_parse
      ~computer:Anthropic_toolset
      (List.concat
         [ tool_use
             ~toolset:"computer"
             ~index:0
             ~id:"t1"
             ~name:"screenshot"
             "{}"
         ; tool_use
             ~toolset:"computer"
             ~index:1
             ~id:"t2"
             ~name:"left_click"
             {|{"coordinate":[10,20],"text":"ctrl"}|}
         ; tool_use
             ~toolset:"computer"
             ~index:2
             ~id:"t3"
             ~name:"type"
             {|{"text":"hi"}|}
         ; tool_use
             ~toolset:"computer"
             ~index:3
             ~id:"t4"
             ~name:"key"
             {|{"text":"ctrl+s"}|}
         ; tool_use
             ~toolset:"computer"
             ~index:4
             ~id:"t5"
             ~name:"scroll"
             {|{"coordinate":[5,5],"scroll_direction":"down","scroll_amount":3}|}
         ; tool_use
             ~toolset:"computer"
             ~index:5
             ~id:"t6"
             ~name:"left_click_drag"
             {|{"start_coordinate":[1,2],"coordinate":[3,4]}|}
         ; tool_use
             ~toolset:"computer"
             ~index:6
             ~id:"t7"
             ~name:"zoom"
             {|{"region":[0,0,100,50]}|}
         ; tool_use
             ~toolset:"computer"
             ~index:7
             ~id:"t8"
             ~name:"wait"
             {|{"duration":1}|}
         ; tool_use ~index:8 ~id:"t9" ~name:"bash" {|{"cmd":"ls"}|}
         ])
  in
  show_calls calls;
  [%expect
    {|
    t1 computer: {"action":"screenshot","native":{"form":"anthropic_toolset","name":"screenshot","input":{}}}
      runs: screenshot
    t2 computer: {"action":"click","x":10,"y":20,"button":"left","count":1,"keys":"ctrl","native":{"form":"anthropic_toolset","name":"left_click","input":{"coordinate":[10,20],"text":"ctrl"}}}
      runs: ctrl+click (10, 20)
    t3 computer: {"action":"type","text":"hi","native":{"form":"anthropic_toolset","name":"type","input":{"text":"hi"}}}
      runs: type “hi”
    t4 computer: {"action":"key","combo":"ctrl+s","native":{"form":"anthropic_toolset","name":"key","input":{"text":"ctrl+s"}}}
      runs: key ctrl+s
    t5 computer: {"action":"scroll","x":5,"y":5,"dx":0,"dy":3,"native":{"form":"anthropic_toolset","name":"scroll","input":{"coordinate":[5,5],"scroll_direction":"down","scroll_amount":3}}}
      runs: scroll ↓3 at (5, 5)
    t6 computer: {"action":"drag","path":[[1,2],[3,4]],"button":"left","native":{"form":"anthropic_toolset","name":"left_click_drag","input":{"start_coordinate":[1,2],"coordinate":[3,4]}}}
      runs: drag (1, 2) → (3, 4)
    t7 computer: {"action":"zoom","region":[0,0,100,50],"native":{"form":"anthropic_toolset","name":"zoom","input":{"region":[0,0,100,50]}}}
      runs: zoom [0, 0, 100, 50]
    t8 computer: {"action":"wait","seconds":1,"native":{"form":"anthropic_toolset","name":"wait","input":{"duration":1}}}
      runs: wait 1s
    t9 bash: {"cmd":"ls"}
      error: missing "action": one of screenshot, click, move, drag, mouse_down, mouse_up, scroll, type, key, hold_key, wait, zoom, cursor_position
    |}]
;;

let%expect_test
    "Anthropic computer_2025: calls are stored as the tool's own actions"
  =
  let calls =
    anthropic_parse
      ~computer:
        (Option.value_exn (model "anthropic/claude-opus-4-6").native_computer)
      (List.concat
         [ tool_use
             ~index:0
             ~id:"t1"
             ~name:"computer"
             {|{"action":"double_click","coordinate":[7,8]}|}
         ; tool_use
             ~index:1
             ~id:"t2"
             ~name:"computer"
             {|{"action":"mouse_move","coordinate":[1,1]}|}
         ; tool_use
             ~index:2
             ~id:"t3"
             ~name:"computer"
             {|{"action":"hold_key","text":"shift","duration":1}|}
         ])
  in
  show_calls calls;
  [%expect
    {|
    t1 computer: {"action":"click","x":7,"y":8,"button":"left","count":2,"native":{"form":"anthropic","input":{"action":"double_click","coordinate":[7,8]}}}
      runs: double-click (7, 8)
    t2 computer: {"action":"move","x":1,"y":1,"native":{"form":"anthropic","input":{"action":"mouse_move","coordinate":[1,1]}}}
      runs: move (1, 1)
    t3 computer: {"action":"hold_key","combo":"shift","duration":1,"native":{"form":"anthropic","input":{"action":"hold_key","text":"shift","duration":1}}}
      runs: hold shift for 1s
    |}]
;;

let computer_turn ~args =
  [ Message.user "go"
  ; Assistant
      { content =
          [ Content.thinking ~signature:"SIG" "plan"
          ; Tool_call { id = "t1"; name = "computer"; arguments = args }
          ]
      ; model = "anthropic/claude-opus-5-5"
      ; usage = Usage.zero
      ; stop_reason = Tool_use
      }
  ; Tool_result
      { tool_call_id = "t1"
      ; tool_name = "computer"
      ; text = "ok"
      ; is_error = false
      ; images = [ { mime_type = "image/png"; data = "AAAA" } ]
      }
  ]
;;

let anthropic_replay m messages =
  let body = A.request_body ~oauth:false (request m messages) in
  List.iter (list body "messages") ~f:(fun msg ->
    List.iter (list msg "content") ~f:(fun block ->
      match field block "type" with
      | Some (`String ("tool_use" | "tool_result")) ->
        print_json
          (`Object
              (List.filter_map
                 (match block with
                  | `Object f -> f
                  | _ -> [])
                 ~f:(fun (k, v) ->
                   match k with
                   | "content" -> Some (k, `String "...")
                   | _ -> Some (k, v))))
      | _ -> ()))
;;

let%expect_test
    "Anthropic: replayed as written to the same form, translated to another"
  =
  let stored =
    Computer_native.of_anthropic
      ~toolset:true
      ~name:"left_click"
      (Or_error.ok_exn (Json.parse {|{"coordinate":[10,20]}|}))
  in
  print_endline stored;
  print_endline "== to the toolset";
  anthropic_replay "anthropic/claude-opus-5-5" (computer_turn ~args:stored);
  print_endline "== to computer_20251124";
  anthropic_replay "anthropic/claude-opus-4-6" (computer_turn ~args:stored);
  print_endline "== to a plain tool (OpenAI generic)";
  print_endline (Computer_native.generic stored);
  print_endline "== a generic call to the toolset";
  anthropic_replay
    "anthropic/claude-opus-5-5"
    (computer_turn ~args:{|{"action":"click","x":3,"y":4,"button":"right"}|});
  [%expect
    {|
    {"action":"click","x":10,"y":20,"button":"left","count":1,"native":{"form":"anthropic_toolset","name":"left_click","input":{"coordinate":[10,20]}}}
    == to the toolset
    {
      "type": "tool_use",
      "id": "t1",
      "name": "left_click",
      "toolset_name": "computer",
      "input": {
        "coordinate": [
          10,
          20
        ]
      },
      "cache_control": {
        "type": "ephemeral"
      }
    }
    {
      "type": "tool_result",
      "tool_use_id": "t1",
      "content": "...",
      "is_error": false,
      "toolset_name": "computer",
      "cache_control": {
        "type": "ephemeral"
      }
    }
    == to computer_20251124
    {
      "type": "tool_use",
      "id": "t1",
      "name": "computer",
      "input": {
        "action": "left_click",
        "coordinate": [
          10,
          20
        ]
      },
      "cache_control": {
        "type": "ephemeral"
      }
    }
    {
      "type": "tool_result",
      "tool_use_id": "t1",
      "content": "...",
      "is_error": false,
      "cache_control": {
        "type": "ephemeral"
      }
    }
    == to a plain tool (OpenAI generic)
    {"action":"click","x":10,"y":20,"button":"left","count":1}
    == a generic call to the toolset
    {
      "type": "tool_use",
      "id": "t1",
      "name": "right_click",
      "toolset_name": "computer",
      "input": {
        "coordinate": [
          3,
          4
        ]
      },
      "cache_control": {
        "type": "ephemeral"
      }
    }
    {
      "type": "tool_result",
      "tool_use_id": "t1",
      "content": "...",
      "is_error": false,
      "toolset_name": "computer",
      "cache_control": {
        "type": "ephemeral"
      }
    }
    |}]
;;

let%expect_test
    "Anthropic: cache breakpoints after the tools, system and stable prefix"
  =
  let messages =
    List.concat
      [ computer_turn ~args:{|{"action":"screenshot"}|}
      ; [ Message.Assistant
            { content = [ Text "done" ]
            ; model = "anthropic/claude-opus-5-5"
            ; usage = Usage.zero
            ; stop_reason = End_turn
            }
        ; Message.user "again"
        ]
      ]
  in
  let body =
    A.request_body ~oauth:false (request "anthropic/claude-opus-5-5" messages)
  in
  let rec marks path (j : Json.t) =
    match j with
    | `Object fields ->
      List.iter fields ~f:(fun (k, v) ->
        if String.equal k "cache_control"
        then printf "%s\n" path
        else marks (path ^ "." ^ k) v)
    | `Array l -> List.iteri l ~f:(fun i v -> marks (sprintf "%s[%d]" path i) v)
    | _ -> ()
  in
  marks "" body;
  [%expect
    {|
    .system[0]
    .messages[1].content[1]
    .messages[2].content[0]
    .messages[4].content[0]
    |}]
;;

(* --- OpenAI --- *)

let openai = Openai_responses.Endpoint.Openai { api_key = "k" }

let openai_parse items =
  let events, _, _ =
    O.parse_events
      (List.mapi items ~f:(fun i item ->
         sprintf
           {|{"type":"response.output_item.done","output_index":%d,"item":%s}|}
           i
           item)
       @ [ {|{"type":"response.completed","response":{"usage":{"input_tokens":1,"output_tokens":1}}}|}
         ])
  in
  let builder = Assistant_builder.create ~model:"m" in
  List.iter events ~f:(Assistant_builder.apply builder);
  List.filter_map
    (Assistant_builder.finish builder ~stop_reason:Tool_use ~usage:Usage.zero)
      .content
    ~f:(function
    | Tool_call c -> Some c
    | _ -> None)
;;

let%expect_test "OpenAI: the tool offered" =
  let body =
    O.request_body
      ~endpoint:openai
      (request "openai/gpt-6.1-sol" [ Message.user "go" ])
  in
  List.iter (list body "tools") ~f:(fun t ->
    print_s
      [%sexp (field t "type" : Json.t option), (field t "name" : Json.t option)]);
  print_endline "== Codex: a function tool";
  let body =
    O.request_body
      ~endpoint:(Codex { access_token = "t"; account_id = "a" })
      (request "openai-codex/gpt-6.1-sol" [ Message.user "go" ])
  in
  List.iter (list body "tools") ~f:(fun t ->
    print_s
      [%sexp (field t "type" : Json.t option), (field t "name" : Json.t option)]);
  [%expect
    {|
    (((String function)) ((String bash)))
    (((String computer)) ())
    == Codex: a function tool
    (((String function)) ((String bash)))
    (((String function)) ((String computer)))
    |}]
;;

let batch =
  {|{"type":"computer_call","id":"cu_1","call_id":"call_1","status":"completed","actions":[{"type":"click","button":"left","x":10,"y":20},{"type":"type","text":"hi"},{"type":"keypress","keys":["CTRL","S"]},{"type":"scroll","x":5,"y":5,"scroll_x":0,"scroll_y":300},{"type":"drag","path":[{"x":1,"y":2},{"x":3,"y":4}]},{"type":"double_click","x":1,"y":1},{"type":"move","x":2,"y":2},{"type":"wait"},{"type":"screenshot"}],"pending_safety_checks":[{"id":"sc_1","code":"malicious_instructions","message":"Careful"}]}|}
;;

let%expect_test "OpenAI: a batch of actions becomes one call per action" =
  let calls = openai_parse [ batch ] in
  show_calls calls;
  print_s
    [%sexp
      (Computer_native.safety_checks
         (Or_error.ok_exn (Json.parse (List.hd_exn calls).arguments))
       : Computer_native.Safety_check.t list)];
  [%expect
    {|
    call_1 computer: {"action":"click","x":10,"y":20,"button":"left","count":1,"native":{"form":"openai","id":"cu_1","action":{"type":"click","button":"left","x":10,"y":20}},"safety_checks":[{"id":"sc_1","code":"malicious_instructions","message":"Careful"}]}
      runs: click (10, 20)
    call_1__1 computer: {"action":"type","text":"hi","native":{"form":"openai","action":{"type":"type","text":"hi"}}}
      runs: type “hi”
    call_1__2 computer: {"action":"key","combo":"CTRL+s","native":{"form":"openai","action":{"type":"keypress","keys":["CTRL","S"]}}}
      runs: key CTRL+s
    call_1__3 computer: {"action":"scroll","x":5,"y":5,"dx":0,"dy":3,"native":{"form":"openai","action":{"type":"scroll","x":5,"y":5,"scroll_x":0,"scroll_y":300}}}
      runs: scroll ↓3 at (5, 5)
    call_1__4 computer: {"action":"drag","path":[[1,2],[3,4]],"button":"left","native":{"form":"openai","action":{"type":"drag","path":[{"x":1,"y":2},{"x":3,"y":4}]}}}
      runs: drag (1, 2) → (3, 4)
    call_1__5 computer: {"action":"click","x":1,"y":1,"button":"left","count":2,"native":{"form":"openai","action":{"type":"double_click","x":1,"y":1}}}
      runs: double-click (1, 1)
    call_1__6 computer: {"action":"move","x":2,"y":2,"native":{"form":"openai","action":{"type":"move","x":2,"y":2}}}
      runs: move (2, 2)
    call_1__7 computer: {"action":"wait","seconds":2,"native":{"form":"openai","action":{"type":"wait"}}}
      runs: wait 2s
    call_1__8 computer: {"action":"screenshot","native":{"form":"openai","action":{"type":"screenshot"}}}
      runs: screenshot
    (((id sc_1) (code (malicious_instructions)) (message (Careful))))
    |}]
;;

let%expect_test
    "OpenAI: replayed as one computer_call and one computer_call_output"
  =
  let calls = openai_parse [ batch ] in
  let results =
    List.mapi calls ~f:(fun i (c : Content.Tool_call.t) ->
      Message.Tool_result
        { tool_call_id = c.id
        ; tool_name = c.name
        ; text = sprintf "done %d" i
        ; is_error = i = 2
        ; images =
            (if i = 3
             then [ { mime_type = "image/png"; data = "LAST" } ]
             else [])
        })
  in
  let messages =
    Message.user "go"
    :: Message.Assistant
         { content = List.map calls ~f:(fun c -> Content.Tool_call c)
         ; model = "openai/gpt-6.1-sol"
         ; usage = Usage.zero
         ; stop_reason = Tool_use
         }
    :: results
  in
  let body =
    O.request_body ~endpoint:openai (request "openai/gpt-6.1-sol" messages)
  in
  List.iter (list body "input") ~f:print_json;
  print_endline "== to Anthropic (translated, one tool_use per action)";
  anthropic_replay "anthropic/claude-opus-4-6" messages;
  [%expect
    {|
    {
      "type": "message",
      "role": "user",
      "content": [
        {
          "type": "input_text",
          "text": "go"
        }
      ]
    }
    {
      "type": "computer_call",
      "id": "cu_1",
      "call_id": "call_1",
      "actions": [
        {
          "type": "click",
          "button": "left",
          "x": 10,
          "y": 20
        },
        {
          "type": "type",
          "text": "hi"
        },
        {
          "type": "keypress",
          "keys": [
            "CTRL",
            "S"
          ]
        },
        {
          "type": "scroll",
          "x": 5,
          "y": 5,
          "scroll_x": 0,
          "scroll_y": 300
        },
        {
          "type": "drag",
          "path": [
            {
              "x": 1,
              "y": 2
            },
            {
              "x": 3,
              "y": 4
            }
          ]
        },
        {
          "type": "double_click",
          "x": 1,
          "y": 1
        },
        {
          "type": "move",
          "x": 2,
          "y": 2
        },
        {
          "type": "wait"
        },
        {
          "type": "screenshot"
        }
      ],
      "pending_safety_checks": [
        {
          "id": "sc_1",
          "code": "malicious_instructions",
          "message": "Careful"
        }
      ],
      "status": "completed"
    }
    {
      "type": "computer_call_output",
      "call_id": "call_1",
      "output": {
        "type": "computer_screenshot",
        "image_url": "data:image/png;base64,LAST",
        "detail": "original"
      },
      "acknowledged_safety_checks": [
        {
          "id": "sc_1",
          "code": "malicious_instructions",
          "message": "Careful"
        }
      ]
    }
    {
      "type": "message",
      "role": "user",
      "content": [
        {
          "type": "input_text",
          "text": "computer: done 2"
        }
      ]
    }
    == to Anthropic (translated, one tool_use per action)
    {
      "type": "tool_use",
      "id": "call_1",
      "name": "computer",
      "input": {
        "action": "left_click",
        "coordinate": [
          10,
          20
        ]
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__1",
      "name": "computer",
      "input": {
        "action": "type",
        "text": "hi"
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__2",
      "name": "computer",
      "input": {
        "action": "key",
        "text": "CTRL+s"
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__3",
      "name": "computer",
      "input": {
        "action": "scroll",
        "coordinate": [
          5,
          5
        ],
        "scroll_direction": "down",
        "scroll_amount": 3
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__4",
      "name": "computer",
      "input": {
        "action": "left_click_drag",
        "start_coordinate": [
          1,
          2
        ],
        "coordinate": [
          3,
          4
        ]
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__5",
      "name": "computer",
      "input": {
        "action": "double_click",
        "coordinate": [
          1,
          1
        ]
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__6",
      "name": "computer",
      "input": {
        "action": "mouse_move",
        "coordinate": [
          2,
          2
        ]
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__7",
      "name": "computer",
      "input": {
        "action": "wait",
        "duration": 2
      }
    }
    {
      "type": "tool_use",
      "id": "call_1__8",
      "name": "computer",
      "input": {
        "action": "screenshot"
      }
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1",
      "content": "...",
      "is_error": false
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__1",
      "content": "...",
      "is_error": false
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__2",
      "content": "...",
      "is_error": true,
      "cache_control": {
        "type": "ephemeral"
      }
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__3",
      "content": "...",
      "is_error": false
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__4",
      "content": "...",
      "is_error": false
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__5",
      "content": "...",
      "is_error": false
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__6",
      "content": "...",
      "is_error": false
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__7",
      "content": "...",
      "is_error": false
    }
    {
      "type": "tool_result",
      "tool_use_id": "call_1__8",
      "content": "...",
      "is_error": false,
      "cache_control": {
        "type": "ephemeral"
      }
    }
    |}]
;;

let%expect_test "a call with OpenAI safety checks needs approval in Ask mode" =
  let needs_approval args =
    match computer_spec.destructive with
    | Depends f -> f (Or_error.ok_exn (Json.parse args))
    | Always -> true
    | Never -> false
  in
  let action = Or_error.ok_exn (Json.parse {|{"type":"click","x":1,"y":2}|}) in
  print_s
    [%sexp
      (needs_approval (Computer_native.of_openai action) : bool)
    , (needs_approval
         (Computer_native.of_openai
            ~safety_checks:
              [ Or_error.ok_exn
                  (Json.parse {|{"id":"sc","code":"irrelevant_domain"}|})
              ]
            action)
       : bool)];
  [%expect {| (false true) |}]
;;
