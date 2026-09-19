open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let login_manager t ~sw =
  Login_manager.create
    ~env:t.env
    ~sw
    ~getenv:(fun _ -> None)
    ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
    ()
;;

let with_agent replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let login = login_manager t ~sw in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create replies)
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  f t agent login
;;

let call t agent login ?(params = "{}") meth =
  let request =
    Json.of_string
      (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params)
  in
  print_endline
    (mask t (Json.to_string (Rpc_server.handle agent login request)))
;;

let%expect_test "state, models, thinking, errors" =
  with_agent []
  @@ fun t agent login ->
  call t agent login "ping";
  call t agent login "get_state";
  call t agent login ~params:{|{"thinking": "high"}|} "set_thinking";
  call t agent login ~params:{|{"thinking": "sideways"}|} "set_thinking";
  call t agent login ~params:{|{"model": "deepseek-v4-pro"}|} "set_model";
  call t agent login ~params:{|{"model": "gpt-9"}|} "set_model";
  call t agent login ~params:{|{"model": "GPT-5.5"}|} "set_model";
  call t agent login ~params:{|{"model": "deepseek v4 pro"}|} "set_model";
  call t agent login ~params:{|{"model": "claude fabl"}|} "set_model";
  call t agent login ~params:{|{"model": "Claude Fable 5.1"}|} "set_model";
  call t agent login ~params:{|{"model": "deepseek-v4-pro"}|} "set_model";
  call t agent login "get_state";
  call t agent login "nope";
  call t agent login ~params:{|{"text": 5}|} "prompt";
  print_endline
    (Json.to_string
       (Rpc_server.handle agent login (Json.of_string {|{"id": 7}|})));
  print_endline
    (Json.to_string (Rpc_server.handle agent login (Json.of_string {|[1]|})));
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":"pong"}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"thinking must be one of: off, on, low, high, max"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"unknown model \"gpt-9\"; did you mean: openai/gpt-4 (GPT-4), openai/gpt-5 (GPT-5), openai/gpt-4o (GPT-4o)"}
    {"type":"response","id":"r1","ok":false,"error":"model \"GPT-5.5\" is ambiguous; one of: openai/gpt-5.5, openai-codex/gpt-5.5"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"model \"claude fabl\" is ambiguous; one of: anthropic/claude-fable-5, anthropic/claude-fable-5-1"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-v4-pro","provider":"deepseek","key":"deepseek/deepseek-v4-pro","name":"DeepSeek V4 Pro","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":1.32,"output":3.96,"cache_read":0.044}},"thinking":"high","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    {"type":"response","id":"r1","ok":false,"error":"unknown method \"nope\""}
    {"type":"response","id":"r1","ok":false,"error":"param \"text\" must be a string"}
    {"type":"response","id":7,"ok":false,"error":"request must have a string \"method\""}
    {"type":"response","id":null,"ok":false,"error":"request must have a string \"method\""}
    |}]
;;

let%expect_test "prompt emits events, get_messages/get_entries reflect the run" =
  with_agent
    [ Reply.tool_call ~text:"Looking." ~id:"c1" ~name:"ls" ~arguments:"{}" ()
    ; Reply.text "Empty."
    ]
  @@ fun t agent login ->
  let events = Queue.create () in
  Agent.subscribe agent ~f:(fun e -> Queue.enqueue events (Rpc_json.event e));
  call t agent login ~params:{|{"text": "what is here?"}|} "prompt";
  call t agent login ~params:{|{"text": "again"}|} "prompt";
  Agent.wait_idle agent;
  Queue.iter events ~f:(fun e -> print_endline (mask t (Json.to_string e)));
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"a run is already in progress; use steer or follow_up"}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":true,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    {"type":"event","event":"agent_start"}
    {"type":"event","event":"message_start","message":{"role":"user","text":"what is here?"}}
    {"type":"event","event":"message_end","message":{"role":"user","text":"what is here?"}}
    {"type":"event","event":"turn_start"}
    {"type":"event","event":"message_start","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}}
    {"type":"event","event":"message_update","partial":{"role":"assistant","content":[{"type":"text","text":"Looking."}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"},"delta":{"type":"text_delta","text":"Looking."}}
    {"type":"event","event":"message_update","partial":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":""}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"},"delta":{"type":"tool_call_start","index":0,"id":"c1","name":"ls"}}
    {"type":"event","event":"message_update","partial":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"},"delta":{"type":"tool_call_delta","index":0,"arguments":"{}"}}
    {"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"}}
    {"type":"event","event":"tool_start","call":{"id":"c1","name":"ls","arguments":"{}"}}
    {"type":"event","event":"tool_end","call":{"id":"c1","name":"ls","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false}}
    {"type":"event","event":"message_start","message":{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false}}
    {"type":"event","event":"message_end","message":{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false}}
    {"type":"event","event":"turn_end","assistant":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"},"tool_results":[{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false}]}
    {"type":"event","event":"turn_start"}
    {"type":"event","event":"message_start","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}}
    {"type":"event","event":"message_update","partial":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"},"delta":{"type":"text_delta","text":"Empty."}}
    {"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"}}
    {"type":"event","event":"turn_end","assistant":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"},"tool_results":[]}
    {"type":"event","event":"agent_end","messages":[{"role":"user","text":"what is here?"},{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"},{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false},{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"}]}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":4,"usage":{"input":30,"output":13,"cache_read":5},"cost_usd":2.313e-05,"context_tokens":10}}
    |}];
  call t agent login "get_messages";
  call t agent login "get_entries";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":[{"role":"user","text":"what is here?"},{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"},{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false},{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"}]}
    {"type":"response","id":"r1","ok":true,"result":{"head":"<id>","entries":[{"id":"<id>","parent":null,"kind":"message","message":{"role":"user","text":"what is here?"}},{"id":"<id>","parent":"<id>","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"}},{"id":"<id>","parent":"<id>","kind":"message","message":{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false}},{"id":"<id>","parent":"<id>","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"}}]}}
    |}]
;;

let%expect_test "prompt attachments are inlined into the user message" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t "a.txt" "hello\nworld\n";
  Core_unix.mkdir_p (Filename.concat t.dir "dir");
  let login = login_manager t ~sw in
  let on_request (request : Provider.Request.t) =
    List.iter request.messages ~f:(function
      | Message.User u -> print_endline (mask t u.text)
      | _ -> ())
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create ~on_request [ Reply.text "ok" ])
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  call
    t
    agent
    login
    ~params:{|{"text": "look", "attachments": ["a.txt", "missing.txt", "dir"]}|}
    "prompt";
  Agent.wait_idle agent;
  [%expect
    {|
    look

    <file path="a.txt">
    hello
    world
    </file>

    <file path="missing.txt" error="file not found: $DIR/missing.txt"/>

    <file path="dir" error="$DIR/dir is a directory; use ls"/>
    {"type":"response","id":"r1","ok":true,"result":{}}
    |}]
;;

let%expect_test "sessions: list, new, switch, fork, rewind" =
  with_agent [ Reply.text "one"; Reply.text "two" ]
  @@ fun t agent login ->
  call t agent login ~params:{|{"text": "first"}|} "prompt";
  Agent.wait_idle agent;
  let first = (Agent.state agent).session_path in
  call t agent login "new_session";
  call t agent login "list_sessions";
  call t agent login ~params:(sprintf {|{"path": "%s"}|} first) "switch_session";
  call t agent login ~params:{|{"path": "/nowhere.jsonl"}|} "switch_session";
  call t agent login "fork";
  call t agent login ~params:{|{"to": "bogus"}|} "rewind";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":null,"cwd":"$DIR","created_at":"<time>","updated_at":"<time>","first_prompt":"first","message_count":2,"parent":null},{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":null,"cwd":"$DIR","created_at":"<time>","updated_at":"<time>","first_prompt":null,"message_count":0,"parent":null}]}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"(Sys_error \"/nowhere.jsonl: No such file or directory\")"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"(\"no such entry\" (to_ bogus))"}
    |}]
;;

let%expect_test
    "end to end over pipes: request/response framing, invalid JSON, EOF aborts"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:
        (Faux_provider.create
           [ Reply.tool_call
               ~id:"c1"
               ~name:"bash"
               ~arguments:{|{"command":"sleep 5"}|}
               ()
           ])
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  let login = login_manager t ~sw in
  let in_r, in_w = Eio_unix.pipe sw in
  let out_r, out_w = Eio_unix.pipe sw in
  let output = Buffer.create 4096 in
  Eio.Fiber.all
    [ (fun () ->
        Rpc_server.run ~env:t.env ~agent ~login ~input:in_r ~output:out_w;
        Eio.Flow.close out_w)
    ; (fun () ->
        Eio.Flow.copy_string
          "{\"id\":1,\"method\":\"ping\"}\n\
           not json\n\n\
           {\"id\":2,\"method\":\"prompt\",\"params\":{\"text\":\"go\"}}\n"
          in_w;
        Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.3;
        Eio.Flow.close in_w)
    ; (fun () ->
        let reader = Eio.Buf_read.of_flow out_r ~max_size:(1024 * 1024) in
        try
          while true do
            Buffer.add_string output (Eio.Buf_read.line reader ^ "\n")
          done
        with
        | End_of_file -> ())
    ];
  let lines = String.split_lines (Buffer.contents output) in
  List.iter lines ~f:(fun line ->
    let json = Json.of_string line in
    let field name =
      Option.value_map (Json.member name json) ~default:"" ~f:Json.to_string
    in
    match field "type" with
    | "\"response\"" ->
      printf
        "response id=%s ok=%s %s\n"
        (field "id")
        (field "ok")
        (field "error")
    | _ -> printf "event %s\n" (field "event"));
  print_s [%sexp (Agent.is_running agent : bool)];
  [%expect
    {|
    response id=1 ok=true
    response id=null ok=false "invalid JSON: json: unexpected string: 'not'"
    event "state"
    event "agent_start"
    event "message_start"
    event "message_end"
    event "turn_start"
    event "message_start"
    response id=2 ok=true
    event "message_update"
    event "message_update"
    event "message_end"
    event "tool_start"
    event "queue_update"
    event "tool_end"
    event "message_start"
    event "message_end"
    event "turn_end"
    event "agent_end"
    event "state"
    false
    |}]
;;

let%expect_test "abort returns restored messages and emits queue_update" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 5"}|}
        ()
    ]
  @@ fun t agent login ->
  let events = Queue.create () in
  Agent.subscribe agent ~f:(fun e -> Queue.enqueue events (Rpc_json.event e));
  call t agent login ~params:{|{"text": "go"}|} "prompt";
  call t agent login ~params:{|{"text": "first steer"}|} "steer";
  call t agent login ~params:{|{"text": "second follow up"}|} "follow_up";
  call t agent login "abort";
  Agent.wait_idle agent;
  Queue.iter events ~f:(fun e -> print_endline (mask t (Json.to_string e)));
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"restored":["first steer","second follow up"]}}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":true,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    {"type":"event","event":"agent_start"}
    {"type":"event","event":"message_start","message":{"role":"user","text":"go"}}
    {"type":"event","event":"message_end","message":{"role":"user","text":"go"}}
    {"type":"event","event":"turn_start"}
    {"type":"event","event":"message_start","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}}
    {"type":"event","event":"queue_update","steer":1,"follow_up":0}
    {"type":"event","event":"queue_update","steer":1,"follow_up":1}
    {"type":"event","event":"queue_update","steer":0,"follow_up":0}
    {"type":"event","event":"message_end","message":{"role":"assistant","content":[],"stop_reason":{"type":"aborted"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"}}
    {"type":"event","event":"turn_end","assistant":{"role":"assistant","content":[],"stop_reason":{"type":"aborted"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"},"tool_results":[]}
    {"type":"event","event":"agent_end","messages":[{"role":"user","text":"go"},{"role":"assistant","content":[],"stop_reason":{"type":"aborted"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"}]}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":2,"usage":{"input":20,"output":8,"cache_read":5},"cost_usd":1.413e-05,"context_tokens":20}}
    |}]
;;

let%expect_test "auth: status, login via prompt, logout" =
  with_agent []
  @@ fun t agent login ->
  Login_manager.subscribe login ~f:(fun e ->
    print_endline (Json.to_string (Rpc_json.login_event e)));
  call t agent login "auth_status";
  call t agent login ~params:{|{"provider": "groq"}|} "login";
  call
    t
    agent
    login
    ~params:{|{"provider": "deepseek", "method": "oauth"}|}
    "login";
  call t agent login ~params:{|{"provider": "deepseek"}|} "login";
  call t agent login ~params:{|{"id": "p1", "value": "sk-rpc"}|} "auth_respond";
  Login_manager.wait login;
  call t agent login "auth_status";
  call
    t
    agent
    login
    ~params:{|{"provider": "anthropic", "method": "api_key"}|}
    "login";
  call t agent login "auth_cancel";
  Login_manager.wait login;
  call t agent login ~params:{|{"provider": "deepseek"}|} "logout";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":[{"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Anthropic (Claude Pro/Max)"},{"method":"api_key","label":"Anthropic API key"}],"configured":null,"expires_ms":null},{"provider":"openai","name":"OpenAI","methods":[{"method":"api_key","label":"OpenAI API key"}],"configured":null,"expires_ms":null},{"provider":"openai-codex","name":"OpenAI Codex (ChatGPT)","methods":[{"method":"oauth","label":"OpenAI (ChatGPT Plus/Pro)"}],"configured":null,"expires_ms":null},{"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"DeepSeek API key"}],"configured":null,"expires_ms":null}]}
    {"type":"response","id":"r1","ok":false,"error":"unknown provider \"groq\" (one of: anthropic, openai, openai-codex, deepseek)"}
    {"type":"response","id":"r1","ok":false,"error":"(\"login method not supported by provider\" (provider Deepseek)\n (method_ Oauth))"}
    {"type":"event","event":"auth","kind":"prompt","id":"p1","prompt":"secret","message":"Enter DeepSeek API key"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"done","provider":"deepseek","method":"api_key"}
    {"type":"response","id":"r1","ok":true,"result":[{"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Anthropic (Claude Pro/Max)"},{"method":"api_key","label":"Anthropic API key"}],"configured":null,"expires_ms":null},{"provider":"openai","name":"OpenAI","methods":[{"method":"api_key","label":"OpenAI API key"}],"configured":null,"expires_ms":null},{"provider":"openai-codex","name":"OpenAI Codex (ChatGPT)","methods":[{"method":"oauth","label":"OpenAI (ChatGPT Plus/Pro)"}],"configured":null,"expires_ms":null},{"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"DeepSeek API key"}],"configured":{"method":"api_key","source":"stored api key"},"expires_ms":null}]}
    {"type":"event","event":"auth","kind":"prompt","id":"p2","prompt":"secret","message":"Enter Anthropic API key"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"failed","provider":"anthropic","error":"login cancelled"}
    {"type":"event","event":"auth","kind":"logged_out","provider":"deepseek"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    |}]
;;

let%expect_test "session stats" =
  with_agent
    [ Reply.tool_call ~text:"Looking." ~id:"c1" ~name:"ls" ~arguments:"{}" ()
    ; Reply.text "Empty."
    ]
  @@ fun t agent login ->
  call t agent login ~params:{|{"model": "deepseek-v4-pro"}|} "set_model";
  call t agent login ~params:{|{"text": "what is here?"}|} "prompt";
  Agent.wait_idle agent;
  call t agent login "session_stats";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"message_count":4,"turns":2,"tool_calls":{"ls":1},"usage":{"input":30,"output":13,"cache_read":5},"cost_usd":8.47e-05,"context_percent":0.001,"model_changes":1,"compactions":0,"duration_seconds":<t>}}
    |}]
;;

let%expect_test "import, set_session_name, export, clone, set_cwd" =
  with_agent [ Reply.text "unused" ]
  @@ fun t agent login ->
  let external_dir = Filename.concat t.dir "external" in
  let src = Session.create ~dir:external_dir ~cwd:t.dir () in
  let (_ : Session.Entry.t) =
    Session.append_message src (Message.user "imported question")
  in
  call
    t
    agent
    login
    ~params:(sprintf {|{"path": "%s"}|} (Session.path src))
    "import";
  call t agent login "get_state";
  call t agent login ~params:{|{"name": "imported"}|} "set_session_name";
  call t agent login "get_state";
  call t agent login ~params:{|{"format": "markdown"}|} "export";
  call t agent login ~params:{|{"format": "jsonl"}|} "export";
  call t agent login ~params:{|{"format": "html"}|} "export";
  call t agent login ~params:{|{"path": "/nowhere.jsonl"}|} "import";
  call t agent login "clone";
  let sub = Filename.concat t.dir "sub" in
  Core_unix.mkdir_p sub;
  call t agent login ~params:(sprintf {|{"path": "%s"}|} sub) "set_cwd";
  call t agent login "get_state";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"path":"$DIR/sessions/<stamp>_<id>.jsonl"}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":"imported","cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    {"type":"response","id":"r1","ok":true,"result":{"path":"$DIR/sessions/exports/<stamp>_<id>.md"}}
    {"type":"response","id":"r1","ok":true,"result":{"path":"$DIR/sessions/exports/<stamp>_<id>.jsonl"}}
    {"type":"response","id":"r1","ok":false,"error":"unknown export format \"html\" (expected markdown or jsonl)"}
    {"type":"response","id":"r1","ok":false,"error":"(Sys_error \"/nowhere.jsonl: No such file or directory\")"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":"imported","cwd":"$DIR/sub","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    |}]
;;

let%expect_test "config: get, set, invalid, and the config_changed event" =
  with_agent []
  @@ fun t agent login ->
  Agent.subscribe agent ~f:(fun e ->
    print_endline (Json.to_string (Rpc_json.event e)));
  call t agent login "get_config";
  call
    t
    agent
    login
    ~params:{|{"config": {"scoped_models": ["a", "b"], "confirm_tools": true}}|}
    "set_config";
  call t agent login "get_config";
  call t agent login ~params:{|{"config": {"scoped_models": "x"}}|} "set_config";
  call t agent login ~params:{|{}|} "set_config";
  call
    t
    agent
    login
    ~params:{|{"call_id": "nope", "allow": true}|}
    "tool_confirm_respond";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"scoped_models":[],"confirm_tools":false}}
    {"type":"event","event":"config_changed","config":{"scoped_models":["a","b"],"confirm_tools":true}}
    {"type":"response","id":"r1","ok":true,"result":{"scoped_models":["a","b"],"confirm_tools":true}}
    {"type":"response","id":"r1","ok":true,"result":{"scoped_models":["a","b"],"confirm_tools":true}}
    {"type":"response","id":"r1","ok":false,"error":"config.scoped_models must be an array of strings"}
    {"type":"response","id":"r1","ok":false,"error":"missing param \"config\""}
    {"type":"response","id":"r1","ok":false,"error":"no pending confirmation for tool call \"nope\""}
    |}]
;;

let%expect_test "shell runs a command, streams tool events, and adds to context"
  =
  with_agent []
  @@ fun t agent login ->
  Agent.subscribe agent ~f:(fun e ->
    print_endline (mask t (Json.to_string (Rpc_json.event e))));
  call
    t
    agent
    login
    ~params:{|{"command": "printf 'a\\nb'", "add_to_context": true}|}
    "shell";
  call t agent login "get_messages";
  [%expect
    {|
    {"type":"event","event":"tool_start","call":{"id":"shell-0","name":"shell","arguments":"{\"command\":\"printf 'a\\\\nb'\"}"}}
    {"type":"event","event":"tool_output","call_id":"shell-0","chunk":"a\nb"}
    {"type":"event","event":"tool_end","call":{"id":"shell-0","name":"shell","arguments":"{\"command\":\"printf 'a\\\\nb'\"}"},"result":{"role":"tool_result","tool_call_id":"shell-0","tool_name":"shell","text":"a\nb","is_error":false}}
    {"type":"event","event":"message_start","message":{"role":"user","text":"$ printf 'a\\nb'\na\nb"}}
    {"type":"event","event":"message_end","message":{"role":"user","text":"$ printf 'a\\nb'\na\nb"}}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}}
    {"type":"response","id":"r1","ok":true,"result":{"text":"a\nb","is_error":false}}
    {"type":"response","id":"r1","ok":true,"result":[{"role":"user","text":"$ printf 'a\\nb'\na\nb"}]}
    |}]
;;

let%expect_test "shell without add_to_context leaves messages unchanged" =
  with_agent []
  @@ fun t agent login ->
  call
    t
    agent
    login
    ~params:{|{"command": "printf 'a\\nb'", "add_to_context": false}|}
    "shell";
  call
    t
    agent
    login
    ~params:{|{"command": "exit 3", "add_to_context": false}|}
    "shell";
  call t agent login "get_messages";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"text":"a\nb","is_error":false}}
    {"type":"response","id":"r1","ok":true,"result":{"text":"[exit code 3]","is_error":true}}
    {"type":"response","id":"r1","ok":true,"result":[]}
    |}]
;;

let%expect_test "shell refuses while a run is in progress" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.3"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun t agent login ->
  call t agent login ~params:{|{"text": "go"}|} "prompt";
  call
    t
    agent
    login
    ~params:{|{"command": "echo hi", "add_to_context": false}|}
    "shell";
  Agent.wait_idle agent;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"cannot run a shell command while a run is in progress"}
    |}]
;;

let%expect_test "dequeue returns queued messages and null when empty" =
  with_agent
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.2"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun t agent login ->
  call t agent login ~params:{|{"text": "go"}|} "prompt";
  call t agent login ~params:{|{"text": "steer one"}|} "steer";
  call t agent login ~params:{|{"text": "follow up"}|} "follow_up";
  call t agent login "dequeue";
  call t agent login "dequeue";
  call t agent login "dequeue";
  Agent.wait_idle agent;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"text":"follow up","attachments":[]}}
    {"type":"response","id":"r1","ok":true,"result":{"text":"steer one","attachments":[]}}
    {"type":"response","id":"r1","ok":true,"result":null}
    |}]
;;
