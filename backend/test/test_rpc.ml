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

(* A server with one connected client; [h.sent] collects what the server
   pushes to that client. *)
module H = struct
  type t =
    { server : Rpc_server.t
    ; client : Rpc_server.Client.t
    ; login : Login_manager.t
    ; sent : Json.t Queue.t
    }
end

let make_server t ~sw ~provider =
  let login = login_manager t ~sw in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let agent = new_agent ~cwd:t.dir () in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~sessions_dir
      ~new_agent
      ~default_agent:agent
      ()
  in
  let sent = Queue.create () in
  let client = Rpc_server.connect server ~send:(Queue.enqueue sent) in
  agent, { H.server; client; login; sent }
;;

let with_agent replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent, h = make_server t ~sw ~provider:(Faux_provider.create replies) in
  f t agent h
;;

let call t (h : H.t) ?(params = "{}") meth =
  let request =
    Json.of_string
      (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params)
  in
  print_endline
    (mask t (Json.to_string (Rpc_server.handle h.server h.client request)))
;;

let current (h : H.t) = Rpc_server.agent_of_client h.server h.client

let%expect_test "state, models, thinking, errors" =
  with_agent []
  @@ fun t _agent h ->
  call t h "ping";
  call t h "get_state";
  call t h ~params:{|{"thinking": "high"}|} "set_thinking";
  call t h ~params:{|{"thinking": "sideways"}|} "set_thinking";
  call t h ~params:{|{"model": "deepseek-v4-pro"}|} "set_model";
  call t h ~params:{|{"model": "gpt-9"}|} "set_model";
  call t h ~params:{|{"model": "GPT-5.5"}|} "set_model";
  call t h ~params:{|{"model": "deepseek v4 pro"}|} "set_model";
  call t h ~params:{|{"model": "claude fabl"}|} "set_model";
  call t h ~params:{|{"model": "Claude Fable 5.1"}|} "set_model";
  call t h ~params:{|{"model": "deepseek-v4-pro"}|} "set_model";
  call t h "get_state";
  call t h "nope";
  call t h ~params:{|{"text": 5}|} "prompt";
  print_endline
    (Json.to_string
       (Rpc_server.handle h.server h.client (Json.of_string {|{"id": 7}|})));
  print_endline
    (Json.to_string
       (Rpc_server.handle h.server h.client (Json.of_string {|[1]|})));
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":"pong"}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"thinking must be one of: off, on, low, high, max"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"unknown model \"gpt-9\"; did you mean: openai/gpt-4 (GPT-4), openai/gpt-5 (GPT-5), openai/gpt-4o (GPT-4o)"}
    {"type":"response","id":"r1","ok":false,"error":"model \"GPT-5.5\" is ambiguous; one of: openai/gpt-5.5, openai-codex/gpt-5.5"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"model \"claude fabl\" is ambiguous; one of: anthropic/claude-fable-5, anthropic/claude-fable-5-1"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-v4-pro","provider":"deepseek","key":"deepseek/deepseek-v4-pro","name":"DeepSeek V4 Pro","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":1.32,"output":3.96,"cache_read":0.044}},"thinking":"high","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
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
  @@ fun t agent h ->
  let events = Queue.create () in
  Agent.subscribe agent ~f:(fun e -> Queue.enqueue events (Rpc_json.event e));
  call t h ~params:{|{"text": "what is here?"}|} "prompt";
  call t h ~params:{|{"text": "again"}|} "prompt";
  Agent.wait_idle agent;
  Queue.iter events ~f:(fun e -> print_endline (mask t (Json.to_string e)));
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"a run is already in progress; use steer or follow_up"}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":true,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
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
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":4,"usage":{"input":30,"output":13,"cache_read":5},"cost_usd":2.313e-05,"context_tokens":10,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
    |}];
  call t h "get_messages";
  call t h "get_entries";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":[{"role":"user","text":"what is here?"},{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"},{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false},{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"}]}
    {"type":"response","id":"r1","ok":true,"result":{"head":"<id>","entries":[{"id":"<id>","parent":null,"kind":"system_prompt"},{"id":"<id>","parent":"<id>","kind":"message","message":{"role":"user","text":"what is here?"}},{"id":"<id>","parent":"<id>","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"tool_call","id":"c1","name":"ls","arguments":"{}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":20,"output":8,"cache_read":5},"model":"deepseek-flash"}},{"id":"<id>","parent":"<id>","kind":"message","message":{"role":"tool_result","tool_call_id":"c1","tool_name":"ls","text":"sessions/\n","is_error":false}},{"id":"<id>","parent":"<id>","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"}}]}}
    |}]
;;

let%expect_test "prompt attachments are inlined into the user message" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t "a.txt" "hello\nworld\n";
  Core_unix.mkdir_p (Filename.concat t.dir "dir");
  let on_request (request : Provider.Request.t) =
    List.iter request.messages ~f:(function
      | Message.User u -> print_endline (mask t u.text)
      | _ -> ())
  in
  let agent, h =
    make_server
      t
      ~sw
      ~provider:(Faux_provider.create ~on_request [ Reply.text "ok" ])
  in
  call
    t
    h
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
  @@ fun t agent h ->
  call t h ~params:{|{"text": "first"}|} "prompt";
  Agent.wait_idle agent;
  let first = (Agent.state agent).session_path in
  call t h "new_session";
  call t h "list_sessions";
  call t h ~params:(sprintf {|{"path": "%s"}|} first) "switch_session";
  call t h ~params:{|{"path": "/nowhere.jsonl"}|} "switch_session";
  call t h "fork";
  call t h ~params:{|{"to": "bogus"}|} "rewind";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":null,"cwd":"$DIR","created_at":"<time>","updated_at":"<time>","first_prompt":"first","message_count":2,"parent":null,"live":true,"running":false,"clients":0},{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":null,"cwd":"$DIR","created_at":"<time>","updated_at":"<time>","first_prompt":null,"message_count":0,"parent":null,"live":true,"running":false,"clients":1}]}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"no session \"/nowhere.jsonl\""}
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
  let agent, h =
    make_server
      t
      ~sw
      ~provider:
        (Faux_provider.create
           [ Reply.tool_call
               ~id:"c1"
               ~name:"bash"
               ~arguments:{|{"command":"sleep 5"}|}
               ()
           ])
  in
  let in_r, in_w = Eio_unix.pipe sw in
  let out_r, out_w = Eio_unix.pipe sw in
  let output = Buffer.create 4096 in
  Eio.Fiber.all
    [ (fun () ->
        Rpc_server.serve_connection h.server ~input:in_r ~output:out_w;
        Rpc_server.shutdown h.server;
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
  @@ fun t agent h ->
  let events = Queue.create () in
  Agent.subscribe agent ~f:(fun e -> Queue.enqueue events (Rpc_json.event e));
  call t h ~params:{|{"text": "go"}|} "prompt";
  call t h ~params:{|{"text": "first steer"}|} "steer";
  call t h ~params:{|{"text": "second follow up"}|} "follow_up";
  call t h "abort";
  Agent.wait_idle agent;
  Queue.iter events ~f:(fun e -> print_endline (mask t (Json.to_string e)));
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"restored":["first steer","second follow up"]}}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":true,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
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
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":2,"usage":{"input":20,"output":8,"cache_read":5},"cost_usd":1.413e-05,"context_tokens":20,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
    |}]
;;

let%expect_test "auth: status, login via prompt, logout" =
  with_agent []
  @@ fun t _agent h ->
  Login_manager.subscribe h.login ~f:(fun e ->
    print_endline (Json.to_string (Rpc_json.login_event e)));
  call t h "auth_status";
  call t h ~params:{|{"provider": "groq"}|} "login";
  call t h ~params:{|{"provider": "deepseek", "method": "oauth"}|} "login";
  call t h ~params:{|{"provider": "deepseek"}|} "login";
  call t h ~params:{|{"id": "p1", "value": "sk-rpc"}|} "auth_respond";
  Login_manager.wait h.login;
  call t h "auth_status";
  call t h ~params:{|{"provider": "anthropic", "method": "api_key"}|} "login";
  call t h "auth_cancel";
  Login_manager.wait h.login;
  call t h ~params:{|{"provider": "deepseek"}|} "logout";
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
  @@ fun t agent h ->
  call t h ~params:{|{"model": "deepseek-v4-pro"}|} "set_model";
  call t h ~params:{|{"text": "what is here?"}|} "prompt";
  Agent.wait_idle agent;
  call t h "session_stats";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"message_count":4,"turns":2,"tool_calls":{"ls":1},"usage":{"input":30,"output":13,"cache_read":5},"cost_usd":8.47e-05,"context_percent":0.001,"model_changes":1,"compactions":0,"duration_seconds":<t>}}
    |}]
;;

let%expect_test "import, set_session_name, export, clone, set_cwd" =
  with_agent [ Reply.text "unused" ]
  @@ fun t _agent h ->
  let external_dir = Filename.concat t.dir "external" in
  let src = Session.create ~dir:external_dir ~cwd:t.dir () in
  let (_ : Session.Entry.t) =
    Session.append_message src (Message.user "imported question")
  in
  call t h ~params:(sprintf {|{"path": "%s"}|} (Session.path src)) "import";
  call t h "get_state";
  call t h ~params:{|{"name": "imported"}|} "set_session_name";
  call t h "get_state";
  call t h ~params:{|{"format": "markdown"}|} "export";
  call t h ~params:{|{"format": "jsonl"}|} "export";
  call t h ~params:{|{"format": "html"}|} "export";
  call t h ~params:{|{"path": "/nowhere.jsonl"}|} "import";
  call t h "clone";
  let sub = Filename.concat t.dir "sub" in
  Core_unix.mkdir_p sub;
  call t h ~params:(sprintf {|{"path": "%s"}|} sub) "set_cwd";
  call t h "get_state";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"path":"$DIR/sessions/<stamp>_<id>.jsonl"}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":"imported","cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
    {"type":"response","id":"r1","ok":true,"result":{"path":"$DIR/sessions/exports/<stamp>_<id>.md"}}
    {"type":"response","id":"r1","ok":true,"result":{"path":"$DIR/sessions/exports/<stamp>_<id>.jsonl"}}
    {"type":"response","id":"r1","ok":false,"error":"unknown export format \"html\" (expected markdown or jsonl)"}
    {"type":"response","id":"r1","ok":false,"error":"(Sys_error \"/nowhere.jsonl: No such file or directory\")"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":"imported","cwd":"$DIR/sub","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR/sub","session_id":null,"session_name":null}]}}
    |}]
;;

let%expect_test "config: get, set, invalid, and the config_changed event" =
  with_agent []
  @@ fun t agent h ->
  Agent.subscribe agent ~f:(fun e ->
    print_endline (Json.to_string (Rpc_json.event e)));
  call t h "get_config";
  call
    t
    h
    ~params:{|{"config": {"scoped_models": ["a", "b"], "confirm_tools": true}}|}
    "set_config";
  call t h "get_config";
  call t h ~params:{|{"config": {"scoped_models": "x"}}|} "set_config";
  call t h ~params:{|{}|} "set_config";
  call t h ~params:{|{"call_id": "nope", "allow": true}|} "tool_confirm_respond";
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
  @@ fun t agent h ->
  Agent.subscribe agent ~f:(fun e ->
    print_endline (mask t (Json.to_string (Rpc_json.event e))));
  call
    t
    h
    ~params:{|{"command": "printf 'a\\nb'", "add_to_context": true}|}
    "shell";
  call t h "get_messages";
  [%expect
    {|
    {"type":"event","event":"tool_start","call":{"id":"shell-0","name":"shell","arguments":"{\"command\":\"printf 'a\\\\nb'\"}"}}
    {"type":"event","event":"tool_output","call_id":"shell-0","chunk":"a\nb"}
    {"type":"event","event":"tool_end","call":{"id":"shell-0","name":"shell","arguments":"{\"command\":\"printf 'a\\\\nb'\"}"},"result":{"role":"tool_result","tool_call_id":"shell-0","tool_name":"shell","text":"a\nb","is_error":false}}
    {"type":"event","event":"message_start","message":{"role":"user","text":"$ printf 'a\\nb'\na\nb"}}
    {"type":"event","event":"message_end","message":{"role":"user","text":"$ printf 'a\\nb'\na\nb"}}
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":1,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}
    {"type":"response","id":"r1","ok":true,"result":{"text":"a\nb","is_error":false}}
    {"type":"response","id":"r1","ok":true,"result":[{"role":"user","text":"$ printf 'a\\nb'\na\nb"}]}
    |}]
;;

let%expect_test "shell without add_to_context leaves messages unchanged" =
  with_agent []
  @@ fun t _agent h ->
  call
    t
    h
    ~params:{|{"command": "printf 'a\\nb'", "add_to_context": false}|}
    "shell";
  call t h ~params:{|{"command": "exit 3", "add_to_context": false}|} "shell";
  call t h "get_messages";
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
  @@ fun t agent h ->
  call t h ~params:{|{"text": "go"}|} "prompt";
  call t h ~params:{|{"command": "echo hi", "add_to_context": false}|} "shell";
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
  @@ fun t agent h ->
  call t h ~params:{|{"text": "go"}|} "prompt";
  call t h ~params:{|{"text": "steer one"}|} "steer";
  call t h ~params:{|{"text": "follow up"}|} "follow_up";
  call t h "dequeue";
  call t h "dequeue";
  call t h "dequeue";
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

let is_exec json =
  match Json.member "event" json with
  | Some (`String "tool_exec") -> true
  | _ -> false
;;

let exec_name json =
  match Json.member "name" json with
  | Some (`String name) -> name
  | _ -> ""
;;

(* Yields until the server pushes a [tool_exec] for a real tool to [sent].
   The [$instructions] lookup that every run starts with is answered on the
   host's behalf (no instruction files), printing that it happened. *)
let wait_for_exec (h : H.t) client sent =
  let rec go n =
    match Queue.find sent ~f:is_exec with
    | Some json when String.equal (exec_name json) Host_ops.instructions_op ->
      Queue.filter_inplace sent ~f:(fun j -> not (phys_equal j json));
      let exec_id =
        match Json.member "exec_id" json with
        | Some (`String id) -> id
        | _ -> ""
      in
      printf "(host answered %s)\n" Host_ops.instructions_op;
      ignore
        (Rpc_server.handle
           h.server
           client
           (Json.of_string
              (sprintf
                 {|{"id": 0, "method": "tool_exec_result", "params": {"exec_id": "%s", "text": "[]"}}|}
                 exec_id))
         : Json.t);
      go n
    | Some json -> json
    | None ->
      if n = 0 then failwith "no tool_exec";
      Eio.Fiber.yield ();
      go (n - 1)
  in
  go 1000
;;

(* A second client that advertises tools becomes the session's active host
   (tools default to the frontend); its [tool_exec] events are answered
   through [tool_exec_output]/[tool_exec_result]. *)
let%expect_test "remote tool host: exec round trip, switch, disconnect" =
  with_agent
    [ Reply.tool_call ~id:"c1" ~name:"bash" ~arguments:{|{"command":"x"}|} ()
    ; Reply.text "done"
    ; Reply.tool_call ~id:"c2" ~name:"ls" ~arguments:"{}" ()
    ; Reply.text "done again"
    ]
  @@ fun t agent h ->
  let sent = Queue.create () in
  let laptop = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  let call_as client ?(params = "{}") meth =
    let request =
      Json.of_string
        (sprintf {|{"id": "r2", "method": "%s", "params": %s}|} meth params)
    in
    print_endline
      (mask t (Json.to_string (Rpc_server.handle h.server client request)))
  in
  call_as
    laptop
    ~params:{|{"name": "laptop", "tools": true, "cwd": "/home/me/proj"}|}
    "hello";
  let state = Agent.state agent in
  print_endline (mask t (sprintf "%s %s" state.active_host state.cwd));
  (* The exec goes only to the host client. *)
  Queue.clear sent;
  Queue.clear h.sent;
  call t h ~params:{|{"text": "run it"}|} "prompt";
  let exec = wait_for_exec h laptop sent in
  let exec_id =
    match Json.member "exec_id" exec with
    | Some (`String id) -> id
    | _ -> ""
  in
  print_endline (mask t (Json.to_string exec));
  printf "default client saw tool_exec: %b\n" (Queue.exists h.sent ~f:is_exec);
  call_as
    laptop
    ~params:(sprintf {|{"exec_id": "%s", "chunk": "partial\n"}|} exec_id)
    "tool_exec_output";
  call_as
    laptop
    ~params:
      (sprintf
         {|{"exec_id": "%s", "text": "partial\nall\n", "is_error": false}|}
         exec_id)
    "tool_exec_result";
  call_as laptop ~params:{|{"exec_id": "nope", "text": ""}|} "tool_exec_result";
  Agent.wait_idle agent;
  List.iter (Agent.messages agent) ~f:(function
    | Message.Tool_result r -> printf "tool_result: %S\n" r.text
    | _ -> ());
  [%expect
    {|
    {"type":"response","id":"r2","ok":true,"result":{"client_id":"client-2","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"cwd":"/home/me/proj","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"client-2","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null},{"id":"client-2","name":"laptop","cwd":"/home/me/proj","session_id":"<id>","session_name":null}]}}}
    client-2 /home/me/proj
    {"type":"response","id":"r1","ok":true,"result":{}}
    (host answered $instructions)
    {"type":"event","event":"tool_exec","host":"client-2","exec_id":"<id>/c1-1","call_id":"c1","name":"bash","arguments":{"command":"x"},"cwd":"/home/me/proj"}
    default client saw tool_exec: false
    {"type":"response","id":"r2","ok":true,"result":{}}
    {"type":"response","id":"r2","ok":true,"result":{}}
    {"type":"response","id":"r2","ok":false,"error":"no tool execution \"nope\""}
    tool_result: "partial\nall\n"
    |}];
  (* Switch back to the backend, then drop the laptop: an unknown host is
     refused, the backend runs the next call itself. *)
  call t h ~params:{|{"host": "nobody"}|} "set_active_host";
  call t h ~params:{|{"host": "backend"}|} "set_active_host";
  Rpc_server.disconnect h.server laptop;
  print_endline
    (mask t (Sexp.to_string [%sexp (Agent.hosts agent : Agent.Host.t list)]));
  call t h ~params:{|{"text": "again"}|} "prompt";
  Agent.wait_idle agent;
  List.iter
    (List.drop (Agent.messages agent) 4)
    ~f:(function
      | Message.Tool_result r -> printf "tool_result: %S\n" r.text
      | _ -> ());
  [%expect
    {|
    {"type":"response","id":"r1","ok":false,"error":"unknown tool host \"nobody\""}
    {"type":"response","id":"r1","ok":true,"result":{}}
    (((id backend)(name <host>)(cwd $DIR)(session_id())(session_name())))
    {"type":"response","id":"r1","ok":true,"result":{}}
    tool_result: "sessions/\n"
    |}]
;;

let%expect_test "remote tool host: disconnect mid-call fails the call" =
  with_agent
    [ Reply.tool_call ~id:"c1" ~name:"bash" ~arguments:{|{"command":"x"}|} ()
    ; Reply.text "done"
    ; Reply.tool_call ~id:"c2" ~name:"bash" ~arguments:{|{"command":"y"}|} ()
    ; Reply.text "done"
    ]
  @@ fun t agent h ->
  let sent = Queue.create () in
  let laptop = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  ignore
    (Rpc_server.handle
       h.server
       laptop
       (Json.of_string
          {|{"id": 1, "method": "hello", "params": {"name": "laptop", "tools": true}}|})
     : Json.t);
  call t h ~params:{|{"text": "run it"}|} "prompt";
  ignore (wait_for_exec h laptop sent : Json.t);
  Rpc_server.disconnect h.server laptop;
  Agent.wait_idle agent;
  print_s [%sexp ((Agent.state agent).active_host : string)];
  List.iter (Agent.messages agent) ~f:(function
    | Message.Tool_result r -> printf "tool_result: %S\n" r.text
    | _ -> ());
  (* With the host gone, the next call fails immediately. *)
  call t h ~params:{|{"text": "and again"}|} "prompt";
  Agent.wait_idle agent;
  List.iter
    (List.drop (Agent.messages agent) 4)
    ~f:(function
      | Message.Tool_result r -> printf "tool_result: %S\n" r.text
      | _ -> ());
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    (host answered $instructions)
    client-2
    tool_result: "[tool host disconnected]"
    {"type":"response","id":"r1","ok":true,"result":{}}
    tool_result: "tool host \"client-2\" is not connected; use set_active_host to pick another"
    |}]
;;

(* Hosts are global: a client attached to one session can run the tools of
   another, while still serving its own. *)
let%expect_test "remote tool host: attached to another session" =
  with_agent
    [ Reply.tool_call ~id:"c1" ~name:"bash" ~arguments:{|{"command":"x"}|} ()
    ; Reply.text "done"
    ; Reply.tool_call ~id:"c2" ~name:"bash" ~arguments:{|{"command":"y"}|} ()
    ; Reply.text "done"
    ]
  @@ fun t agent h ->
  let sent = Queue.create () in
  let laptop = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  let handle_as client ?(params = "{}") meth =
    Rpc_server.handle
      h.server
      client
      (Json.of_string
         (sprintf {|{"id": "r2", "method": "%s", "params": %s}|} meth params))
  in
  let call_as client ?params meth =
    print_endline (mask t (Json.to_string (handle_as client ?params meth)))
  in
  let show_hosts label a =
    printf
      "%s: active=%s hosts=%s\n"
      label
      (Agent.active_host a)
      (mask t (Sexp.to_string [%sexp (Agent.hosts a : Agent.Host.t list)]))
  in
  ignore
    (handle_as
       laptop
       ~params:{|{"name": "laptop", "tools": true, "cwd": "/home/me/proj"}|}
       "hello"
     : Json.t);
  call_as laptop "new_session";
  let other = Rpc_server.agent_of_client h.server laptop in
  call_as laptop ~params:{|{"name": "side"}|} "set_session_name";
  call_as h.client ~params:{|{"host": "client-2"}|} "set_active_host";
  show_hosts "main" agent;
  show_hosts "side" other;
  [%expect
    {|
    {"type":"response","id":"r2","ok":true,"result":{}}
    {"type":"response","id":"r2","ok":true,"result":{}}
    {"type":"response","id":"r2","ok":true,"result":{}}
    main: active=client-2 hosts=(((id backend)(name <host>)(cwd $DIR)(session_id())(session_name()))((id client-2)(name laptop)(cwd /home/me/proj)(session_id(<id>))(session_name(side))))
    side: active=client-2 hosts=(((id backend)(name <host>)(cwd /home/me/proj)(session_id())(session_name()))((id client-2)(name laptop)(cwd /home/me/proj)(session_id(<id>))(session_name(side))))
    |}];
  (* The main session's exec goes to the laptop, whose reply is routed back
     by exec id rather than to its own session. *)
  let answer exec =
    let exec_id =
      match Json.member "exec_id" exec with
      | Some (`String id) -> id
      | _ -> ""
    in
    print_endline (mask t (Json.to_string exec));
    call_as
      laptop
      ~params:
        (sprintf
           {|{"exec_id": "%s", "text": "ran %s"}|}
           exec_id
           (exec_name exec))
      "tool_exec_result"
  in
  call t h ~params:{|{"text": "run it"}|} "prompt";
  answer (wait_for_exec h laptop sent);
  Agent.wait_idle agent;
  Queue.clear sent;
  call_as laptop ~params:{|{"text": "run mine"}|} "prompt";
  answer (wait_for_exec h laptop sent);
  Agent.wait_idle other;
  let results a =
    List.filter_map (Agent.messages a) ~f:(function
      | Message.Tool_result r -> Some r.text
      | _ -> None)
  in
  print_s
    [%sexp
      { main = (results agent : string list)
      ; side = (results other : string list)
      }];
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    (host answered $instructions)
    {"type":"event","event":"tool_exec","host":"client-2","exec_id":"<id>/c1-1","call_id":"c1","name":"bash","arguments":{"command":"x"},"cwd":"/home/me/proj"}
    {"type":"response","id":"r2","ok":true,"result":{}}
    {"type":"response","id":"r2","ok":true,"result":{}}
    (host answered $instructions)
    {"type":"event","event":"tool_exec","host":"client-2","exec_id":"<id>/c2-1","call_id":"c2","name":"bash","arguments":{"command":"y"},"cwd":"/home/me/proj"}
    {"type":"response","id":"r2","ok":true,"result":{}}
    ((main ("ran bash")) (side ("ran bash")))
    |}];
  (* Dropping tools in a later hello removes the host from every session. *)
  ignore (handle_as laptop ~params:{|{"name": "laptop"}|} "hello" : Json.t);
  show_hosts "main" agent;
  show_hosts "side" other;
  [%expect
    {|
    main: active=client-2 hosts=(((id backend)(name <host>)(cwd $DIR)(session_id())(session_name())))
    side: active=client-2 hosts=(((id backend)(name <host>)(cwd /home/me/proj)(session_id())(session_name())))
    |}]
;;

(* [@] completion lists paths on the active tool host, under the session cwd:
   the backend's own filesystem by default, a connected frontend's after
   [set_active_host]. *)
let%expect_test "list_paths: on the backend, then routed to a remote host" =
  with_agent []
  @@ fun t _agent h ->
  List.iter [ "src/sub"; "docs"; ".git/objects"; "_build/default" ] ~f:(fun d ->
    Core_unix.mkdir_p (Filename.concat t.dir d));
  List.iter
    [ "src/app.ml"
    ; "src/sub/deep.ml"
    ; "docs/README.md"
    ; ".git/HEAD"
    ; "_build/x"
    ]
    ~f:(fun f -> Out_channel.write_all (Filename.concat t.dir f) ~data:"");
  call t h "list_paths";
  call t h ~params:{|{"prefix": "APP"}|} "list_paths";
  call t h ~params:{|{"prefix": "zzz"}|} "list_paths";
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":["docs/","docs/README.md","sessions/","sessions/<stamp>_<id>.jsonl","src/","src/app.ml","src/sub/","src/sub/deep.ml"]}
    {"type":"response","id":"r1","ok":true,"result":["src/app.ml"]}
    {"type":"response","id":"r1","ok":true,"result":[]}
    |}];
  let sent = Queue.create () in
  let laptop = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  let call_as client ?(params = "{}") meth =
    Rpc_server.handle
      h.server
      client
      (Json.of_string
         (sprintf {|{"id": "r2", "method": "%s", "params": %s}|} meth params))
  in
  ignore
    (call_as
       laptop
       ~params:{|{"name": "laptop", "tools": true, "cwd": "/home/me/proj"}|}
       "hello"
     : Json.t);
  (* The request blocks until the host answers, so answer from another fiber. *)
  Eio.Fiber.both
    (fun () -> call t h ~params:{|{"prefix": "ma"}|} "list_paths")
    (fun () ->
       let rec wait () =
         match Queue.find sent ~f:is_exec with
         | Some json -> json
         | None ->
           Eio.Fiber.yield ();
           wait ()
       in
       let exec = wait () in
       print_endline (mask t (Json.to_string exec));
       let exec_id =
         match Json.member "exec_id" exec with
         | Some (`String id) -> id
         | _ -> ""
       in
       ignore
         (call_as
            laptop
            ~params:
              (sprintf
                 {|{"exec_id": "%s", "text": "[\"main.ml\",\"src/main.ml\"]", "is_error": false}|}
                 exec_id)
            "tool_exec_result"
          : Json.t));
  [%expect
    {|
    {"type":"event","event":"tool_exec","host":"client-2","exec_id":"<id>/list_paths-0","call_id":"list_paths","name":"$list_paths","arguments":{"prefix":"ma"},"cwd":"/home/me/proj"}
    {"type":"response","id":"r1","ok":true,"result":["main.ml","src/main.ml"]}
    |}]
;;
