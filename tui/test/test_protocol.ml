open! Core
open! Expect_test_helpers_core
open Prigh_protocol

let decode line =
  match Server_message.of_line line with
  | Ok m -> print_s (Server_message.sexp_of_t m)
  | Error e -> print_s [%message "error" (e : Error.t)]
;;

let%expect_test "responses" =
  decode {|{"type":"response","id":3,"ok":true,"result":"pong"}|};
  decode {|{"type":"response","id":4,"ok":false,"error":"unknown model \"x\""}|};
  decode {|{"type":"response","id":null,"ok":false,"error":"invalid JSON"}|};
  [%expect
    {|
    (Response ((id (3)) (result (Ok pong))))
    (Response ((id (4)) (result (Error "unknown model \"x\""))))
    (Response ((id ()) (result (Error "invalid JSON"))))
    |}]
;;

let%expect_test "state event and messages" =
  decode
    {|{"type":"event","event":"state","state":{"session_id":"s1","session_path":"/tmp/s1.jsonl","session_name":"my session","cwd":"/work","git_branch":"main","model":{"id":"claude-fable-5-1","provider":"anthropic","key":"anthropic/claude-fable-5-1","name":"Claude Fable 5.1","context_window":1000000,"max_output":128000,"supports_thinking":true,"cost":{"input":10,"output":50,"cache_read":0.25}},"thinking":"low","running":false,"message_count":3,"usage":{"input":120,"output":40,"cache_read":10},"cost_usd":0.0032,"context_tokens":160}}|};
  decode
    {|{"type":"event","event":"message_start","message":{"role":"user","text":"hi"}}|};
  decode
    {|{"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Hello"},{"type":"thinking","text":"hmm"},{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}],"stop_reason":{"type":"error","message":"boom"},"usage":{"input":1,"output":2,"cache_read":0},"model":"anthropic/claude-fable-5-1"}}|};
  decode
    {|{"type":"event","event":"message_update","partial":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"tool_call_start","index":0,"id":"c1","name":"read"}}|};
  decode
    {|{"type":"event","event":"tool_end","call":{"id":"c1","name":"bash","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"out","is_error":false}}|};
  decode
    {|{"type":"event","event":"tool_output","call_id":"c1","chunk":"line\n"}|};
  decode
    {|{"type":"event","event":"agent_end","messages":[{"role":"user","text":"x"}]}|};
  decode {|{"type":"event","event":"compacted","summary":"s"}|};
  decode {|{"type":"event","event":"notice","text":"n"}|};
  decode {|{"type":"event","event":"queue_update","steer":2,"follow_up":1}|};
  [%expect
    {|
    (Event (
      State (
        (session_id   s1)
        (session_path /tmp/s1.jsonl)
        (session_name ("my session"))
        (session_description ())
        (cwd /work)
        (git_branch (main))
        (model (
          (id                claude-fable-5-1)
          (provider          anthropic)
          (key               anthropic/claude-fable-5-1)
          (name              "Claude Fable 5.1")
          (context_window    1000000)
          (max_output        128000)
          (supports_thinking true)
          (cost (
            (input      10)
            (output     50)
            (cache_read 0.25)))))
        (thinking      low)
        (running       false)
        (message_count 3)
        (usage (
          (input      120)
          (output     40)
          (cache_read 10)))
        (cost_usd       0.0032)
        (context_tokens 160)
        (active_host    backend)
        (hosts     ())
        (subagents ())
        (jobs      ()))))
    (Event (Message_start (User ((text hi)))))
    (Event (
      Message_end (
        Assistant (
          (content (
            (Text     Hello)
            (Thinking hmm)
            (Tool_call (
              (id        c1)
              (name      bash)
              (arguments "{\"command\":\"ls\"}")))))
          (stop_reason (Error boom))
          (usage (
            (input      1)
            (output     2)
            (cache_read 0)))
          (model anthropic/claude-fable-5-1)))))
    (Event (
      Message_update
      (partial (
        (content ())
        (stop_reason End_turn)
        (usage (
          (input      0)
          (output     0)
          (cache_read 0)))
        (model m)))
      (delta (
        Tool_call_start
        (index 0)
        (id    c1)
        (name  read)))))
    (Event (
      Tool_end
      (call (
        (id        c1)
        (name      bash)
        (arguments {})))
      (result (
        (tool_call_id c1)
        (tool_name    bash)
        (text         out)
        (is_error     false)))))
    (Event (
      Tool_output
      (call_id c1)
      (chunk   "line\n")))
    (Event (Agent_end ((User ((text x))))))
    (Event (Compacted s))
    (Event (Notice n))
    (Event (
      Queue_update
      (steer     2)
      (follow_up 1)))
    |}]
;;

let%expect_test "images in user messages and tool results (absent means none)" =
  decode
    {|{"type":"event","event":"message_start","message":{"role":"user","text":"what is this?","images":[{"mime_type":"image/png","data":"iVBORw0KGgo="},{"mime_type":"image/webp","data":"UklGRg"}]}}|};
  decode
    {|{"type":"event","event":"message_start","message":{"role":"user","text":"none","images":[]}}|};
  decode
    {|{"type":"event","event":"tool_end","call":{"id":"c1","name":"read","arguments":"{\"path\":\"shot.png\"}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"read","text":"Read image file [image/png, 800x600]","is_error":false,"images":[{"mime_type":"image/png","data":"iVBORw0KGgo="}]}}|};
  decode
    {|{"type":"event","event":"message_end","message":{"role":"tool_result","tool_call_id":"c1","tool_name":"read","text":"t","is_error":false,"images":[{"mime_type":"image/png"}]}}|};
  [%expect
    {|
    (Event (
      Message_start (
        User (
          (text "what is this?")
          (images (
            ((mime_type image/png)  (bytes 8))
            ((mime_type image/webp) (bytes 4))))))))
    (Event (Message_start (User ((text none)))))
    (Event (
      Tool_end
      (call (
        (id        c1)
        (name      read)
        (arguments "{\"path\":\"shot.png\"}")))
      (result (
        (tool_call_id c1)
        (tool_name    read)
        (text         "Read image file [image/png, 800x600]")
        (is_error     false)
        (images ((
          (mime_type image/png)
          (bytes     8))))))))
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"message_end\",\"message\":{\"role\":\"tool_result\",\"tool_call_id\":\"c1\",\"tool_name\":\"read\",\"text\":\"t\",\"is_error\":false,\"images\":[{\"mime_type\":\"image/png\"}]}}"
        (in images[0] "missing field \"data\""))))
    |}]
;;

let%expect_test "message and entry times (\"at\", ms since the epoch; absent \
                 means unknown)"
  =
  decode
    {|{"type":"event","event":"message_start","message":{"role":"user","text":"hi","at":1791210730250}}|};
  decode
    {|{"type":"event","event":"message_end","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m","at":1791210731000}}|};
  decode
    {|{"type":"event","event":"message_end","message":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"out","is_error":false,"at":1791210732000}}|};
  decode
    {|{"type":"event","event":"message_end","message":{"role":"user","text":"bad","at":"soon"}}|};
  let entry json =
    match Entry.of_json (Jsonaf.of_string json) with
    | Ok e -> print_s (Entry.sexp_of_t e)
    | Error e -> print_s [%message "error" (e : Error.t)]
  in
  entry {|{"id":"e1","parent":null,"at":1791210730250,"kind":"cwd","cwd":"/w"}|};
  entry {|{"id":"e2","parent":"e1","kind":"cwd","cwd":"/w"}|};
  [%expect
    {|
    (Event (
      Message_start (
        User (
          (text hi)
          (at   "2026-10-05 14:32:10.25Z")))))
    (Event (
      Message_end (
        Assistant (
          (content ())
          (stop_reason End_turn)
          (usage (
            (input      1)
            (output     2)
            (cache_read 0)))
          (model m)
          (at    "2026-10-05 14:32:11Z")))))
    (Event (
      Message_end (
        Tool_result (
          (tool_call_id c1)
          (tool_name    bash)
          (text         out)
          (is_error     false)
          (at           "2026-10-05 14:32:12Z")))))
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"message_end\",\"message\":{\"role\":\"user\",\"text\":\"bad\",\"at\":\"soon\"}}"
        "field \"at\": expected number, got \"soon\"")))
    ((id e1) (parent ()) (at "2026-10-05 14:32:10.25Z") (kind (Cwd (cwd /w))))
    ((id e2) (parent (e1)) (kind (Cwd (cwd /w))))
    |}]
;;

let%expect_test "image sizes come from the base64 length, padded or not" =
  List.iter
    [ ""; "QQ=="; "QQ"; "QUI="; "QUI"; "QUJD"; "iVBORw0KGgo=" ]
    ~f:(fun data ->
      print_s [%message data ~bytes:(Image.decoded_size data : int)]);
  List.iter [ 0; 1023; 1024; 35021; 1_572_864 ] ~f:(fun bytes ->
    print_endline
      (Image.to_string_hum { mime_type = "image/jpeg"; bytes; data = "" }));
  [%expect
    {|
    ("" (bytes 0))
    (QQ== (bytes 1))
    (QQ (bytes 1))
    (QUI= (bytes 2))
    (QUI (bytes 2))
    (QUJD (bytes 3))
    (iVBORw0KGgo= (bytes 8))
    [image: image/jpeg, 0 B]
    [image: image/jpeg, 1023 B]
    [image: image/jpeg, 1.0 KB]
    [image: image/jpeg, 34.2 KB]
    [image: image/jpeg, 1.5 MB]
    |}]
;;

let%expect_test "tool_confirm event" =
  decode
    {|{"type":"event","event":"tool_confirm","call_id":"c1","name":"bash","summary":"rm -rf build"}|};
  decode {|{"type":"event","event":"tool_confirm","name":"write"}|};
  [%expect
    {|
    (Event (
      Tool_confirm
      (call_id c1)
      (name    bash)
      (summary "rm -rf build")))
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"tool_confirm\",\"name\":\"write\"}"
        "missing field \"call_id\"")))
    |}]
;;

let%expect_test "terminal relay events" =
  decode
    {|{"type":"event","event":"terminal_open","host":"client-2","term_id":"t1","key":"sess-1","cwd":"/work","cols":120,"rows":40}|};
  decode
    {|{"type":"event","event":"terminal_frame","host":"client-2","term_id":"t1","kind":"binary","data":"bHMK"}|};
  decode
    {|{"type":"event","event":"terminal_frame","host":"client-2","term_id":"t1","kind":"text","data":"{\"type\":\"ping\"}"}|};
  decode
    {|{"type":"event","event":"terminal_frame","host":"client-2","term_id":"t1","kind":"video","data":""}|};
  decode
    {|{"type":"event","event":"terminal_close","host":"client-2","term_id":"t1"}|};
  [%expect
    {|
    (Event (
      Terminal_open
      (term_id t1)
      (key     sess-1)
      (cwd     /work)
      (cols    120)
      (rows    40)))
    (Event (
      Terminal_frame
      (term_id t1)
      (kind    Binary)
      (data    bHMK)))
    (Event (
      Terminal_frame
      (term_id t1)
      (kind    Text)
      (data    "{\"type\":\"ping\"}")))
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"terminal_frame\",\"host\":\"client-2\",\"term_id\":\"t1\",\"kind\":\"video\",\"data\":\"\"}"
        "unknown frame kind \"video\"")))
    (Event (Terminal_close t1))
    |}]
;;

let%expect_test "tool host events and state hosts" =
  decode
    {|{"type":"event","event":"tool_exec","host":"client-2","exec_id":"c1-0","call_id":"c1","name":"bash","arguments":{"command":"ls"},"cwd":"/work"}|};
  decode
    {|{"type":"event","event":"tool_exec_cancel","host":"client-2","exec_id":"c1-0"}|};
  decode
    (Fixtures.state_json ()
     |> fun s -> sprintf {|{"type":"event","event":"state","state":%s}|} s);
  [%expect
    {|
    (Event (
      Tool_exec
      (exec_id c1-0)
      (call_id c1)
      (name    bash)
      (arguments ((command ls)))
      (cwd /work)))
    (Event (Tool_exec_cancel c1-0))
    (Event (
      State (
        (session_id   abc123)
        (session_path /home/u/.prigh/sessions/1.jsonl)
        (session_name        ())
        (session_description ())
        (cwd /work)
        (git_branch ())
        (model (
          (id                deepseek-flash)
          (provider          deepseek)
          (key               deepseek/deepseek-flash)
          (name              "DeepSeek V4.1 Flash")
          (context_window    1000000)
          (max_output        128000)
          (supports_thinking true)
          (cost (
            (input      10)
            (output     50)
            (cache_read 1)))))
        (thinking      off)
        (running       false)
        (message_count 2)
        (usage (
          (input      1200)
          (output     300)
          (cache_read 0)))
        (cost_usd       0.0123)
        (context_tokens 1500)
        (active_host    backend)
        (hosts ((
          (id   backend)
          (name srv)
          (cwd  /work)
          (session_id   ())
          (session_name ()))))
        (subagents ())
        (jobs      ()))))
    |}]
;;

let%expect_test "state lists background subagents (absent means none)" =
  let subagents json =
    match Json.parse json |> Or_error.bind ~f:State.of_json with
    | Ok state -> print_s [%sexp (state.subagents : State.Subagent.t list)]
    | Error e -> print_s [%message "error" (e : Error.t)]
  in
  subagents
    (Fixtures.state_json
       ~subagents:[ "a1", "look around", true; "a2", "run tests", false ]
       ());
  subagents
    {|{"session_id":"s1","session_path":"/tmp/s1.jsonl","cwd":"/work","model":{"id":"m","provider":"p","key":"p/m","name":"M","context_window":1,"max_output":1,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}|};
  [%expect
    {|
    (((id a1) (task "look around") (running true))
     ((id a2) (task "run tests")   (running false)))
    ()
    |}]
;;

let%expect_test "state lists background jobs; list_jobs entries" =
  (match
     Json.parse
       (Fixtures.state_json
          ~jobs:[ "j1", "make test", None; "j2", "npm run dev", Some "killed" ]
          ())
     |> Or_error.bind ~f:State.of_json
   with
   | Ok state -> print_s [%sexp (state.jobs : State.Job.t list)]
   | Error e -> print_s [%message "error" (e : Error.t)]);
  print_s
    [%sexp
      (Json.parse
         {|{"id":"j1","command":"make","running":false,"exit":"exited 2","delivered":true,"elapsed":1.5,"bytes":12,"last_line":"Error 2"}|}
       |> Or_error.bind ~f:Job_info.of_json
       : Job_info.t Or_error.t)];
  [%expect
    {|
    (((id      j1)
      (command "make test")
      (running true)
      (exit ()))
     ((id      j2)
      (command "npm run dev")
      (running false)
      (exit (killed))))
    (Ok (
      (id      j1)
      (command make)
      (running false)
      (exit ("exited 2"))
      (delivered true)
      (elapsed   1.5)
      (bytes     12)
      (last_line ("Error 2"))))
    |}]
;;

let%expect_test "subagent events are decoded recursively" =
  decode
    {|{"type":"event","event":"subagent","call_id":"p","agent_id":"p","inner":{"type":"event","event":"subagent","call_id":"c1","agent_id":"p/c1","inner":{"type":"event","event":"tool_start","call":{"id":"c1","name":"read","arguments":"{}"}}}}|};
  decode
    {|{"type":"event","event":"subagent_start","call_id":"p","agent_id":"p","task":"go","model":"m","tools":["read","ls"]}|};
  decode
    {|{"type":"event","event":"subagent_end","call_id":"p","agent_id":"p","usage":{"input":3,"output":4,"cache_read":1},"turns":2,"cost_usd":0.001,"result":{"text":"done","is_error":false}}|};
  [%expect
    {|
    (Event (
      Subagent
      (call_id  p)
      (agent_id p)
      (event (
        Subagent
        (call_id  c1)
        (agent_id p/c1)
        (event (
          Tool_start (
            (id        c1)
            (name      read)
            (arguments {}))))))))
    (Event (
      Subagent_start
      (call_id  p)
      (agent_id p)
      (task     go)
      (model    m)
      (tools (read ls))))
    (Event (
      Subagent_end
      (call_id  p)
      (agent_id p)
      (usage (
        (input      3)
        (output     4)
        (cache_read 1)))
      (turns    2)
      (cost_usd 0.001)
      (result (
        (text     done)
        (is_error false)))))
    |}]
;;

let%expect_test "auth events" =
  decode
    {|{"type":"event","event":"auth","kind":"auth_url","url":"https://x","instructions":"paste the code"}|};
  decode
    {|{"type":"event","event":"auth","kind":"prompt","id":"p1","prompt":"secret","message":"API key"}|};
  decode
    {|{"type":"event","event":"auth","kind":"prompt","id":"p2","prompt":"select","message":"Pick","options":[{"id":"a","label":"A"},{"id":"b","label":"B"}]}|};
  decode
    {|{"type":"event","event":"auth","kind":"prompt","id":"p3","prompt":"manual_code","message":"Paste","placeholder":"http://localhost:1455/callback"}|};
  decode
    {|{"type":"event","event":"auth","kind":"prompt","id":"p4","prompt":"secret","message":"Key","allow_empty":true}|};
  decode
    {|{"type":"event","event":"auth","kind":"prompt","id":"p5","prompt":"text","message":"Base URL","placeholder":"http://localhost:3000/v1","default":"http://gpu/v1"}|};
  decode {|{"type":"event","event":"auth","kind":"prompt_cancelled","id":"p1"}|};
  decode
    {|{"type":"event","event":"auth","kind":"progress","message":"waiting"}|};
  decode
    {|{"type":"event","event":"auth","kind":"done","provider":"anthropic","method":"oauth"}|};
  decode
    {|{"type":"event","event":"auth","kind":"failed","provider":"openai","error":"nope"}|};
  decode
    {|{"type":"event","event":"auth","kind":"logged_out","provider":"deepseek"}|};
  [%expect
    {|
    (Event (
      Auth (
        Auth_url
        (url          https://x)
        (instructions "paste the code"))))
    (Event (
      Auth (
        Prompt
        (id p1)
        (prompt (
          Secret
          (message     "API key")
          (allow_empty false))))))
    (Event (
      Auth (
        Prompt
        (id p2)
        (prompt (
          Select
          (message Pick)
          (options (
            (a A)
            (b B))))))))
    (Event (
      Auth (
        Prompt
        (id p3)
        (prompt (
          Manual_code
          (message     Paste)
          (placeholder http://localhost:1455/callback))))))
    (Event (
      Auth (
        Prompt
        (id p4)
        (prompt (
          Secret
          (message     Key)
          (allow_empty true))))))
    (Event (
      Auth (
        Prompt
        (id p5)
        (prompt (
          Text
          (message     "Base URL")
          (placeholder http://localhost:3000/v1)
          (default     http://gpu/v1))))))
    (Event (Auth (Prompt_cancelled (id p1))))
    (Event (Auth (Progress waiting)))
    (Event (
      Auth (
        Done
        (provider anthropic)
        (method_  oauth))))
    (Event (
      Auth (
        Failed
        (provider openai)
        (error    nope))))
    (Event (Auth (Logged_out deepseek)))
    |}]
;;

let%expect_test "auth status, models, sessions" =
  let show f json =
    match Json.parse json with
    | Error e -> print_s [%message "parse" (e : Error.t)]
    | Ok j ->
      (match f j with
       | Ok s -> print_s s
       | Error e -> print_s [%message "decode" (e : Error.t)])
  in
  show
    (fun j -> Or_error.map (Auth_status.of_json j) ~f:Auth_status.sexp_of_t)
    {|{"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Claude Pro/Max"},{"method":"api_key","label":"API key"}],"configured":{"method":"oauth","source":"auth.json"},"expires_ms":1700000000000}|};
  show
    (fun j -> Or_error.map (Auth_status.of_json j) ~f:Auth_status.sexp_of_t)
    {|{"provider":"openai","name":"OpenAI","methods":[],"configured":null,"expires_ms":null}|};
  show
    (fun j -> Or_error.map (Auth_status.of_json j) ~f:Auth_status.sexp_of_t)
    {|{"provider":"aiproxy","name":"aiproxy","methods":[{"method":"api_key","label":"aiproxy API key"}],"configured":{"method":"api_key","source":"no key"},"expires_ms":null,"custom":{"base_url":"http://localhost:3000/v1","api":"chat","api_label":"OpenAI chat completions (/chat/completions)"}}|};
  show
    (fun j ->
       Or_error.map (Session_summary.of_json j) ~f:Session_summary.sexp_of_t)
    {|{"id":"abc","path":"/p","name":"build fix","cwd":"/c","created_at":"2025-01-01T00:00:00Z","updated_at":"2025-06-01T12:34:56Z","first_prompt":null,"message_count":0,"parent":null}|};
  [%expect
    {|
    ((provider anthropic)
     (name     Anthropic)
     (methods (
       ((method_ oauth)   (label "Claude Pro/Max"))
       ((method_ api_key) (label "API key"))))
     (configured ((
       (method_ oauth)
       (source  auth.json))))
     (expires_ms (1700000000000))
     (custom ()))
    ((provider openai)
     (name     OpenAI)
     (methods    ())
     (configured ())
     (expires_ms ())
     (custom     ()))
    ((provider aiproxy)
     (name     aiproxy)
     (methods ((
       (method_ api_key)
       (label   "aiproxy API key"))))
     (configured ((
       (method_ api_key)
       (source  "no key"))))
     (expires_ms ())
     (custom ((
       (base_url http://localhost:3000/v1)
       (api      chat)
       (api_label "OpenAI chat completions (/chat/completions)")))))
    ((id   abc)
     (path /p)
     (name ("build fix"))
     (description ())
     (cwd        /c)
     (created_at 2025-01-01T00:00:00Z)
     (updated_at (2025-06-01T12:34:56Z))
     (first_prompt ())
     (message_count 0)
     (parent ())
     (live    false)
     (running false)
     (clients 0))
    |}]
;;

let%expect_test "entries and session stats" =
  let show f json =
    match Json.parse json with
    | Error e -> print_s [%message "parse" (e : Error.t)]
    | Ok j ->
      (match f j with
       | Ok s -> print_s s
       | Error e -> print_s [%message "decode" (e : Error.t)])
  in
  (* The optional list-session fields may be absent. *)
  show
    (fun j ->
       Or_error.map (Session_summary.of_json j) ~f:Session_summary.sexp_of_t)
    {|{"id":"a","path":"/p","cwd":"/c","created_at":"2025-01-01T00:00:00Z","first_prompt":"hi","message_count":3}|};
  show
    (fun j -> Or_error.map (Entry.of_json j) ~f:Entry.sexp_of_t)
    {|{"id":"e1","parent":null,"kind":"message","message":{"role":"user","text":"first\nsecond"}}|};
  show
    (fun j -> Or_error.map (Entry.of_json j) ~f:Entry.sexp_of_t)
    {|{"id":"e2","parent":"e1","kind":"model","model":"deepseek/deepseek-flash","thinking":"high"}|};
  show
    (fun j -> Or_error.map (Entry.of_json j) ~f:Entry.sexp_of_t)
    {|{"id":"e3","parent":"e2","kind":"compaction","summary":"summarised","kept_from":"e1"}|};
  show
    (fun j -> Or_error.map (Entry.of_json j) ~f:Entry.sexp_of_t)
    {|{"id":"e4","parent":"e3","kind":"name","name":"my session"}|};
  show
    (fun j -> Or_error.map (Entry.of_json j) ~f:Entry.sexp_of_t)
    {|{"id":"e5","parent":"e4","kind":"cwd","cwd":"/work"}|};
  show
    (fun j -> Or_error.map (Session_stats.of_json j) ~f:Session_stats.sexp_of_t)
    {|{"message_count":4,"turns":2,"tool_calls":{"bash":1,"read":2},"usage":{"input":30,"output":13,"cache_read":5},"cost_usd":0.0001,"context_percent":1.5,"model_changes":1,"compactions":0,"duration_seconds":12.5}|};
  [%expect
    {|
    ((id   a)
     (path /p)
     (name        ())
     (description ())
     (cwd        /c)
     (created_at 2025-01-01T00:00:00Z)
     (updated_at ())
     (first_prompt (hi))
     (message_count 3)
     (parent ())
     (live    false)
     (running false)
     (clients 0))
    ((id e1) (parent ()) (kind (Message (User ((text "first\nsecond"))))))
    ((id e2)
     (parent (e1))
     (kind (
       Model
       (model    deepseek/deepseek-flash)
       (thinking high))))
    ((id e3)
     (parent (e2))
     (kind (
       Compaction
       (summary   summarised)
       (kept_from e1))))
    ((id e4) (parent (e3)) (kind (Name (name "my session"))))
    ((id e5) (parent (e4)) (kind (Cwd (cwd /work))))
    ((message_count 4)
     (turns         2)
     (tool_calls (
       (bash 1)
       (read 2)))
     (usage (
       (input      30)
       (output     13)
       (cache_read 5)))
     (cost_usd         0.0001)
     (context_percent  1.5)
     (model_changes    1)
     (compactions      0)
     (duration_seconds 12.5))
    |}]
;;

let%expect_test "errors name the field" =
  decode {|{"type":"event","event":"state","state":{"session_id":"s1"}}|};
  decode {|{"type":"event","event":"bogus"}|};
  decode {|not json|};
  decode {|{"type":"response","id":1,"ok":true,"result":{"nested":[1,2]}}|};
  [%expect
    {|
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"state\",\"state\":{\"session_id\":\"s1\"}}"
        "missing field \"session_path\"")))
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"bogus\"}"
        "unknown event \"bogus\"")))
    (error (e "invalid JSON at byte 0: unexpected token"))
    (Response ((id (1)) (result (Ok ((nested (1 2)))))))
    |}]
;;

let%expect_test "request encoding" =
  print_endline
    (Request.to_line
       { id = 7; method_ = "set_model"; params = [ "model", Json.str "a/b" ] });
  print_endline (Request.to_line { id = 8; method_ = "ping"; params = [] });
  [%expect
    {|
    {"id":7,"method":"set_model","params":{"model":"a/b"}}
    {"id":8,"method":"ping","params":{}}
    |}]
;;

let%expect_test "config round trip and config_changed event" =
  let show f json =
    match Json.parse json with
    | Error e -> print_s [%message "parse" (e : Error.t)]
    | Ok j ->
      (match f j with
       | Ok s -> print_s s
       | Error e -> print_s [%message "decode" (e : Error.t)])
  in
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"scoped_models":["anthropic/claude-fable-5-1","deepseek/deepseek-flash"],"confirm_tools":true}|};
  show (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t) {|{}|};
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"default_model":null,"default_thinking":"low"}|};
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"default_model":3}|};
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"scoped_models":[],"confirm_tools":false,"default_model":null,"default_thinking":null,"fallback_models":["openai-codex/gpt-6-sol","anthropic/claude-opus-5-5"],"default_cwd":"/srv/work"}|};
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"fallback_models":"gpt-6-sol"}|};
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"default_cwd":["/srv"]}|};
  print_s
    [%sexp
      (Config.to_json
         { Config.scoped_models = [ "a"; "b" ]
         ; confirm_tools = true
         ; default_model = Some "deepseek/deepseek-flash"
         ; default_thinking = Some "max"
         ; fallback_models =
             [ "openai-codex/gpt-6-sol"; "deepseek/deepseek-flash" ]
         ; default_cwd = Some "~/proj"
         }
       : Json.t)];
  decode
    {|{"type":"event","event":"config_changed","config":{"scoped_models":["a"],"confirm_tools":false}}|};
  [%expect
    {|
    ((scoped_models (anthropic/claude-fable-5-1 deepseek/deepseek-flash))
     (confirm_tools true)
     (default_model    ())
     (default_thinking ())
     (fallback_models  ())
     (default_cwd      ()))
    ((scoped_models ())
     (confirm_tools false)
     (default_model    ())
     (default_thinking ())
     (fallback_models  ())
     (default_cwd      ()))
    ((scoped_models ())
     (confirm_tools false)
     (default_model ())
     (default_thinking (low))
     (fallback_models ())
     (default_cwd     ()))
    (decode (e "default_model must be a string"))
    ((scoped_models ())
     (confirm_tools false)
     (default_model    ())
     (default_thinking ())
     (fallback_models (openai-codex/gpt-6-sol anthropic/claude-opus-5-5))
     (default_cwd (/srv/work)))
    (decode (e "fallback_models must be an array of strings"))
    (decode (e "default_cwd must be a string"))
    ((scoped_models (a b))
     (confirm_tools    true)
     (default_model    deepseek/deepseek-flash)
     (default_thinking max)
     (fallback_models (openai-codex/gpt-6-sol deepseek/deepseek-flash))
     (default_cwd ~/proj))
    (Event (
      Config_changed (
        (scoped_models (a))
        (confirm_tools false)
        (default_model    ())
        (default_thinking ())
        (fallback_models  ())
        (default_cwd      ()))))
    |}]
;;

let%expect_test "btw_delta" =
  decode
    {|{"type":"event","event":"btw_delta","btw_id":"btw-1","delta":"It is "}|};
  decode {|{"type":"event","event":"btw_delta","btw_id":"btw-1"}|};
  [%expect
    {|
    (Event (
      Btw_delta
      (btw_id btw-1)
      (delta  "It is ")))
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"btw_delta\",\"btw_id\":\"btw-1\"}"
        "missing field \"delta\"")))
    |}]
;;

(* With a [host_id] (the backend lists our tool host under it), that is our
   id. *)
let%expect_test "hello reply: client id and namespace" =
  List.iter
    [ {|{"client_id":"client-1","namespace":"lloyd","state":{}}|}
    ; {|{"client_id":"client-1","namespace":null}|}
    ; {|{"client_id":"client-1"}|}
    ; {|{"client_id":"client-1","host_id":"host-abc"}|}
    ; {|{"namespace":"lloyd"}|}
    ; {|{"client_id":"client-1","namespace":3}|}
    ]
    ~f:(fun json ->
      print_s
        [%sexp
          (Hello_reply.of_json (Or_error.ok_exn (Json.parse json))
           : Hello_reply.t Or_error.t)]);
  [%expect
    {|
    (Ok ((client_id client-1) (namespace (lloyd)) (user ())))
    (Ok (
      (client_id client-1)
      (namespace ())
      (user      ())))
    (Ok (
      (client_id client-1)
      (namespace ())
      (user      ())))
    (Ok (
      (client_id host-abc)
      (namespace ())
      (user      ())))
    (Error "missing field \"client_id\"")
    (Error "field \"namespace\": expected string, got 3")
    |}]
;;

let%expect_test "Json.parse agrees with Jsonaf, and says where it fails" =
  let docs =
    [ {|{"a":1,"b":[true,false,null],"c":{"d":"e\"\\\/\b\f\n\r\t"},"n":-1.5e3}|}
    ; {|  [ ]  |}
    ; {|{}|}
    ; {|"\u00e9\u4e2d\ud83d\ude00"|}
    ; {|[0, -0, 12.25, 1E2, 3e-4]|}
    ; {|{"nested":[[[{"x":[1,[2,[3]]]}]]]}|}
    ]
  in
  List.iter docs ~f:(fun doc ->
    let ours = Json.parse doc |> Or_error.ok_exn in
    let theirs = Jsonaf.parse doc |> Or_error.ok_exn in
    printf "%b %s\n" (Jsonaf.exactly_equal ours theirs) (Jsonaf.to_string ours));
  List.iter
    [ ""; "[1,]"; {|{"a" 1}|}; "tru"; {|"open|}; "[1] x"; "01"; {|"\q"|} ]
    ~f:(fun doc ->
      match Json.parse doc with
      | Ok j -> printf "%S parsed?! %s\n" doc (Jsonaf.to_string j)
      | Error e -> printf "%S: %s\n" doc (Error.to_string_hum e));
  [%expect
    {|
    true {"a":1,"b":[true,false,null],"c":{"d":"e\"\\/\b\f\n\r\t"},"n":-1.5e3}
    true []
    true {}
    true "é中😀"
    true [0,-0,12.25,1E2,3e-4]
    true {"nested":[[[{"x":[1,[2,[3]]]}]]]}
    "": invalid JSON at byte 0: unexpected character
    "[1,]": invalid JSON at byte 3: unexpected character
    "{\"a\" 1}": invalid JSON at byte 5: expected ':'
    "tru": invalid JSON at byte 0: unexpected token
    "\"open": invalid JSON at byte 5: unterminated string
    "[1] x": invalid JSON at byte 4: trailing characters
    "01": invalid JSON at byte 0: leading zero
    "\"\\q\"": invalid JSON at byte 2: bad escape
    |}]
;;

let%expect_test "skills and MCP requests" =
  List.iter
    [ Request.Method.List_skills
    ; List_mcp { reconnect = false }
    ; List_mcp { reconnect = true }
    ; Mcp_approve { source = "/p/.mcp.json"; server = "fs" }
    ]
    ~f:(fun m -> print_endline (Request.to_line (Request.create ~id:1 m)));
  [%expect
    {|
    {"id":1,"method":"list_skills","params":{}}
    {"id":1,"method":"list_mcp","params":{}}
    {"id":1,"method":"list_mcp","params":{"reconnect":true}}
    {"id":1,"method":"mcp_approve","params":{"source":"/p/.mcp.json","server":"fs"}}
    |}]
;;

let%expect_test "list_skills and list_mcp results" =
  let show of_json sexp_of json =
    match Or_error.bind (Json.parse json) ~f:of_json with
    | Ok v -> print_s (sexp_of v)
    | Error e -> print_s [%message "error" (e : Error.t)]
  in
  show
    (fun j -> Json.list_field j "skills" ~f:Skill.of_json)
    [%sexp_of: Skill.t list]
    {|{"skills":[{"name":"frontend-design","description":"Build UIs","path":"/p/.claude/skills/frontend-design/SKILL.md","model_invocable":true},{"name":"release","description":"Cut a release","path":"/h/.prigh/skills/release/SKILL.md","model_invocable":false}]}|};
  show
    Mcp_list.of_json
    Mcp_list.sexp_of_t
    {|{"servers":[{"name":"fs","source":"/p/.mcp.json","project":true,"status":"ready","tools":[{"name":"mcp__fs__read_file","description":"Read a file"}]},{"name":"gh","source":"/h/.prigh/mcp.json","project":false,"status":"failed","error":"exited 1","tools":[]},{"name":"db","source":"/p/.mcp.json","project":true,"status":"needs_approval"}],"problems":["/p/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" (http)"]}|};
  show Mcp_list.of_json Mcp_list.sexp_of_t {|{"servers":[]}|};
  show
    Mcp_list.of_json
    Mcp_list.sexp_of_t
    {|{"servers":[{"name":"fs","source":"/p/.mcp.json","project":true,"status":"sleeping"}],"problems":[]}|};
  [%expect
    {|
    (((name        frontend-design)
      (description "Build UIs")
      (path /p/.claude/skills/frontend-design/SKILL.md)
      (model_invocable true))
     ((name            release)
      (description     "Cut a release")
      (path            /h/.prigh/skills/release/SKILL.md)
      (model_invocable false)))
    ((servers (
       ((name    fs)
        (source  /p/.mcp.json)
        (project true)
        (status  Ready)
        (error ())
        (tools ((
          (name        mcp__fs__read_file)
          (description "Read a file")))))
       ((name    gh)
        (source  /h/.prigh/mcp.json)
        (project false)
        (status  Failed)
        (error ("exited 1"))
        (tools ()))
       ((name    db)
        (source  /p/.mcp.json)
        (project true)
        (status  Needs_approval)
        (error ())
        (tools ()))))
     (problems (
       "/p/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" (http)")))
    ((servers  ())
     (problems ()))
    (error (
      e (
        in servers[0] (
          "field \"status\"" "unknown MCP server status \"sleeping\""))))
    |}]
;;
