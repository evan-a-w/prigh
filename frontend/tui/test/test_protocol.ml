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
  decode
    {|{"type":"event","event":"queue_update","steer":2,"follow_up":1,"steer_texts":["a","b"],"follow_up_texts":["c"]}|};
  [%expect
    {|
    (Event (
      State (
        (session_id   s1)
        (session_path /tmp/s1.jsonl)
        (session_name ("my session"))
        (session_description ())
        (cwd /work)
        (home ())
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
        (hosts      ())
        (subagents  ())
        (jobs       ())
        (budget_usd ())
        (goal       ())
        (reminders  ())
        (resume_at  ())
        (approval_mode Auto)
        (plan_mode     false)
        (plan          ())
        (session_rules ()))))
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
    (Event (Queue_update (steer (a b)) (follow_up (c))))
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

let%expect_test
    "message and entry times (\"at\", ms since the epoch; absent means unknown)"
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
      Tool_confirm (
        (call_id c1)
        (agent_id ())
        (name    bash)
        (summary "rm -rf build")
        (rules ()))))
    (error (
      e (
        "while decoding"
        "{\"type\":\"event\",\"event\":\"tool_confirm\",\"name\":\"write\"}"
        "missing field \"call_id\"")))
    |}]
;;

let%expect_test "ask_user events: the deadline, and whether it timed out" =
  decode
    {|{"type":"event","event":"ask_user","call_id":"q1","questions":[{"question":"Sure?"}],"deadline_ms":1760191500000}|};
  decode
    {|{"type":"event","event":"ask_user","call_id":"q2","questions":[{"question":"Sure?"}],"deadline_ms":null}|};
  decode
    {|{"type":"event","event":"ask_user_done","call_id":"q1","timed_out":true}|};
  (* An older backend's, without [timed_out]. *)
  decode {|{"type":"event","event":"ask_user_done","call_id":"q2"}|};
  [%expect
    {|
    (Event (
      Ask_user (
        (call_id q1)
        (questions ((
          (question Sure?)
          (header  ())
          (choices ())
          (multiple false))))
        (deadline ("2025-10-11 14:05:00Z")))))
    (Event (
      Ask_user (
        (call_id q2)
        (questions ((
          (question Sure?)
          (header  ())
          (choices ())
          (multiple false))))
        (deadline ()))))
    (Event (
      Ask_user_done
      (call_id   q1)
      (timed_out true)))
    (Event (
      Ask_user_done
      (call_id   q2)
      (timed_out false)))
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
  decode
    {|{"type":"event","event":"terminal_kill","host":"client-2","key":"sess-1"}|};
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
    (Event (Terminal_kill sess-1))
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
      (cwd   /work)
      (stdin false)))
    (Event (Tool_exec_cancel c1-0))
    (Event (
      State (
        (session_id   abc123)
        (session_path /home/u/.prigh/sessions/1.jsonl)
        (session_name        ())
        (session_description ())
        (cwd /work)
        (home       ())
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
          (session_name ())
          (home         ()))))
        (subagents  ())
        (jobs       ())
        (budget_usd ())
        (goal       ())
        (reminders  ())
        (resume_at  ())
        (approval_mode Auto)
        (plan_mode     false)
        (plan          ())
        (session_rules ()))))
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
      (tools (read ls))
      (worktree ())))
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
     (live       false)
     (running    false)
     (background 0)
     (clients    0)
     (archived   false)
     (bytes      0)
     (host      ())
     (repo      ())
     (worktrees ())
     (home      ()))
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
     (live       false)
     (running    false)
     (background 0)
     (clients    0)
     (archived   false)
     (bytes      0)
     (host      ())
     (repo      ())
     (worktrees ())
     (home      ()))
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
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"snippets":[{"name":"review","text":"Review it.\nBriefly."}]}|};
  show
    (fun j -> Or_error.map (Config.of_json j) ~f:Config.sexp_of_t)
    {|{"snippets":[{"name":"review"}]}|};
  print_s
    [%sexp
      (Config.to_json
         { Config.scoped_models = [ "a"; "b" ]
         ; approval_mode = Ask
         ; permissions = Config.Permissions.empty
         ; default_model = Some "deepseek/deepseek-flash"
         ; default_thinking = Some "max"
         ; fallback_models =
             [ "openai-codex/gpt-6-sol"; "deepseek/deepseek-flash" ]
         ; default_cwd = Some "~/proj"
         ; worktrees_dir = Some "~/trees"
         ; archive_after_days = 7.
         ; notify = "desktop"
         ; snippets =
             [ { name = "review"; text = "Review the diff.\nBe brief." } ]
         }
       : Json.t)];
  decode
    {|{"type":"event","event":"config_changed","config":{"scoped_models":["a"],"confirm_tools":false}}|};
  [%expect
    {|
    ((scoped_models (anthropic/claude-fable-5-1 deepseek/deepseek-flash))
     (approval_mode Ask)
     (permissions (
       (allow ())
       (ask   ())
       (deny  ())))
     (default_model    ())
     (default_thinking ())
     (fallback_models  ())
     (default_cwd      ())
     (worktrees_dir    ())
     (archive_after_days 7)
     (notify             bell)
     (snippets ()))
    ((scoped_models ())
     (approval_mode Auto)
     (permissions (
       (allow ())
       (ask   ())
       (deny  ())))
     (default_model    ())
     (default_thinking ())
     (fallback_models  ())
     (default_cwd      ())
     (worktrees_dir    ())
     (archive_after_days 7)
     (notify             bell)
     (snippets ()))
    ((scoped_models ())
     (approval_mode Auto)
     (permissions (
       (allow ())
       (ask   ())
       (deny  ())))
     (default_model ())
     (default_thinking (low))
     (fallback_models ())
     (default_cwd     ())
     (worktrees_dir   ())
     (archive_after_days 7)
     (notify             bell)
     (snippets ()))
    (decode (e "default_model must be a string"))
    ((scoped_models ())
     (approval_mode Auto)
     (permissions (
       (allow ())
       (ask   ())
       (deny  ())))
     (default_model    ())
     (default_thinking ())
     (fallback_models (openai-codex/gpt-6-sol anthropic/claude-opus-5-5))
     (default_cwd (/srv/work))
     (worktrees_dir ())
     (archive_after_days 7)
     (notify             bell)
     (snippets ()))
    (decode (e "fallback_models must be an array of strings"))
    (decode (e "default_cwd must be a string"))
    ((scoped_models ())
     (approval_mode Auto)
     (permissions (
       (allow ())
       (ask   ())
       (deny  ())))
     (default_model    ())
     (default_thinking ())
     (fallback_models  ())
     (default_cwd      ())
     (worktrees_dir    ())
     (archive_after_days 7)
     (notify             bell)
     (snippets ((
       (name review)
       (text "Review it.\nBriefly.")))))
    (decode (e (in snippets[0] "missing field \"text\"")))
    ((scoped_models (a b))
     (approval_mode ask)
     (permissions (
       (allow ())
       (ask   ())
       (deny  ())))
     (default_model    deepseek/deepseek-flash)
     (default_thinking max)
     (fallback_models (openai-codex/gpt-6-sol deepseek/deepseek-flash))
     (default_cwd        ~/proj)
     (worktrees_dir      ~/trees)
     (archive_after_days 7)
     (notify             desktop)
     (snippets ((
       (name review)
       (text "Review the diff.\nBe brief.")))))
    (Event (
      Config_changed (
        (scoped_models (a))
        (approval_mode Auto)
        (permissions (
          (allow ())
          (ask   ())
          (deny  ())))
        (default_model    ())
        (default_thinking ())
        (fallback_models  ())
        (default_cwd      ())
        (worktrees_dir    ())
        (archive_after_days 7)
        (notify             bell)
        (snippets ()))))
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
    (Ok ((client_id client-1) (namespace (lloyd)) (user ()) (superuser false)))
    (Ok (
      (client_id client-1)
      (namespace ())
      (user      ())
      (superuser false)))
    (Ok (
      (client_id client-1)
      (namespace ())
      (user      ())
      (superuser false)))
    (Ok (
      (client_id host-abc)
      (namespace ())
      (user      ())
      (superuser false)))
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

let%expect_test "archived sessions and deleting them" =
  let parse s = Json.parse s |> Or_error.ok_exn in
  List.iter
    [ Request.Method.Set_archived { path = "/s/a.jsonl"; archived = true }
    ; Delete_sessions [ "/s/a.jsonl"; "/s/b.jsonl" ]
    ; Delete_archived
    ]
    ~f:(fun m -> print_endline (Request.to_line (Request.create ~id:1 m)));
  print_s
    [%sexp
      (Session_summary.of_json
         (parse
            {|{"id":"a","path":"/p","cwd":"/c","created_at":"x","message_count":2,"live":true,"running":true,"background":2,"clients":1,"archived":true,"bytes":2048}|})
       : Session_summary.t Or_error.t)];
  print_s
    [%sexp
      (Entry.of_json
         (parse {|{"id":"e9","parent":"e8","kind":"archived","archived":true}|})
       : Entry.t Or_error.t)];
  decode {|{"type":"event","event":"sessions_changed"}|};
  let result =
    Delete_result.of_json
      (parse
         {|{"deleted":[{"path":"/s/a.jsonl","id":"a","bytes":1500},{"path":"/s/b.jsonl","id":"b","bytes":2000000}],"refused":[{"path":"/s/c.jsonl","error":"session c is running: wait for it or /abort it first"}]}|})
    |> Or_error.ok_exn
  in
  print_s [%sexp (result : Delete_result.t)];
  print_endline (Delete_result.summary result);
  print_endline
    (Delete_result.summary { deleted = []; refused = [] }
     ^ " / "
     ^ String.concat
         ~sep:", "
         (List.map
            [ 0; 1023; 1024; 1_500_000; 3_000_000_000 ]
            ~f:Session_summary.size_string));
  [%expect
    {|
    {"id":1,"method":"set_archived","params":{"path":"/s/a.jsonl","archived":true}}
    {"id":1,"method":"delete_sessions","params":{"paths":["/s/a.jsonl","/s/b.jsonl"]}}
    {"id":1,"method":"delete_archived","params":{}}
    (Ok (
      (id   a)
      (path /p)
      (name        ())
      (description ())
      (cwd        /c)
      (created_at x)
      (updated_at   ())
      (first_prompt ())
      (message_count 2)
      (parent ())
      (live       true)
      (running    true)
      (background 2)
      (clients    1)
      (archived   true)
      (bytes      2048)
      (host      ())
      (repo      ())
      (worktrees ())
      (home      ())))
    (Ok ((id e9) (parent (e8)) (kind (Archived (archived true)))))
    (Event Sessions_changed)
    ((deleted (
       ((path /s/a.jsonl) (id a) (bytes 1500))
       ((path /s/b.jsonl) (id b) (bytes 2000000))))
     (refused ((
       (path /s/c.jsonl)
       (error "session c is running: wait for it or /abort it first")))))
    Deleted 2 sessions (1.9 MB)
    Not deleted: session c is running: wait for it or /abort it first
    Deleted nothing / 0 B, 1023 B, 1.0 KB, 1.4 MB, 2.8 GB
    |}]
;;

let%expect_test "Moved_job: a foreground bash call that became a job" =
  List.iter
    [ "still running after 30s, so it was moved to the background as job j3 \
       (not restarted: it keeps running). Output so far (last 2 of 9 lines):\n\
       eight\n\
       nine\n\
       Its report ([job j3 exited ...] and its last lines) is delivered to you \
       when it exits."
    ; "still running after 0.5s, so it was moved to the background as job j12 \
       (not restarted: it keeps running). No output so far.\n\
       Its report ([job j12 exited ...]) is delivered to you when it exits."
    ; "started job j1: make"
    ; "still running after 30s"
    ]
    ~f:(fun text -> print_s [%sexp (Moved_job.parse text : Moved_job.t option)]);
  [%expect
    {|
    ((
      (id     j3)
      (after  30s)
      (output "eight\nnine")))
    ((
      (id     j12)
      (after  0.5s)
      (output "")))
    ()
    ()
    |}]
;;

let%expect_test "reminders in the state, as the backend sends them" =
  List.iter
    [ {|{"name":"r1","text":"Keep going.\nReally.","on":"stop","max_repeats":3,"repeats":2}|}
    ; {|{"name":"r2","text":"Check disk.","every_turns":1}|}
    ; {|{"name":"r3","text":"x","every_turns":40}|}
    ; {|{"name":"r4","text":"x","on":"stop"}|}
    ]
    ~f:(fun json ->
      match Reminder.of_json (Or_error.ok_exn (Json.parse json)) with
      | Ok r ->
        print_s [%sexp (r : Reminder.t)];
        print_endline (Reminder.Trigger.describe r.trigger)
      | Error e -> print_s [%message "error" (e : Error.t)]);
  [%expect
    {|
    ((name r1)
     (text "Keep going.\nReally.")
     (trigger (
       On_stop
       (max_repeats 3)
       (repeats     2))))
    on stop 2/3
    ((name r2)
     (text "Check disk.")
     (trigger (Every_turns 1)))
    every turn
    ((name r3)
     (text x)
     (trigger (Every_turns 40)))
    every 40 turns
    (error (e "missing field \"max_repeats\""))
    |}]
;;

let%expect_test "worktree requests and replies" =
  List.iter
    [ Request.Method.List_worktrees { cwd = None }
    ; List_worktrees { cwd = Some "/t/other" }
    ; Create_worktree { branch = "feat/a"; base = None; cwd = None }
    ; Create_worktree { branch = "b"; base = Some "main"; cwd = Some "/t/repo" }
    ; Remove_worktree { path = "/t/repo/b"; force = false; cwd = None }
    ; Remove_worktree { path = "/t/repo/b"; force = true; cwd = Some "/t/repo" }
    ]
    ~f:(fun m -> print_endline (Request.to_line (Request.create ~id:1 m)));
  [%expect
    {|
    {"id":1,"method":"list_worktrees","params":{}}
    {"id":1,"method":"list_worktrees","params":{"cwd":"/t/other"}}
    {"id":1,"method":"create_worktree","params":{"branch":"feat/a"}}
    {"id":1,"method":"create_worktree","params":{"branch":"b","base":"main","cwd":"/t/repo"}}
    {"id":1,"method":"remove_worktree","params":{"path":"/t/repo/b"}}
    {"id":1,"method":"remove_worktree","params":{"path":"/t/repo/b","force":true,"cwd":"/t/repo"}}
    |}];
  let show of_json sexp_of json =
    match Or_error.bind (Json.parse json) ~f:of_json with
    | Ok v -> print_s (sexp_of v)
    | Error e -> print_s [%message "error" (e : Error.t)]
  in
  show
    Worktree.Listing.of_json
    Worktree.Listing.sexp_of_t
    {|{"root":"/r/repo","project_dir":"/h/.prigh/worktrees/repo","worktrees":[{"path":"/r/repo","branch":"main","head":"0123456789abcdef0123456789abcdef01234567","main":true,"managed":false,"locked":false,"prunable":false,"changes":0,"upstream":"origin/main","ahead":1,"behind":2,"current":true,"sessions":[{"id":"s1","path":"/s/s1.jsonl","name":"in main","live":true,"running":false}]},{"path":"/h/.prigh/worktrees/repo/x","branch":null,"head":"89abcdef0123456789abcdef0123456789abcdef","main":false,"managed":true,"locked":true,"prunable":false,"changes":3,"upstream":null,"ahead":0,"behind":0,"current":false,"sessions":[]},{"path":"/gone","branch":"g","head":null,"prunable":true,"changes":null}]}|};
  show
    Worktree.Created.of_json
    Worktree.Created.sexp_of_t
    {|{"worktree":{"path":"/w/b","branch":"b","head":null,"main":false,"managed":true},"new_branch":true}|};
  show
    Worktree.removed_of_json
    Worktree.sexp_of_t
    {|{"worktree":{"branch":"b"}}|};
  [%expect
    {|
    ((root        /r/repo)
     (project_dir /h/.prigh/worktrees/repo)
     (worktrees (
       ((path /r/repo)
        (branch (main))
        (head   (0123456789abcdef0123456789abcdef01234567))
        (main     true)
        (managed  false)
        (locked   false)
        (prunable false)
        (changes  (0))
        (upstream (origin/main))
        (ahead   1)
        (behind  2)
        (current true)
        (sessions ((
          (id   s1)
          (path /s/s1.jsonl)
          (name ("in main"))
          (live    true)
          (running false)))))
       ((path /h/.prigh/worktrees/repo/x)
        (branch ())
        (head (89abcdef0123456789abcdef0123456789abcdef))
        (main     false)
        (managed  true)
        (locked   true)
        (prunable false)
        (changes (3))
        (upstream ())
        (ahead   0)
        (behind  0)
        (current false)
        (sessions ()))
       ((path /gone)
        (branch (g))
        (head ())
        (main     false)
        (managed  false)
        (locked   false)
        (prunable true)
        (changes  ())
        (upstream ())
        (ahead   0)
        (behind  0)
        (current false)
        (sessions ())))))
    ((worktree (
       (path /w/b)
       (branch (b))
       (head ())
       (main     false)
       (managed  true)
       (locked   false)
       (prunable false)
       (changes  ())
       (upstream ())
       (ahead   0)
       (behind  0)
       (current false)
       (sessions ())))
     (new_branch true)
     (root ())
     (cwd  ()))
    (error (e "missing field \"path\""))
    |}]
;;

let%expect_test "git hosting: requests" =
  List.iter
    [ Request.Method.Forge_status
    ; Forge_login { host = "github.com"; kind = None; method_ = None }
    ; Forge_login
        { host = "codeberg.org"; kind = Some "forgejo"; method_ = Some "token" }
    ; Forge_logout "codeberg.org"
    ; Pr_list { state = None }
    ; Pr_list { state = Some "all" }
    ; Pr_view 12
    ; Pr_defaults { dir = None }
    ; Pr_create
        { dir = None
        ; title = "Add x"
        ; body = "Because"
        ; base = None
        ; head = None
        ; draft = false
        ; push = true
        }
    ; Pr_create
        { dir = None
        ; title = "WIP"
        ; body = ""
        ; base = Some "dev"
        ; head = Some "feat/y"
        ; draft = true
        ; push = false
        }
    ; Pr_checkout 7
    ]
    ~f:(fun m -> print_endline (Request.to_line (Request.create ~id:1 m)));
  [%expect
    {|
    {"id":1,"method":"forge_status","params":{}}
    {"id":1,"method":"forge_login","params":{"host":"github.com"}}
    {"id":1,"method":"forge_login","params":{"host":"codeberg.org","kind":"forgejo","method":"token"}}
    {"id":1,"method":"forge_logout","params":{"host":"codeberg.org"}}
    {"id":1,"method":"pr_list","params":{}}
    {"id":1,"method":"pr_list","params":{"state":"all"}}
    {"id":1,"method":"pr_view","params":{"number":12}}
    {"id":1,"method":"pr_defaults","params":{}}
    {"id":1,"method":"pr_create","params":{"title":"Add x","body":"Because","draft":false,"push":true}}
    {"id":1,"method":"pr_create","params":{"title":"WIP","body":"","base":"dev","head":"feat/y","draft":true,"push":false}}
    {"id":1,"method":"pr_checkout","params":{"number":7}}
    |}]
;;

let%expect_test "git hosting: replies and auth events" =
  let show of_json sexp_of json =
    match Or_error.bind (Json.parse json) ~f:of_json with
    | Ok v -> print_s (sexp_of v)
    | Error e -> print_s [%message "error" (e : Error.t)]
  in
  show
    Forge_account.list_of_json
    [%sexp_of: Forge_account.t list]
    {|{"accounts":[{"host":"github.com","kind":"github","label":"GitHub","user":"octo","method":"oauth","expires_ms":null,"oauth":true,"oauth_note":null},{"host":"codeberg.org","kind":"forgejo","label":"Forgejo","user":null,"method":null,"expires_ms":null,"oauth":true,"oauth_note":"the consent page names git-credential-oauth"}]}|};
  let repo =
    {|{"host":"github.com","kind":"github","owner":"evan-a-w","name":"prigh","remote":"origin","url":"https://github.com/evan-a-w/prigh"}|}
  in
  let pr =
    {|{"number":24,"title":"Git hosting","state":"open","draft":true,"author":"lloyd","head":"feat/git-hosting","base":"main","url":"https://github.com/evan-a-w/prigh/pull/24","body":"Closes #1","created_at":"2026-10-25T10:00:00Z","updated_at":"2026-10-25T11:00:00Z"}|}
  in
  show
    Pr_list.of_json
    Pr_list.sexp_of_t
    (sprintf {|{"repo":%s,"prs":[%s]}|} repo pr);
  show
    Pr_detail.of_json
    Pr_detail.sexp_of_t
    (sprintf
       {|{"repo":%s,"pr":%s,"checks":{"state":"failure","items":[{"name":"build","state":"success","url":"https://ci/1"},{"name":"lint","state":"failure","url":null}]},"diff":"diff --git a/x b/x\n","truncated":false}|}
       repo
       pr);
  (* pr_create's reply *)
  show
    (fun j -> Or_error.map (Pr_detail.of_json j) ~f:Pr_detail.git_diff)
    Git_diff.sexp_of_t
    (sprintf {|{"repo":%s,"pr":%s}|} repo pr);
  show
    Pr_defaults.of_json
    Pr_defaults.sexp_of_t
    (sprintf
       {|{"repo":%s,"head":"feat/x","base":"main","title":"Add x","body":"- Add x\n- Test x","commits":["Add x","Test x"],"pushed":false}|}
       repo);
  show
    Pr_checkout.of_json
    Pr_checkout.sexp_of_t
    {|{"path":"/src/prigh-wt/pr-24","branch":"pr-24","created":true}|};
  decode
    {|{"type":"event","event":"auth","kind":"forge_done","host":"github.com","user":"octo","method":"oauth"}|};
  decode
    {|{"type":"event","event":"auth","kind":"forge_logged_out","host":"codeberg.org"}|};
  [%expect
    {|
    (((host  github.com)
      (kind  github)
      (label GitHub)
      (user    (octo))
      (method_ (oauth))
      (expires_ms ())
      (oauth true)
      (oauth_note ()))
     ((host  codeberg.org)
      (kind  forgejo)
      (label Forgejo)
      (user       ())
      (method_    ())
      (expires_ms ())
      (oauth true)
      (oauth_note ("the consent page names git-credential-oauth"))))
    ((repo (
       (host   github.com)
       (kind   github)
       (owner  evan-a-w)
       (name   prigh)
       (remote origin)
       (url    https://github.com/evan-a-w/prigh)))
     (prs ((
       (number 24)
       (title  "Git hosting")
       (state  Open)
       (draft  true)
       (author lloyd)
       (head   feat/git-hosting)
       (base   main)
       (url    https://github.com/evan-a-w/prigh/pull/24)
       (body   "Closes #1")
       (created_at 2026-10-25T10:00:00Z)
       (updated_at 2026-10-25T11:00:00Z)))))
    ((repo (
       (host   github.com)
       (kind   github)
       (owner  evan-a-w)
       (name   prigh)
       (remote origin)
       (url    https://github.com/evan-a-w/prigh)))
     (pr (
       (number 24)
       (title  "Git hosting")
       (state  Open)
       (draft  true)
       (author lloyd)
       (head   feat/git-hosting)
       (base   main)
       (url    https://github.com/evan-a-w/prigh/pull/24)
       (body   "Closes #1")
       (created_at 2026-10-25T10:00:00Z)
       (updated_at 2026-10-25T11:00:00Z)))
     (checks ((
       (state failure)
       (items (
         ((name  build)
          (state success)
          (url (https://ci/1)))
         ((name  lint)
          (state failure)
          (url ())))))))
     (diff      "diff --git a/x b/x\n")
     (truncated false))
    ((root      evan-a-w/prigh)
     (against   main)
     (diff      "")
     (truncated false)
     (branch (feat/git-hosting))
     (base main)
     (main true)
     (branches        ())
     (remote_branches ())
     (tags            ())
     (commits         ())
     (rev             ())
     (direct false))
    ((repo (
       (host   github.com)
       (kind   github)
       (owner  evan-a-w)
       (name   prigh)
       (remote origin)
       (url    https://github.com/evan-a-w/prigh)))
     (head  feat/x)
     (base  main)
     (title "Add x")
     (body  "- Add x\n- Test x")
     (commits ("Add x" "Test x"))
     (pushed false))
    ((path    /src/prigh-wt/pr-24)
     (branch  pr-24)
     (created true))
    (Event (Auth (Forge_done (host github.com) (user (octo)) (method_ oauth))))
    (Event (Auth (Forge_logged_out codeberg.org)))
    |}]
;;
