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
    {|{"type":"event","event":"state","state":{"session_id":"s1","session_path":"/tmp/s1.jsonl","cwd":"/work","model":{"id":"claude-fable-5-1","provider":"anthropic","key":"anthropic/claude-fable-5-1","name":"Claude Fable 5.1","context_window":1000000,"max_output":128000,"supports_thinking":true,"cost":{"input":10,"output":50,"cache_read":0.25}},"thinking":"low","running":false,"message_count":3,"usage":{"input":120,"output":40,"cache_read":10},"cost_usd":0.0032,"context_tokens":160}}|};
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
  [%expect
    {|
    (Event (
      State (
        (session_id   s1)
        (session_path /tmp/s1.jsonl)
        (cwd          /work)
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
        (context_tokens 160))))
    (Event (Message_start (User hi)))
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
    (Event (Agent_end ((User x))))
    (Event (Compacted s))
    (Event (Notice n))
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
    (Event (Auth (Prompt (id p1) (prompt (Secret (message "API key"))))))
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
    (fun j ->
      Or_error.map (Session_summary.of_json j) ~f:Session_summary.sexp_of_t)
    {|{"id":"abc","path":"/p","cwd":"/c","created_at":"2025-01-01T00:00:00Z","first_prompt":null,"message_count":0}|};
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
     (expires_ms (1700000000000)))
    ((provider openai)
     (name     OpenAI)
     (methods    ())
     (configured ())
     (expires_ms ()))
    ((id         abc)
     (path       /p)
     (cwd        /c)
     (created_at 2025-01-01T00:00:00Z)
     (first_prompt ())
     (message_count 0))
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
    (error (e "json: unexpected string: 'not'"))
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
