open! Core
module P = Prigh_protocol

let model_json
  ?(provider = "anthropic")
  ?(supports_thinking = true)
  ?(context_window = 1000000)
  id
  name
  =
  sprintf
    {|{"id":"%s","provider":"%s","key":"%s/%s","name":"%s","context_window":%d,"max_output":128000,"supports_thinking":%b,"cost":{"input":10,"output":50,"cache_read":1}}|}
    id
    provider
    provider
    id
    name
    context_window
    supports_thinking
;;

let models_json =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ model_json "claude-fable-5" "Claude Fable 5"
       ; model_json "claude-fable-5-1" "Claude Fable 5.1"
       ; model_json ~provider:"openai" "gpt-5.5" "GPT-5.5"
       ; model_json ~provider:"deepseek" "deepseek-flash" "DeepSeek V4.1 Flash"
       ])
;;

let state_json
  ?(model =
    model_json ~provider:"deepseek" "deepseek-flash" "DeepSeek V4.1 Flash")
  ?(running = false)
  ?(cwd = "/work")
  ?git_branch
  ?(thinking = "off")
  ?(context_tokens = 1500)
  ?(cost_usd = 0.0123)
  ?session_name
  ()
  =
  sprintf
    {|{"session_id":"abc123","session_path":"/home/u/.prigh/sessions/1.jsonl","session_name":%s,"cwd":%s,"git_branch":%s,"model":%s,"thinking":%s,"running":%b,"message_count":2,"usage":{"input":1200,"output":300,"cache_read":0},"cost_usd":%g,"context_tokens":%d}|}
    (match session_name with
     | Some name -> P.Json.to_string (P.Json.str name)
     | None -> "null")
    (P.Json.to_string (P.Json.str cwd))
    (match git_branch with
     | Some branch -> P.Json.to_string (P.Json.str branch)
     | None -> "null")
    model
    (P.Json.to_string (P.Json.str thinking))
    running
    cost_usd
    context_tokens
;;

let state
  ?model
  ?running
  ?cwd
  ?git_branch
  ?thinking
  ?context_tokens
  ?cost_usd
  ?session_name
  ()
  =
  Or_error.ok_exn
    (P.State.of_json
       (Or_error.ok_exn
          (P.Json.parse
             (state_json
                ?model
                ?running
                ?cwd
                ?git_branch
                ?thinking
                ?context_tokens
                ?cost_usd
                ?session_name
                ()))))
;;

let sessions_json =
  {|[{"id":"1","path":"/home/u/.prigh/sessions/1.jsonl","name":"build fix","cwd":"/work","created_at":"2025-06-01T10:00:00Z","updated_at":"2025-06-01T10:00:11Z","first_prompt":"fix the build\nplease","message_count":12,"parent":null},{"id":"2","path":"/home/u/.prigh/sessions/2.jsonl","name":null,"cwd":"/other","created_at":"2025-06-02T11:30:00Z","updated_at":"2025-06-02T11:30:22Z","first_prompt":null,"message_count":0,"parent":null}]|}
;;

let entries_json =
  {|{"head":"u2","entries":[{"id":"u1","parent":null,"kind":"message","message":{"role":"user","text":"first question"}},{"id":"a1","parent":"u1","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"answer one"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}},{"id":"u2","parent":"a1","kind":"message","message":{"role":"user","text":"second question\nmore"}}]}|}
;;

let tree_json =
  {|{"head":"a2","entries":[{"id":"u1","parent":null,"kind":"message","message":{"role":"user","text":"root question"}},{"id":"a1","parent":"u1","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"first answer"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}},{"id":"a2","parent":"a1","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"second answer"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}},{"id":"ab","parent":"u1","kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"branch answer"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}},{"id":"t1","parent":"ab","kind":"message","message":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"branch tool","is_error":false}}]}|}
;;

let stats_json =
  {|{"message_count":4,"turns":2,"tool_calls":{"bash":1,"read":2},"usage":{"input":30,"output":13,"cache_read":5},"cost_usd":0.0001,"context_percent":1.5,"model_changes":1,"compactions":0,"duration_seconds":12.5}|}
;;

let auth_json =
  {|[{"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Claude Pro/Max"},{"method":"api_key","label":"API key"}],"configured":null,"expires_ms":null},{"provider":"openai","name":"OpenAI","methods":[{"method":"api_key","label":"API key"}],"configured":null,"expires_ms":null},{"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"API key"}],"configured":{"method":"api_key","source":"auth.json"},"expires_ms":null}]|}
;;

let config_json = {|{"scoped_models":[],"confirm_tools":false}|}

let messages_json =
  {|[{"role":"user","text":"earlier question"},{"role":"assistant","content":[{"type":"text","text":"earlier **answer**"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}]|}
;;

let assistant ?(stop = "end_turn") text =
  Or_error.ok_exn
    (P.Message.of_json
       (Or_error.ok_exn
          (P.Json.parse
             (sprintf
                {|{"role":"assistant","content":[{"type":"text","text":%s}],"stop_reason":{"type":"%s"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}|}
                (P.Json.to_string (P.Json.str text))
                stop))))
;;

let partial =
  match assistant "" with
  | P.Message.Assistant a -> a
  | _ -> assert false
;;

let tool_call ?(name = "bash") ?(arguments = "{}") id : P.Tool_call.t =
  { id; name; arguments }
;;

let tool_result ?(name = "bash") ?(is_error = false) ~id text
  : P.Message.Tool_result.t
  =
  { tool_call_id = id; tool_name = name; text; is_error }
;;
