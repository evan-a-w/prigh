open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* A pi-protocol client on an in-memory line pair: [send] feeds one command,
   [dump] prints everything the adapter wrote so far. *)
module H = struct
  type t =
    { sandbox : Tool_test_helpers.t
    ; server : Rpc_server.t
    ; login : Login_manager.t
    ; input : string option Eio.Stream.t
    ; output : string Queue.t
    ; mutable now : int
    }

  let send t line = Eio.Stream.add t.input (Some line)

  let settle t =
    for _ = 1 to 20 do
      Eio.Fiber.yield ()
    done;
    ignore t
  ;;

  let dump t =
    Queue.iter t.output ~f:(fun line ->
      mask t.sandbox line
      |> String.substr_replace_all
           ~pattern:(Core_unix.gethostname () ^ " (")
           ~with_:"<host> ("
      |> print_endline);
    Queue.clear t.output
  ;;

  (* Yields until a line containing [substring] has been written. *)
  let wait_for t substring =
    let seen () = Queue.exists t.output ~f:(String.is_substring ~substring) in
    let clock = Eio.Stdenv.clock t.sandbox.env in
    let rec go n =
      if seen () || n = 0
      then ()
      else (
        Eio.Time.sleep clock 0.005;
        go (n - 1))
    in
    go 2000;
    settle t
  ;;
end

let with_server
      ?tools
      ?token
      ?hello_token
      ?session
      ?(confirm_tools = false)
      replies
      f
  =
  with_sandbox
  @@ fun sandbox ->
  if confirm_tools
  then write sandbox ".prigh/config.json" {|{"confirm_tools": true}|};
  Eio.Switch.run
  @@ fun sw ->
  let provider = Faux_provider.create replies in
  let login =
    Login_manager.create
      ~env:sandbox.env
      ~sw
      ~getenv:(fun _ -> None)
      ~store:(Auth_store.create ~path:(Filename.concat sandbox.dir "auth.json"))
      ()
  in
  let sessions_dir = Filename.concat sandbox.dir "sessions" in
  let tools =
    Option.value
      tools
      ~default:
        (Tools.all
         @ [ Tool_subagent.create
               ~provider
               ~current_model:(fun () -> Model.default)
               ~current_thinking:(fun () -> Off)
               ~home:"/nonexistent"
           ])
  in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:sandbox.env
      ~sw
      ~provider
      ~tools
      ~sessions_dir
      ~home:sandbox.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:sandbox.env
      ~sw
      ?token
      ~login
      ~sessions_dir
      ~cwd:sandbox.dir
      ~new_agent
      ()
  in
  let h =
    { H.sandbox
    ; server
    ; login
    ; input = Eio.Stream.create 100
    ; output = Queue.create ()
    ; now = 1000
    }
  in
  let finished, resolve = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
    Pi_rpc.serve_lines
      server
      ~now:(fun () ->
        h.now <- h.now + 1000;
        h.now)
      ?token:hello_token
      ?session
      ~read_line:(fun () -> Eio.Stream.take h.input)
      ~write_line:(Queue.enqueue h.output)
      ();
    Eio.Promise.resolve resolve ());
  H.settle h;
  f h;
  Eio.Stream.add h.input None;
  Eio.Promise.await finished
;;

let cmd h ?(fields = "") type_ =
  H.send h (sprintf {|{"id": "q", "type": "%s"%s}|} type_ fields);
  H.settle h
;;

let%expect_test "hello sets status entries; state, commands, models, stats" =
  with_server []
  @@ fun h ->
  H.dump h;
  cmd h "get_state";
  cmd h "get_commands";
  cmd h "get_session_stats";
  cmd h "get_messages";
  cmd h "nope";
  H.send h "not json";
  H.settle h;
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    {"id":"q","type":"response","command":"get_state","success":true,"data":{"model":{"id":"deepseek-flash","name":"DeepSeek V4.1 Flash","provider":"deepseek","reasoning":true,"contextWindow":1000000},"cwd":"$DIR","thinkingLevel":"off","isStreaming":false,"isCompacting":false,"steeringMode":"all","followUpMode":"all","sessionFile":"$DIR/sessions/<stamp>_<id>.jsonl","sessionId":"<id>","autoCompactionEnabled":true,"messageCount":0,"pendingMessageCount":0}}
    {"id":"q","type":"response","command":"get_commands","success":true,"data":{"commands":[{"name":"compact","description":"Compact the context","source":"builtin","argumentHint":"[instructions]"},{"name":"new","description":"Start a new session","source":"builtin"},{"name":"name","description":"Name the session","source":"builtin","argumentHint":"<name>"},{"name":"model","description":"Pick a model","source":"builtin","argumentHint":"[name]"},{"name":"thinking","description":"Set or cycle the thinking level","source":"builtin","argumentHint":"[level]"},{"name":"session","description":"Show session info","source":"builtin"},{"name":"export","description":"Export the session as markdown (on the backend)","source":"builtin","argumentHint":"[path]"},{"name":"copy","description":"Copy the last assistant message","source":"builtin"},{"name":"fork","description":"Fork the session from an earlier message","source":"builtin"},{"name":"clone","description":"Clone the session","source":"builtin"},{"name":"cd","description":"Change the working directory","source":"builtin","argumentHint":"[dir]"},{"name":"login","description":"Log in to a provider","source":"extension","argumentHint":"[provider] [oauth|api_key]"},{"name":"logout","description":"Log out of a provider","source":"extension","argumentHint":"<provider>"},{"name":"auth","description":"Show provider credentials","source":"extension"},{"name":"sessions","description":"Pick a saved session to switch to","source":"extension"},{"name":"switch","description":"Switch to a session by id or path","source":"extension","argumentHint":"<id|path>"},{"name":"host","description":"Pick where tools run","source":"extension"},{"name":"change_default","description":"Save the current model and thinking level as the default","source":"extension"},{"name":"help","description":"List the commands","source":"extension"}]}}
    {"id":"q","type":"response","command":"get_session_stats","success":true,"data":{"sessionFile":"$DIR/sessions/<stamp>_<id>.jsonl","sessionId":"<id>","userMessages":0,"assistantMessages":0,"toolCalls":0,"toolResults":0,"totalMessages":0,"tokens":{"input":0,"output":0,"cacheRead":0,"cacheWrite":0,"total":0},"cost":0,"contextUsage":{"tokens":0,"contextWindow":1000000,"percent":0}}}
    {"id":"q","type":"response","command":"get_messages","success":true,"data":{"messages":[]}}
    {"id":"q","type":"response","command":"nope","success":false,"error":"command \"nope\" is not supported by prigh"}
    {"id":null,"type":"response","command":"invalid","success":false,"error":"invalid JSON: json: unexpected string: 'not'"}
    |}]
;;

let%expect_test "a prompt streams pi events with stable timestamps" =
  with_server
    [ Reply.tool_call ~text:"Looking." ~id:"c1" ~name:"ls" ~arguments:"{}" ()
    ; Reply.text "Empty."
    ]
  @@ fun h ->
  H.dump h;
  cmd h ~fields:{|, "message": "what is here?"|} "prompt";
  H.wait_for h "agent_settled";
  H.dump h;
  cmd h "get_messages";
  cmd h "get_session_stats";
  cmd h "get_last_assistant_text";
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    {"type":"agent_start"}
    {"type":"message_start","message":{"role":"user","content":"what is here?","timestamp":0}}
    {"type":"message_end","message":{"role":"user","content":"what is here?","timestamp":0}}
    {"type":"turn_start"}
    {"type":"message_start","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1}}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"text","text":"Looking."}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1},"assistantMessageEvent":{"type":"text_delta"}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"toolCall","id":"c1","name":"ls","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1},"assistantMessageEvent":{"type":"tool_call_start"}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"toolCall","id":"c1","name":"ls","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1},"assistantMessageEvent":{"type":"tool_call_delta"}}
    {"type":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"toolCall","id":"c1","name":"ls","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1}}
    {"type":"tool_execution_start","toolCallId":"c1","toolName":"ls","args":{}}
    {"type":"tool_execution_end","toolCallId":"c1","toolName":"ls","args":{},"result":{"content":[{"type":"text","text":"sessions/\n"}]},"isError":false}
    {"type":"message_start","message":{"role":"toolResult","toolCallId":"c1","toolName":"ls","content":[{"type":"text","text":"sessions/\n"}],"isError":false,"timestamp":2}}
    {"type":"message_end","message":{"role":"toolResult","toolCallId":"c1","toolName":"ls","content":[{"type":"text","text":"sessions/\n"}],"isError":false,"timestamp":2}}
    {"type":"turn_end","message":{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"toolCall","id":"c1","name":"ls","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1},"toolResults":[{"role":"toolResult","toolCallId":"c1","toolName":"ls","content":[{"type":"text","text":"sessions/\n"}],"isError":false,"timestamp":2}]}
    {"type":"turn_start"}
    {"type":"message_start","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3},"assistantMessageEvent":{"type":"text_delta"}}
    {"type":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}}
    {"type":"turn_end","message":{"role":"assistant","content":[{"type":"text","text":"Empty."}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3},"toolResults":[]}
    {"type":"agent_end","messages":[{"role":"user","content":"what is here?","timestamp":0},{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"toolCall","id":"c1","name":"ls","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1},{"role":"toolResult","toolCallId":"c1","toolName":"ls","content":[{"type":"text","text":"sessions/\n"}],"isError":false,"timestamp":2},{"role":"assistant","content":[{"type":"text","text":"Empty."}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}],"willRetry":false}
    {"type":"agent_settled"}
    {"id":"q","type":"response","command":"get_messages","success":true,"data":{"messages":[{"role":"user","content":"what is here?","timestamp":0},{"role":"assistant","content":[{"type":"text","text":"Looking."},{"type":"toolCall","id":"c1","name":"ls","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1},{"role":"toolResult","toolCallId":"c1","toolName":"ls","content":[{"type":"text","text":"sessions/\n"}],"isError":false,"timestamp":2},{"role":"assistant","content":[{"type":"text","text":"Empty."}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}]}}
    {"id":"q","type":"response","command":"get_session_stats","success":true,"data":{"sessionFile":"$DIR/sessions/<stamp>_<id>.jsonl","sessionId":"<id>","userMessages":1,"assistantMessages":2,"toolCalls":1,"toolResults":1,"totalMessages":4,"tokens":{"input":30,"output":13,"cacheRead":5,"cacheWrite":0,"total":48},"cost":2.313e-05,"contextUsage":{"tokens":10,"contextWindow":1000000,"percent":0.001}}}
    {"id":"q","type":"response","command":"get_last_assistant_text","success":true,"data":{"text":"Empty."}}
    |}]
;;

let%expect_test "prompt while running steers; queue texts are forwarded" =
  with_server
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"sleep 0.3"}|}
        ()
    ; Reply.text "done"
    ]
  @@ fun h ->
  H.dump h;
  cmd h ~fields:{|, "message": "go"|} "prompt";
  H.wait_for h "tool_execution_start";
  cmd
    h
    ~fields:{|, "message": "also this", "streamingBehavior": "steer"|}
    "prompt";
  cmd h ~fields:{|, "message": "later"|} "follow_up";
  cmd h ~fields:{|, "message": "too soon"|} "prompt";
  H.wait_for h "\"steering\":[\"also this\"],\"followUp\":[\"later\"]";
  cmd h "abort";
  H.wait_for h "agent_settled";
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    {"type":"agent_start"}
    {"type":"message_start","message":{"role":"user","content":"go","timestamp":0}}
    {"type":"message_end","message":{"role":"user","content":"go","timestamp":0}}
    {"type":"turn_start"}
    {"type":"message_start","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1}}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1},"assistantMessageEvent":{"type":"tool_call_start"}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"sleep 0.3"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1},"assistantMessageEvent":{"type":"tool_call_delta"}}
    {"type":"message_end","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"sleep 0.3"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1}}
    {"type":"tool_execution_start","toolCallId":"c1","toolName":"bash","args":{"command":"sleep 0.3"}}
    {"type":"queue_update","steering":["also this"],"followUp":[]}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"queue_update","steering":["also this"],"followUp":["later"]}
    {"id":"q","type":"response","command":"follow_up","success":true,"data":{}}
    {"id":"q","type":"response","command":"prompt","success":false,"error":"a run is already in progress; use steer or follow_up"}
    {"type":"queue_update","steering":[],"followUp":[]}
    {"id":"q","type":"response","command":"abort","success":true,"data":{}}
    {"type":"tool_execution_end","toolCallId":"c1","toolName":"bash","args":{"command":"sleep 0.3"},"result":{"content":[{"type":"text","text":"[cancelled]"}]},"isError":true}
    {"type":"message_start","message":{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"[cancelled]"}],"isError":true,"timestamp":2}}
    {"type":"message_end","message":{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"[cancelled]"}],"isError":true,"timestamp":2}}
    {"type":"turn_end","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"sleep 0.3"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1},"toolResults":[{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"[cancelled]"}],"isError":true,"timestamp":2}]}
    {"type":"agent_end","messages":[{"role":"user","content":"go","timestamp":0},{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"sleep 0.3"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1},{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"[cancelled]"}],"isError":true,"timestamp":2}],"willRetry":false}
    {"type":"agent_settled"}
    |}]
;;

let%expect_test "tool confirmation is a confirm dialog" =
  with_server
    ~confirm_tools:true
    [ Reply.tool_call
        ~id:"c1"
        ~name:"bash"
        ~arguments:{|{"command":"echo hi"}|}
        ()
    ; Reply.text "after"
    ]
  @@ fun h ->
  H.dump h;
  cmd h ~fields:{|, "message": "go"|} "prompt";
  H.wait_for h "\"method\":\"confirm\"";
  H.dump h;
  H.send
    h
    {|{"type":"extension_ui_response","id":"confirm-c1","confirmed":true}|};
  H.wait_for h "agent_settled";
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    {"type":"agent_start"}
    {"type":"message_start","message":{"role":"user","content":"go","timestamp":0}}
    {"type":"message_end","message":{"role":"user","content":"go","timestamp":0}}
    {"type":"turn_start"}
    {"type":"message_start","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1}}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{}}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1},"assistantMessageEvent":{"type":"tool_call_start"}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"echo hi"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1},"assistantMessageEvent":{"type":"tool_call_delta"}}
    {"type":"message_end","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"echo hi"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1}}
    {"type":"tool_execution_start","toolCallId":"c1","toolName":"bash","args":{"command":"echo hi"}}
    {"type":"extension_ui_request","id":"confirm-c1","method":"confirm","title":"Run bash?","message":"echo hi"}
    {"type":"tool_execution_update","toolCallId":"c1","toolName":"bash","args":{"command":"echo hi"},"partialResult":{"content":[{"type":"text","text":"hi\n"}]}}
    {"type":"tool_execution_end","toolCallId":"c1","toolName":"bash","args":{"command":"echo hi"},"result":{"content":[{"type":"text","text":"hi\n"}]},"isError":false}
    {"type":"message_start","message":{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"hi\n"}],"isError":false,"timestamp":2}}
    {"type":"message_end","message":{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"hi\n"}],"isError":false,"timestamp":2}}
    {"type":"turn_end","message":{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"echo hi"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1},"toolResults":[{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"hi\n"}],"isError":false,"timestamp":2}]}
    {"type":"turn_start"}
    {"type":"message_start","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}}
    {"type":"message_update","message":{"role":"assistant","content":[{"type":"text","text":"after"}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3},"assistantMessageEvent":{"type":"text_delta"}}
    {"type":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"after"}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}}
    {"type":"turn_end","message":{"role":"assistant","content":[{"type":"text","text":"after"}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3},"toolResults":[]}
    {"type":"agent_end","messages":[{"role":"user","content":"go","timestamp":0},{"role":"assistant","content":[{"type":"toolCall","id":"c1","name":"bash","arguments":{"command":"echo hi"}}],"provider":"prigh","model":"deepseek-flash","stopReason":"toolUse","timestamp":1},{"role":"toolResult","toolCallId":"c1","toolName":"bash","content":[{"type":"text","text":"hi\n"}],"isError":false,"timestamp":2},{"role":"assistant","content":[{"type":"text","text":"after"}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}],"willRetry":false}
    {"type":"agent_settled"}
    |}]
;;

let last_response h =
  Queue.to_list h.H.output
  |> List.filter_map ~f:(fun line ->
    let json = Json.of_string line in
    Option.bind (Json.member "data" json) ~f:(fun data -> Some (line, data)))
  |> List.last_exn
;;

let%expect_test "models are listed in pi's shape" =
  with_server []
  @@ fun h ->
  cmd h "get_available_models";
  let _, data = last_response h in
  let models = Json.list_exn (Json.member_exn "models" data) in
  print_s [%sexp (List.length models : int)];
  List.iter (List.take models 2) ~f:(fun m -> print_endline (Json.to_string m));
  List.iter
    (List.drop models (List.length models - 1))
    ~f:(fun m -> print_endline (Json.to_string m));
  [%expect
    {|
    65
    {"id":"claude-fable-5","name":"Claude Fable 5","provider":"anthropic","reasoning":true,"contextWindow":1000000}
    {"id":"claude-fable-5-1","name":"Claude Fable 5.1","provider":"anthropic","reasoning":true,"contextWindow":1000000}
    {"id":"deepseek-v4-pro","name":"DeepSeek V4 Pro","provider":"deepseek","reasoning":true,"contextWindow":1000000}
    |}]
;;

let%expect_test
    "thinking levels: set, cycle, availability; model change is quiet"
  =
  with_server []
  @@ fun h ->
  H.dump h;
  cmd h "get_available_thinking_levels";
  cmd h ~fields:{|, "level": "xhigh"|} "set_thinking_level";
  cmd h "cycle_thinking_level";
  cmd h "cycle_thinking_level";
  cmd h ~fields:{|, "provider": "openai", "modelId": "gpt-4"|} "set_model";
  cmd h "get_available_thinking_levels";
  cmd h "cycle_thinking_level";
  cmd h ~fields:{|, "provider": "openai", "modelId": "gpt-9"|} "set_model";
  cmd h ~fields:{|, "name": "renamed"|} "set_session_name";
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    {"id":"q","type":"response","command":"get_available_thinking_levels","success":true,"data":{"levels":["off","low","medium","high","max"]}}
    {"type":"thinking_level_changed","level":"max"}
    {"id":"q","type":"response","command":"set_thinking_level","success":true,"data":{"level":"max"}}
    {"type":"thinking_level_changed","level":"off"}
    {"id":"q","type":"response","command":"cycle_thinking_level","success":true,"data":{"level":"off"}}
    {"type":"thinking_level_changed","level":"low"}
    {"id":"q","type":"response","command":"cycle_thinking_level","success":true,"data":{"level":"low"}}
    {"id":"q","type":"response","command":"set_model","success":true,"data":{}}
    {"id":"q","type":"response","command":"get_available_thinking_levels","success":true,"data":{"levels":["off"]}}
    {"id":"q","type":"response","command":"cycle_thinking_level","success":true,"data":null}
    {"id":"q","type":"response","command":"set_model","success":false,"error":"unknown model \"openai/gpt-9\"; did you mean: openai/gpt-4 (GPT-4), openai/gpt-5 (GPT-5), openai/gpt-4o (GPT-4o)"}
    {"type":"session_info_changed","name":"renamed"}
    {"id":"q","type":"response","command":"set_session_name","success":true,"data":{}}
    |}]
;;

let%expect_test "bash runs on the backend and lands in the transcript" =
  with_server []
  @@ fun h ->
  H.dump h;
  cmd h ~fields:{|, "command": "echo out; echo err >&2; exit 3"|} "bash";
  H.wait_for h "\"command\":\"bash\"";
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    {"type":"message_start","message":{"role":"user","content":"$ echo out; echo err >&2; exit 3\nout\nerr\n","timestamp":0}}
    {"type":"message_end","message":{"role":"user","content":"$ echo out; echo err >&2; exit 3\nout\nerr\n","timestamp":0}}
    {"id":"q","type":"response","command":"bash","success":true,"data":{"output":"out\nerr\n[exit code 3]","exitCode":1,"cancelled":false,"truncated":false}}
    |}]
;;

let%expect_test "fork picks the messages before a user entry; clone and cd" =
  with_server [ Reply.text "one"; Reply.text "two"; Reply.text "three" ]
  @@ fun h ->
  H.dump h;
  cmd h ~fields:{|, "message": "first"|} "prompt";
  H.wait_for h "agent_settled";
  cmd h ~fields:{|, "message": "second"|} "prompt";
  H.wait_for h "\"text\":\"two\"";
  H.wait_for h "agent_settled";
  Queue.clear h.output;
  cmd h "get_fork_messages";
  let _, data = last_response h in
  let entries = Json.list_exn (Json.member_exn "messages" data) in
  List.iter entries ~f:(fun e ->
    print_endline (Json.string_exn (Json.member_exn "text" e)));
  let second =
    Json.string_exn (Json.member_exn "entryId" (List.last_exn entries))
  in
  Queue.clear h.output;
  cmd h ~fields:(sprintf {|, "entryId": "%s"|} second) "fork";
  cmd h "get_messages";
  cmd h ~fields:{|, "entryId": "bogus"|} "fork";
  cmd h "clone";
  cmd h ~fields:{|, "cwd": "/nonexistent-dir"|} "change_cwd";
  cmd h ~fields:{|, "cwd": "/"|} "change_cwd";
  cmd h "get_state";
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    first
    second
    {"id":"q","type":"response","command":"fork","success":true,"data":{"text":"second","cancelled":false}}
    {"id":"q","type":"response","command":"get_messages","success":true,"data":{"messages":[{"role":"user","content":"first","timestamp":0},{"role":"assistant","content":[{"type":"text","text":"one"}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1}]}}
    {"id":"q","type":"response","command":"fork","success":false,"error":"no entry \"bogus\""}
    {"id":"q","type":"response","command":"clone","success":true,"data":{"cancelled":false}}
    {"id":"q","type":"response","command":"change_cwd","success":false,"error":"<host>: not a directory: /nonexistent-dir"}
    {"id":"q","type":"response","command":"change_cwd","success":true,"data":{"cancelled":false,"cwd":"/"}}
    {"id":"q","type":"response","command":"get_state","success":true,"data":{"model":{"id":"deepseek-flash","name":"DeepSeek V4.1 Flash","provider":"deepseek","reasoning":true,"contextWindow":1000000},"cwd":"/","thinkingLevel":"off","isStreaming":false,"isCompacting":false,"steeringMode":"all","followUpMode":"all","sessionFile":"$DIR/sessions/<stamp>_<id>.jsonl","sessionId":"<id>","autoCompactionEnabled":true,"messageCount":2,"pendingMessageCount":0}}
    |}]
;;

(* The id of the most recent dialog (select/input/confirm) the adapter opened. *)
let last_dialog h =
  Queue.to_list h.H.output
  |> List.filter_map ~f:(fun line ->
    let json = Json.of_string line in
    match Json.member "method" json with
    | Some (`String ("select" | "input" | "confirm")) ->
      Json.string (Json.member_exn "id" json)
    | _ -> None)
  |> List.last_exn
;;

let answer h ?(fields = {|"cancelled": true|}) id =
  H.send h (sprintf {|{"type":"extension_ui_response","id":"%s",%s}|} id fields);
  H.settle h
;;

let%expect_test "sessions: list, switch, /sessions dialog, /switch" =
  with_server [ Reply.text "hi" ]
  @@ fun h ->
  H.dump h;
  cmd h ~fields:{|, "message": "first"|} "prompt";
  H.wait_for h "agent_settled";
  Queue.clear h.output;
  cmd h "new_session";
  cmd h "list_sessions";
  let _, data = last_response h in
  let sessions = Json.list_exn (Json.member_exn "sessions" data) in
  let path =
    Json.string_exn (Json.member_exn "path" (List.last_exn sessions))
  in
  H.dump h;
  cmd h ~fields:(sprintf {|, "path": "%s"|} path) "switch_session";
  cmd h "get_messages";
  cmd h "new_session";
  cmd h "get_messages";
  cmd h ~fields:{|, "message": "/sessions"|} "prompt";
  let picker = last_dialog h in
  H.dump h;
  answer h ~fields:{|"value": "no such label"|} picker;
  cmd h ~fields:{|, "message": "/sessions"|} "prompt";
  let picker = last_dialog h in
  let label =
    "first · 2 msgs · "
    ^ Json.string_exn (Json.member_exn "id" (List.last_exn sessions))
  in
  answer h ~fields:(sprintf {|"value": "%s"|} label) picker;
  cmd h "get_messages";
  cmd h ~fields:{|, "message": "/switch nope"|} "prompt";
  cmd h ~fields:{|, "message": "/bogus"|} "prompt";
  H.dump h;
  [%expect
    {|
    {"type":"extension_ui_request","id":"status-host","method":"setStatus","statusKey":"host","statusText":null}
    {"type":"extension_ui_request","id":"status-branch","method":"setStatus","statusKey":"branch","statusText":null}
    {"id":"q","type":"response","command":"new_session","success":true,"data":{}}
    {"id":"q","type":"response","command":"list_sessions","success":true,"data":{"sessions":[{"id":"<id>","path":"$DIR/sessions/<stamp>_<id>.jsonl","name":null,"description":null,"cwd":"$DIR","created_at":"<time>","updated_at":"<time>","first_prompt":"first","message_count":2,"parent":null,"live":false}],"current":"<id>"}}
    {"type":"session_reloaded"}
    {"id":"q","type":"response","command":"switch_session","success":true,"data":{}}
    {"id":"q","type":"response","command":"get_messages","success":true,"data":{"messages":[{"role":"user","content":"first","timestamp":0},{"role":"assistant","content":[{"type":"text","text":"hi"}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1}]}}
    {"id":"q","type":"response","command":"new_session","success":true,"data":{}}
    {"id":"q","type":"response","command":"get_messages","success":true,"data":{"messages":[]}}
    {"type":"extension_ui_request","id":"dialog-13","method":"select","title":"Switch to session","options":["first · 2 msgs · <id>"]}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"extension_ui_request","id":"notify-1","method":"notify","message":"unknown choice no such label","notifyType":"error"}
    {"type":"extension_ui_request","id":"dialog-15","method":"select","title":"Switch to session","options":["first · 2 msgs · <id>"]}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"session_reloaded"}
    {"id":"q","type":"response","command":"get_messages","success":true,"data":{"messages":[{"role":"user","content":"first","timestamp":0},{"role":"assistant","content":[{"type":"text","text":"hi"}],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":1}]}}
    {"id":"q","type":"response","command":"prompt","success":false,"error":"no session \"nope\""}
    {"type":"agent_start"}
    {"type":"message_start","message":{"role":"user","content":"/bogus","timestamp":2}}
    {"type":"message_end","message":{"role":"user","content":"/bogus","timestamp":2}}
    {"type":"turn_start"}
    {"type":"message_start","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"stop","timestamp":3}}
    {"type":"message_end","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"error","errorMessage":"faux provider: no scripted reply","timestamp":3}}
    {"type":"turn_end","message":{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"error","errorMessage":"faux provider: no scripted reply","timestamp":3},"toolResults":[]}
    {"type":"agent_end","messages":[{"role":"user","content":"/bogus","timestamp":2},{"role":"assistant","content":[],"provider":"prigh","model":"deepseek-flash","stopReason":"error","errorMessage":"faux provider: no scripted reply","timestamp":3}],"willRetry":false}
    {"type":"agent_settled"}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    |}]
;;

let%expect_test "/help, /auth and /login dialogs; a secret prompt is an input" =
  with_server []
  @@ fun h ->
  Queue.clear h.output;
  cmd h ~fields:{|, "message": "/help"|} "prompt";
  cmd h ~fields:{|, "message": "/auth"|} "prompt";
  cmd h ~fields:{|, "message": "/login"|} "prompt";
  let picker = last_dialog h in
  H.dump h;
  answer h ~fields:{|"value": "DeepSeek API key"|} picker;
  let secret = last_dialog h in
  H.dump h;
  answer h ~fields:{|"value": "sk-test"|} secret;
  H.wait_for h "logged in";
  cmd h ~fields:{|, "message": "/auth"|} "prompt";
  cmd h ~fields:{|, "message": "/login anthropic api_key"|} "prompt";
  let secret = last_dialog h in
  H.dump h;
  answer h secret;
  H.wait_for h "cancelled";
  cmd h ~fields:{|, "message": "/logout deepseek"|} "prompt";
  cmd h ~fields:{|, "message": "/logout"|} "prompt";
  cmd h ~fields:{|, "message": "/login groq"|} "prompt";
  H.dump h;
  [%expect
    {|
    {"type":"message_end","message":{"role":"custom","customType":"help","content":"- `/compact [instructions]` — Compact the context\n- `/new` — Start a new session\n- `/name <name>` — Name the session\n- `/model [name]` — Pick a model\n- `/thinking [level]` — Set or cycle the thinking level\n- `/session` — Show session info\n- `/export [path]` — Export the session as markdown (on the backend)\n- `/copy` — Copy the last assistant message\n- `/fork` — Fork the session from an earlier message\n- `/clone` — Clone the session\n- `/cd [dir]` — Change the working directory\n- `/login [provider] [oauth|api_key]` — Log in to a provider\n- `/logout <provider>` — Log out of a provider\n- `/auth` — Show provider credentials\n- `/sessions` — Pick a saved session to switch to\n- `/switch <id|path>` — Switch to a session by id or path\n- `/host` — Pick where tools run\n- `/change_default` — Save the current model and thinking level as the default\n- `/help` — List the commands","display":true,"timestamp":0}}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"message_end","message":{"role":"custom","customType":"auth","content":"Providers:\n- **Anthropic** `anthropic`: not logged in\n- **OpenAI** `openai`: not logged in\n- **OpenAI Codex (ChatGPT)** `openai-codex`: not logged in\n- **DeepSeek** `deepseek`: not logged in","display":true,"timestamp":1}}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"extension_ui_request","id":"dialog-4","method":"select","title":"Log in to","options":["Anthropic (Claude Pro/Max)","Anthropic API key","OpenAI API key","OpenAI (ChatGPT Plus/Pro)","DeepSeek API key"]}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"extension_ui_request","id":"auth-p1","method":"input","title":"Enter DeepSeek API key","placeholder":""}
    {"type":"extension_ui_request","id":"notify-2","method":"notify","message":"logged in to deepseek (api_key)","notifyType":"info"}
    {"type":"message_end","message":{"role":"custom","customType":"auth","content":"Providers:\n- **Anthropic** `anthropic`: not logged in\n- **OpenAI** `openai`: not logged in\n- **OpenAI Codex (ChatGPT)** `openai-codex`: not logged in\n- **DeepSeek** `deepseek`: api_key (stored api key)","display":true,"timestamp":2}}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"extension_ui_request","id":"auth-p2","method":"input","title":"Enter Anthropic API key","placeholder":""}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"extension_ui_request","id":"notify-3","method":"notify","message":"login to anthropic failed: login cancelled","notifyType":"error"}
    {"type":"extension_ui_request","id":"notify-4","method":"notify","message":"logged out of deepseek","notifyType":"info"}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"id":"q","type":"response","command":"prompt","success":false,"error":"usage: /logout <provider>"}
    {"id":"q","type":"response","command":"prompt","success":false,"error":"unknown provider \"groq\" (one of: anthropic, openai, openai-codex, deepseek)"}
    |}]
;;

let%expect_test "a bad token fails hello" =
  with_server ~token:"sekrit" ~hello_token:"wrong" []
  @@ fun h ->
  cmd h "get_state";
  H.dump h;
  [%expect
    {| {"type":"prigh_hello_failed","error":"unauthorised: bad or missing token"} |}]
;;

let%expect_test "the token and session from the query string are used" =
  with_server ~token:"sekrit" ~hello_token:"sekrit" ~session:"nope" []
  @@ fun h ->
  H.dump h;
  [%expect {| {"type":"prigh_hello_failed","error":"no session \"nope\""} |}]
;;

let%expect_test "subagents feed the agents rail widget; the parent sees a tool" =
  with_server
    [ Reply.tool_call
        ~text:"Delegating."
        ~id:"p1"
        ~name:"subagent"
        ~arguments:{|{"task":"look around\nin detail"}|}
        ()
    ; Reply.tool_call ~id:"c1" ~name:"ls" ~arguments:"{}" ()
    ; Reply.text "child report"
    ; Reply.text "main report"
    ]
  @@ fun h ->
  Queue.clear h.output;
  cmd h ~fields:{|, "message": "go"|} "prompt";
  H.wait_for h "agent_settled";
  Queue.filter_inplace h.output ~f:(fun line ->
    String.is_substring line ~substring:"widget"
    || String.is_substring line ~substring:"tool_execution");
  H.dump h;
  [%expect
    {|
    {"type":"tool_execution_start","toolCallId":"p1","toolName":"subagent","args":{"task":"look around\nin detail"}}
    {"type":"extension_ui_request","id":"widget-subagents","method":"setWidget","widgetKey":"subagents","widgetLines":["PI_SUBAGENT_ASYNC_JSON:{\"generatedAt\":3000,\"omitted\":{\"runs\":0,\"children\":0,\"byteLimitExceeded\":false},\"runs\":[{\"id\":\"a1\",\"kind\":\"subagent\",\"label\":\"look around\",\"state\":\"running\",\"startedAt\":2000,\"updatedAt\":2000,\"activity\":{\"turnCount\":0,\"toolCount\":0}}]}"]}
    {"type":"extension_ui_request","id":"widget-subagents","method":"setWidget","widgetKey":"subagents","widgetLines":["PI_SUBAGENT_ASYNC_JSON:{\"generatedAt\":8000,\"omitted\":{\"runs\":0,\"children\":0,\"byteLimitExceeded\":false},\"runs\":[{\"id\":\"a1\",\"kind\":\"subagent\",\"label\":\"look around\",\"state\":\"running\",\"startedAt\":2000,\"updatedAt\":7000,\"activity\":{\"turnCount\":1,\"toolCount\":0}}]}"]}
    {"type":"tool_execution_end","toolCallId":"p1","toolName":"subagent","args":{"task":"look around\nin detail"},"result":{"content":[{"type":"text","text":"started agent a1 (look around); its result will be delivered to you when it finishes; use subagent_wait to block on it"}]},"isError":false}
    {"type":"extension_ui_request","id":"widget-subagents","method":"setWidget","widgetKey":"subagents","widgetLines":["PI_SUBAGENT_ASYNC_JSON:{\"generatedAt\":14000,\"omitted\":{\"runs\":0,\"children\":0,\"byteLimitExceeded\":false},\"runs\":[{\"id\":\"a1\",\"kind\":\"subagent\",\"label\":\"look around\",\"state\":\"running\",\"startedAt\":2000,\"updatedAt\":13000,\"activity\":{\"turnCount\":1,\"toolCount\":1,\"currentTool\":\"ls\",\"currentToolStartedAt\":13000}}]}"]}
    {"type":"extension_ui_request","id":"widget-subagents","method":"setWidget","widgetKey":"subagents","widgetLines":["PI_SUBAGENT_ASYNC_JSON:{\"generatedAt\":16000,\"omitted\":{\"runs\":0,\"children\":0,\"byteLimitExceeded\":false},\"runs\":[{\"id\":\"a1\",\"kind\":\"subagent\",\"label\":\"look around\",\"state\":\"running\",\"startedAt\":2000,\"updatedAt\":15000,\"activity\":{\"turnCount\":1,\"toolCount\":1}}]}"]}
    {"type":"extension_ui_request","id":"widget-subagents","method":"setWidget","widgetKey":"subagents","widgetLines":["PI_SUBAGENT_ASYNC_JSON:{\"generatedAt\":21000,\"omitted\":{\"runs\":0,\"children\":0,\"byteLimitExceeded\":false},\"runs\":[{\"id\":\"a1\",\"kind\":\"subagent\",\"label\":\"look around\",\"state\":\"running\",\"startedAt\":2000,\"updatedAt\":20000,\"activity\":{\"turnCount\":2,\"toolCount\":1}}]}"]}
    {"type":"extension_ui_request","id":"widget-subagents","method":"setWidget","widgetKey":"subagents","widgetLines":["PI_SUBAGENT_ASYNC_JSON:{\"generatedAt\":28000,\"omitted\":{\"runs\":0,\"children\":0,\"byteLimitExceeded\":false},\"runs\":[{\"id\":\"a1\",\"kind\":\"subagent\",\"label\":\"look around\",\"state\":\"complete\",\"startedAt\":2000,\"updatedAt\":27000,\"endedAt\":27000,\"activity\":{\"turnCount\":2,\"toolCount\":1}}]}"]}
    {"type":"extension_ui_request","id":"widget-subagents","method":"setWidget","widgetKey":"subagents","widgetLines":null}
    |}]
;;

let%expect_test "compaction, export, /host" =
  let long = String.make 40_000 'x' in
  with_server
    [ Reply.text long
    ; Reply.text "recent reply"
    ; Reply.text "SUMMARY OF OLD STUFF"
    ]
  @@ fun h ->
  Queue.clear h.output;
  cmd h ~fields:{|, "message": "old question"|} "prompt";
  H.wait_for h "agent_settled";
  cmd h ~fields:{|, "message": "recent question"|} "prompt";
  H.wait_for h "\"text\":\"recent reply\"";
  H.wait_for h "agent_settled";
  Queue.clear h.output;
  cmd h "compact";
  H.wait_for h "compaction_end";
  cmd h "export_html";
  cmd h ~fields:{|, "outputPath": "out.md"|} "export_html";
  cmd h ~fields:{|, "message": "/host"|} "prompt";
  let picker = last_dialog h in
  H.dump h;
  answer h ~fields:{|"value": "nowhere"|} picker;
  H.dump h;
  [%expect
    {|
    {"type":"compaction_end","reason":"manual","result":{"summary":"SUMMARY OF OLD STUFF","tokensBefore":10},"aborted":false,"willRetry":false}
    {"id":"q","type":"response","command":"compact","success":true,"data":{"summary":"SUMMARY OF OLD STUFF"}}
    {"id":"q","type":"response","command":"export_html","success":true,"data":{"path":"$DIR/sessions/exports/<stamp>_<id>.md"}}
    {"id":"q","type":"response","command":"export_html","success":true,"data":{"path":"$DIR/out.md"}}
    {"type":"extension_ui_request","id":"dialog-8","method":"select","title":"Run tools on","options":["<host> ($DIR) · active"]}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {"type":"extension_ui_request","id":"notify-5","method":"notify","message":"unknown choice nowhere","notifyType":"error"}
    |}]
;;

let%expect_test "/change_default saves the model and thinking level" =
  with_server []
  @@ fun h ->
  cmd
    h
    ~fields:{|, "provider": "deepseek", "modelId": "deepseek-v4-pro"|}
    "set_model";
  cmd h ~fields:{|, "level": "high"|} "set_thinking_level";
  Queue.clear h.output;
  cmd h ~fields:{|, "message": "/change_default"|} "prompt";
  H.dump h;
  print_endline (read h.sandbox ".prigh/config.json");
  [%expect
    {|
    {"type":"extension_ui_request","id":"notify-6","method":"notify","message":"default: deepseek/deepseek-v4-pro, thinking high","notifyType":"info"}
    {"id":"q","type":"response","command":"prompt","success":true,"data":{}}
    {
      "scoped_models": [],
      "confirm_tools": false,
      "default_model": "deepseek/deepseek-v4-pro",
      "default_thinking": "high"
    }
    |}]
;;
