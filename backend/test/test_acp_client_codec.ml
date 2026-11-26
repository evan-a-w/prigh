open! Core
open! Prigh
module C = Acp_client_codec

(* Prints what the codec sends ([>]), and for each line the agent writes
   ([<]), what it makes of it. *)

let print_output (o : C.Output.t) =
  match o with
  | Send line -> print_endline ("  > " ^ line)
  | Event e -> print_s [%sexp (e : Adapter_event.t)]
  | Response r -> print_s [%sexp Response (r : C.Response.t)]
  | Config_changed c -> print_s [%sexp Config_changed (c : C.Session_config.t)]
  | Commands c -> print_s [%sexp Commands (c : C.Command.t list)]
  | Title t -> print_s [%sexp Title (t : string option)]
;;

let send line = print_endline ("> " ^ line)

let recv t line =
  print_endline ("< " ^ line);
  List.iter (C.handle_line t line) ~f:print_output
;;

let ok = function
  | Ok outputs -> List.iter outputs ~f:print_output
  | Error e -> print_s [%sexp Error (e : Error.t)]
;;

let ok_line = function
  | Ok line -> send line
  | Error e -> print_s [%sexp Error (e : Error.t)]
;;

let init_response =
  {|{"jsonrpc":"2.0","id":0,"result":{"protocolVersion":1,"agentCapabilities":{"loadSession":true,"promptCapabilities":{"image":true,"embeddedContext":true},"mcpCapabilities":{"http":true},"sessionCapabilities":{"resume":{}}},"agentInfo":{"name":"opencode","version":"1.2.3"},"authMethods":[{"id":"login","name":"Log in","type":"terminal","description":"Run opencode auth login"}]}}|}
;;

let new_session_response =
  {|{"jsonrpc":"2.0","id":1,"result":{"sessionId":"ses_1","modes":{"currentModeId":"build","availableModes":[{"id":"build","name":"Build"},{"id":"plan","name":"Plan","description":"Read only"}]},"configOptions":[{"id":"model","name":"Model","category":"model","type":"select","currentValue":"sonnet","options":[{"value":"sonnet","name":"Sonnet"},{"name":"OpenAI","options":[{"value":"gpt-5","name":"GPT-5"}]}]},{"id":"fast","name":"Fast","type":"boolean","currentValue":false}]}}|}
;;

let started ?quirks () =
  let t = C.create ?quirks () in
  send (C.initialize t ~client_name:"prigh" ~client_version:"0.1");
  recv t init_response;
  send (C.new_session t ~cwd:"/work" ~mcp_servers:[]);
  recv t new_session_response;
  t
;;

let%expect_test "initialize and session/new" =
  let t = C.create () in
  send (C.initialize t ~client_name:"prigh" ~client_version:"0.1");
  recv t init_response;
  send
    (C.new_session
       t
       ~cwd:"/work"
       ~mcp_servers:
         [ Stdio
             { name = "prigh"
             ; command = "prigh"
             ; args = [ "mcp" ]
             ; env = [ "A", "1" ]
             }
         ; Http
             { name = "web"
             ; url = "http://h/mcp"
             ; headers = [ "Authorization", "x" ]
             }
         ]);
  recv t new_session_response;
  print_s [%sexp (C.session_id t : string option)];
  print_s
    [%sexp
      (C.Session_config.option (C.session_config t) ~category:"model"
       : C.Config_option.t option)];
  [%expect
    {|
    > {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":1,"clientCapabilities":{"fs":{"readTextFile":false,"writeTextFile":false},"terminal":false,"session":{"notices":{},"compaction":{}}},"clientInfo":{"name":"prigh","version":"0.1"}}}
    < {"jsonrpc":"2.0","id":0,"result":{"protocolVersion":1,"agentCapabilities":{"loadSession":true,"promptCapabilities":{"image":true,"embeddedContext":true},"mcpCapabilities":{"http":true},"sessionCapabilities":{"resume":{}}},"agentInfo":{"name":"opencode","version":"1.2.3"},"authMethods":[{"id":"login","name":"Log in","type":"terminal","description":"Run opencode auth login"}]}}
    (Response
     (Initialized (protocol_version 1)
      (agent (((name opencode) (title ()) (version (1.2.3)))))
      (capabilities
       ((load_session true) (resume_session true) (close_session false)
        (images true) (audio false) (embedded_context true) (mcp_http true)
        (mcp_sse false)))
      (auth_methods
       (((id login) (name "Log in") (description ("Run opencode auth login"))
         (terminal true))))))
    > {"jsonrpc":"2.0","id":1,"method":"session/new","params":{"cwd":"/work","mcpServers":[{"name":"prigh","command":"prigh","args":["mcp"],"env":[{"name":"A","value":"1"}]},{"type":"http","name":"web","url":"http://h/mcp","headers":[{"name":"Authorization","value":"x"}]}]}}
    < {"jsonrpc":"2.0","id":1,"result":{"sessionId":"ses_1","modes":{"currentModeId":"build","availableModes":[{"id":"build","name":"Build"},{"id":"plan","name":"Plan","description":"Read only"}]},"configOptions":[{"id":"model","name":"Model","category":"model","type":"select","currentValue":"sonnet","options":[{"value":"sonnet","name":"Sonnet"},{"name":"OpenAI","options":[{"value":"gpt-5","name":"GPT-5"}]}]},{"id":"fast","name":"Fast","type":"boolean","currentValue":false}]}}
    (Response
     (Session_opened (kind New_session) (session_id ses_1)
      (config
       ((current_mode (build))
        (modes
         (((id build) (name Build) (description ()))
          ((id plan) (name Plan) (description ("Read only")))))
        (config_options
         (((id model) (name Model) (description ()) (category (model))
           (value
            (Select (current sonnet)
             (choices
              (((value sonnet) (name Sonnet) (description ()) (group ()))
               ((value gpt-5) (name GPT-5) (description ()) (group (OpenAI))))))))
          ((id fast) (name Fast) (description ()) (category ())
           (value (Boolean false)))))))))
    (ses_1)
    (((id model) (name Model) (description ()) (category (model))
      (value
       (Select (current sonnet)
        (choices
         (((value sonnet) (name Sonnet) (description ()) (group ()))
          ((value gpt-5) (name GPT-5) (description ()) (group (OpenAI)))))))))
    |}]
;;

let%expect_test "a turn: text, thoughts, a command, a diff, a plan, usage" =
  let t = started () in
  ignore ([%expect.output] : string);
  ok (C.prompt t [ Text "fix it" ]);
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"Let me "}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"look."}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Running tests."}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call","toolCallId":"call_1","title":"make test","kind":"execute","status":"pending","rawInput":{"cmd":"make test"}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call_update","toolCallId":"call_1","status":"in_progress","content":[{"type":"content","content":{"type":"text","text":"ok 1"}}]}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call_update","toolCallId":"call_1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"ok 1\nok 2"}}]}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call","toolCallId":"call_2","title":"Edit a.ml","kind":"edit","status":"completed","content":[{"type":"diff","path":"/work/a.ml","oldText":"let x = 1\n","newText":"let x = 2\n"},{"type":"diff","path":"/work/b.ml","newText":"new\n"}]}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"plan","entries":[{"content":"Fix","priority":"high","status":"completed"},{"content":"Test","priority":"low","status":"in_progress"}]}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","messageId":"m2","content":{"type":"text","text":"Done."}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"usage_update","used":5000,"size":200000,"cost":{"amount":0.25,"currency":"USD"}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"usage_update","used":6000,"size":200000,"cost":{"amount":0.40,"currency":"USD"}}}}|};
  recv t {|{"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn"}}|};
  print_s [%sexp (C.in_turn t : bool)];
  [%expect
    {|
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"fix it"}]}}
    (Turn_started (turn_id turn-1))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"Let me "}}}}
    (Thinking_delta (item turn-1-t1) (text "Let me "))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"look."}}}}
    (Thinking_delta (item turn-1-t1) (text look.))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Running tests."}}}}
    (Thinking_done (item turn-1-t1) (text "Let me look."))
    (Text_delta (item turn-1-m2) (text "Running tests."))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call","toolCallId":"call_1","title":"make test","kind":"execute","status":"pending","rawInput":{"cmd":"make test"}}}}
    (Text_done (item turn-1-m2) (text "Running tests."))
    (Tool_start (id call_1) (kind Command) (native_name execute)
     (title "make test")
     (input (Object ((cmd (String "make test")) (command (String "make test")))))
     (parent ()))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call_update","toolCallId":"call_1","status":"in_progress","content":[{"type":"content","content":{"type":"text","text":"ok 1"}}]}}}
    (Tool_update (id call_1) (output "ok 1"))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call_update","toolCallId":"call_1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"ok 1\nok 2"}}]}}}
    (Tool_update (id call_1) (output  "\
                                     \nok 2"))
    (Tool_end (id call_1) (ok true) (output  "ok 1\
                                            \nok 2")
     (changes ()) (images ()))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call","toolCallId":"call_2","title":"Edit a.ml","kind":"edit","status":"completed","content":[{"type":"diff","path":"/work/a.ml","oldText":"let x = 1\n","newText":"let x = 2\n"},{"type":"diff","path":"/work/b.ml","newText":"new\n"}]}}}
    (Tool_start (id call_2) (kind File_change) (native_name edit)
     (title "Edit a.ml") (input (Object ())) (parent ()))
    (Tool_end (id call_2) (ok true) (output "")
     (changes
      (((path /work/a.ml) (kind (Update (move_to ())))
        (diff
          "diff --git a/work/a.ml b/work/a.ml\
         \n--- a/work/a.ml\
         \n+++ b/work/a.ml\
         \n@@ -1,1 +1,1 @@\
         \n-let x = 1\
         \n+let x = 2\
         \n"))
       ((path /work/b.ml) (kind Add)
        (diff
          "diff --git a/work/b.ml b/work/b.ml\
         \n--- a/work/b.ml\
         \n+++ b/work/b.ml\
         \n@@ -0,0 +1,1 @@\
         \n+new\
         \n"))))
     (images ()))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"plan","entries":[{"content":"Fix","priority":"high","status":"completed"},{"content":"Test","priority":"low","status":"in_progress"}]}}}
    (Plan
     ((explanation ())
      (steps
       (((step Fix) (status Completed)) ((step Test) (status In_progress))))))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","messageId":"m2","content":{"type":"text","text":"Done."}}}}
    (Text_delta (item m2) (text Done.))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"usage_update","used":5000,"size":200000,"cost":{"amount":0.25,"currency":"USD"}}}}
    (Usage
     ((usage ((input 0) (output 0) (cache_read 0))) (cost_usd (0.25))
      (context_tokens (5000)) (context_window (200000))))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"usage_update","used":6000,"size":200000,"cost":{"amount":0.40,"currency":"USD"}}}}
    (Usage
     ((usage ((input 0) (output 0) (cache_read 0)))
      (cost_usd (0.15000000000000002)) (context_tokens (6000))
      (context_window (200000))))
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn"}}
    (Text_done (item m2) (text Done.))
    (Turn_end (turn_id (turn-1)) (stop Done))
    false
    |}]
;;

let%expect_test "permission requests pick the agent's options" =
  let t = started () in
  ok (C.prompt t [ Text "go" ]);
  let permission id options =
    sprintf
      {|{"jsonrpc":"2.0","id":%s,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c%s","title":"rm -rf build","kind":"execute","rawInput":{"command":"rm -rf build"}},"options":%s}}|}
      id
      (String.filter id ~f:Char.is_digit)
      options
  in
  let all =
    {|[{"optionId":"yes","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always","kind":"allow_always"},{"optionId":"no","name":"Reject","kind":"reject_once"}]|}
  in
  ignore ([%expect.output] : string);
  recv t (permission "10" all);
  print_s
    [%sexp (C.reply_approval t ~request_id:"10" Allow_always : string option)];
  recv t (permission "11" all);
  print_s
    [%sexp
      (C.reply_approval t ~request_id:"11" (Deny (Some "no")) : string option)];
  recv
    t
    (permission
       "\"12\""
       {|[{"optionId":"once","name":"OK","kind":"allow_once"}]|});
  print_s
    [%sexp (C.reply_approval t ~request_id:"12" Allow_always : string option)];
  print_s [%sexp (C.reply_approval t ~request_id:"12" Allow : string option)];
  recv
    t
    (permission "13" {|[{"optionId":"once","name":"OK","kind":"allow_once"}]|});
  print_s
    [%sexp (C.reply_approval t ~request_id:"13" (Deny None) : string option)];
  [%expect
    {|
    < {"jsonrpc":"2.0","id":10,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c10","title":"rm -rf build","kind":"execute","rawInput":{"command":"rm -rf build"}},"options":[{"optionId":"yes","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always","kind":"allow_always"},{"optionId":"no","name":"Reject","kind":"reject_once"}]}}
    (Tool_start (id c10) (kind Command) (native_name execute)
     (title "rm -rf build") (input (Object ((command (String "rm -rf build")))))
     (parent ()))
    (Approval_request (request_id 10) (tool_id (c10)) (kind Command)
     (summary "rm -rf build")
     (input (Object ((command (String "rm -rf build"))))) (offers_always true))
    ("{\"jsonrpc\":\"2.0\",\"id\":10,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"always\"}}}")
    < {"jsonrpc":"2.0","id":11,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c11","title":"rm -rf build","kind":"execute","rawInput":{"command":"rm -rf build"}},"options":[{"optionId":"yes","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always","kind":"allow_always"},{"optionId":"no","name":"Reject","kind":"reject_once"}]}}
    (Tool_start (id c11) (kind Command) (native_name execute)
     (title "rm -rf build") (input (Object ((command (String "rm -rf build")))))
     (parent ()))
    (Approval_request (request_id 11) (tool_id (c11)) (kind Command)
     (summary "rm -rf build")
     (input (Object ((command (String "rm -rf build"))))) (offers_always true))
    ("{\"jsonrpc\":\"2.0\",\"id\":11,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"no\"}}}")
    < {"jsonrpc":"2.0","id":"12","method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c12","title":"rm -rf build","kind":"execute","rawInput":{"command":"rm -rf build"}},"options":[{"optionId":"once","name":"OK","kind":"allow_once"}]}}
    (Tool_start (id c12) (kind Command) (native_name execute)
     (title "rm -rf build") (input (Object ((command (String "rm -rf build")))))
     (parent ()))
    (Approval_request (request_id 12) (tool_id (c12)) (kind Command)
     (summary "rm -rf build")
     (input (Object ((command (String "rm -rf build"))))) (offers_always false))
    ("{\"jsonrpc\":\"2.0\",\"id\":\"12\",\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"once\"}}}")
    ()
    < {"jsonrpc":"2.0","id":13,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c13","title":"rm -rf build","kind":"execute","rawInput":{"command":"rm -rf build"}},"options":[{"optionId":"once","name":"OK","kind":"allow_once"}]}}
    (Tool_start (id c13) (kind Command) (native_name execute)
     (title "rm -rf build") (input (Object ((command (String "rm -rf build")))))
     (parent ()))
    (Approval_request (request_id 13) (tool_id (c13)) (kind Command)
     (summary "rm -rf build")
     (input (Object ((command (String "rm -rf build"))))) (offers_always false))
    ("{\"jsonrpc\":\"2.0\",\"id\":13,\"result\":{\"outcome\":{\"outcome\":\"cancelled\"}}}")
    |}]
;;

let%expect_test "permissions: nothing persistent unless asked; after cancel" =
  let t = started () in
  ok (C.prompt t [ Text "go" ]);
  let permission id options =
    sprintf
      {|{"jsonrpc":"2.0","id":%d,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c%d","title":"rm -rf build","kind":"execute"},"options":%s}}|}
      id
      id
      options
  in
  let always_only =
    {|[{"optionId":"aa","name":"Always","kind":"allow_always"},{"optionId":"ra","name":"Never","kind":"reject_always"}]|}
  in
  let quietly line = ignore (C.handle_line t line : C.Output.t list) in
  ignore ([%expect.output] : string);
  quietly (permission 20 always_only);
  (* Approving once, or denying once, picks no "always" option. *)
  print_s [%sexp (C.reply_approval t ~request_id:"20" Allow : string option)];
  quietly (permission 21 always_only);
  print_s
    [%sexp (C.reply_approval t ~request_id:"21" (Deny None) : string option)];
  quietly (permission 22 always_only);
  print_s
    [%sexp (C.reply_approval t ~request_id:"22" Allow_always : string option)];
  [%expect
    {|
    ("{\"jsonrpc\":\"2.0\",\"id\":20,\"result\":{\"outcome\":{\"outcome\":\"cancelled\"}}}")
    ("{\"jsonrpc\":\"2.0\",\"id\":21,\"result\":{\"outcome\":{\"outcome\":\"cancelled\"}}}")
    ("{\"jsonrpc\":\"2.0\",\"id\":22,\"result\":{\"outcome\":{\"outcome\":\"selected\",\"optionId\":\"aa\"}}}")
    |}];
  (* After session/cancel a new request is answered cancelled at once. *)
  List.iter (C.cancel t) ~f:print_output;
  recv t (permission 23 always_only);
  [%expect
    {|
      > {"jsonrpc":"2.0","method":"session/cancel","params":{"sessionId":"ses_1"}}
    < {"jsonrpc":"2.0","id":23,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c23","title":"rm -rf build","kind":"execute"},"options":[{"optionId":"aa","name":"Always","kind":"allow_always"},{"optionId":"ra","name":"Never","kind":"reject_always"}]}}
      > {"jsonrpc":"2.0","id":23,"result":{"outcome":{"outcome":"cancelled"}}}
    |}]
;;

let%expect_test
    "the codec reports the agent's ACP version (Acp_adapter refuses others)"
  =
  let t = C.create () in
  send (C.initialize t ~client_name:"prigh" ~client_version:"0.1");
  recv
    t
    {|{"jsonrpc":"2.0","id":0,"result":{"protocolVersion":2,"agentCapabilities":{}}}|};
  [%expect
    {|
    > {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":1,"clientCapabilities":{"fs":{"readTextFile":false,"writeTextFile":false},"terminal":false,"session":{"notices":{},"compaction":{}}},"clientInfo":{"name":"prigh","version":"0.1"}}}
    < {"jsonrpc":"2.0","id":0,"result":{"protocolVersion":2,"agentCapabilities":{}}}
    (Response
     (Initialized (protocol_version 2) (agent ())
      (capabilities
       ((load_session false) (resume_session false) (close_session false)
        (images false) (audio false) (embedded_context false) (mcp_http false)
        (mcp_sse false)))
      (auth_methods ())))
    |}]
;;

let%expect_test "a loaded session's running cost counts from its replay" =
  let t = C.create ~cost_total:0.5 () in
  send (C.initialize t ~client_name:"prigh" ~client_version:"0.1");
  recv t init_response;
  send (C.load_session t ~session_id:"ses_old" ~cwd:"/w" ~mcp_servers:[]);
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"usage_update","used":100,"size":1000,"cost":{"amount":0.6,"currency":"USD"}}}}|};
  recv t {|{"jsonrpc":"2.0","id":1,"result":{"modes":null}}|};
  ok (C.prompt t [ Text "hi" ]);
  ignore ([%expect.output] : string);
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"usage_update","used":200,"size":1000,"cost":{"amount":0.75,"currency":"USD"}}}}|};
  print_s [%sexp (C.cost_total t : float option)];
  [%expect
    {|
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"usage_update","used":200,"size":1000,"cost":{"amount":0.75,"currency":"USD"}}}}
    (Usage
     ((usage ((input 0) (output 0) (cache_read 0)))
      (cost_usd (0.15000000000000002)) (context_tokens (200))
      (context_window (1000))))
    (0.75)
    |}]
;;

let%expect_test
    "cancel answers waiting permissions and ends the turn on the response"
  =
  let t = started () in
  ok (C.prompt t [ Text "go" ]);
  ignore ([%expect.output] : string);
  recv
    t
    {|{"jsonrpc":"2.0","id":7,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c1","title":"Edit a.ml","kind":"edit"},"options":[{"optionId":"y","name":"Yes","kind":"allow_once"}]}}|};
  List.iter (C.cancel t) ~f:print_output;
  print_s [%sexp (C.cancel t : C.Output.t list)];
  print_s [%sexp (C.reply_approval t ~request_id:"7" Allow : string option)];
  recv t {|{"jsonrpc":"2.0","id":2,"result":{"stopReason":"cancelled"}}|};
  [%expect
    {|
    < {"jsonrpc":"2.0","id":7,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c1","title":"Edit a.ml","kind":"edit"},"options":[{"optionId":"y","name":"Yes","kind":"allow_once"}]}}
    (Tool_start (id c1) (kind File_change) (native_name edit) (title "Edit a.ml")
     (input (Object ())) (parent ()))
    (Approval_request (request_id 7) (tool_id (c1)) (kind File_change)
     (summary "Edit a.ml") (input (Object ())) (offers_always false))
      > {"jsonrpc":"2.0","method":"session/cancel","params":{"sessionId":"ses_1"}}
      > {"jsonrpc":"2.0","id":7,"result":{"outcome":{"outcome":"cancelled"}}}
    (Request_withdrawn (request_id 7))
    ()
    ()
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"cancelled"}}
    (Tool_end (id c1) (ok false) (output "") (changes ()) (images ()))
    (Turn_end (turn_id (turn-1)) (stop Interrupted))
    |}]
;;

let%expect_test "stop reasons and errors" =
  let t = started () in
  ignore ([%expect.output] : string);
  let turn id response =
    (match C.prompt t [ Text "x" ] with
     | Ok _ -> ()
     | Error e -> print_s [%sexp (e : Error.t)]);
    recv t (sprintf {|{"jsonrpc":"2.0","id":%d,%s}|} id response)
  in
  turn 2 {|"result":{"stopReason":"max_tokens"}|};
  turn 3 {|"result":{"stopReason":"refusal"}|};
  turn 4 {|"error":{"code":-32000,"message":"Authentication required"}|};
  turn 5 {|"error":{"code":-32603,"message":"boom"}|};
  [%expect
    {|
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"max_tokens"}}
    (Turn_end (turn_id (turn-1))
     (stop
      (Failed (kind Other)
       (message "the agent reached its maximum number of tokens"))))
    < {"jsonrpc":"2.0","id":3,"result":{"stopReason":"refusal"}}
    (Turn_end (turn_id (turn-2))
     (stop (Failed (kind Other) (message "the agent refused to continue"))))
    < {"jsonrpc":"2.0","id":4,"error":{"code":-32000,"message":"Authentication required"}}
    (Turn_end (turn_id (turn-3))
     (stop
      (Failed (kind (Auth_required (hint "Authentication required")))
       (message "Authentication required"))))
    < {"jsonrpc":"2.0","id":5,"error":{"code":-32603,"message":"boom"}}
    (Turn_end (turn_id (turn-4)) (stop (Failed (kind Other) (message boom))))
    |}]
;;

let%expect_test "prompt errors, images, set_mode and set_config_option" =
  let t = C.create () in
  ok (C.prompt t [ Text "x" ]);
  send (C.initialize t ~client_name:"prigh" ~client_version:"0.1");
  recv
    t
    {|{"jsonrpc":"2.0","id":0,"result":{"protocolVersion":1,"agentCapabilities":{}}}|};
  send (C.new_session t ~cwd:"/w" ~mcp_servers:[]);
  recv
    t
    (String.substr_replace_first
       new_session_response
       ~pattern:"\"id\":1"
       ~with_:"\"id\":1");
  ignore ([%expect.output] : string);
  ok (C.prompt t [ Image { mime_type = "image/png"; data = "AAAA" } ]);
  ok_line (C.set_mode t ~mode_id:"yolo");
  ok_line (C.set_mode t ~mode_id:"plan");
  recv t {|{"jsonrpc":"2.0","id":2,"result":{}}|};
  ok_line (C.set_config_option t ~config_id:"model" (`Id "gpt-6"));
  ok_line (C.set_config_option t ~config_id:"model" (`Bool true));
  ok_line (C.set_config_option t ~config_id:"fast" (`Bool true));
  recv
    t
    {|{"jsonrpc":"2.0","id":3,"result":{"configOptions":[{"id":"fast","name":"Fast","type":"boolean","currentValue":true}]}}|};
  ok_line (C.set_config_option t ~config_id:"nope" (`Id "x"));
  ok (C.prompt t [ Text "a" ]);
  ok (C.prompt t [ Text "b" ]);
  [%expect
    {|
    (Error "this agent does not take images")
    (Error ("the agent offers no such mode" yolo (modes (build plan))))
    > {"jsonrpc":"2.0","id":2,"method":"session/set_mode","params":{"sessionId":"ses_1","modeId":"plan"}}
    < {"jsonrpc":"2.0","id":2,"result":{}}
    (Response (Mode_set plan))
    (Config_changed
     ((current_mode (plan))
      (modes
       (((id build) (name Build) (description ()))
        ((id plan) (name Plan) (description ("Read only")))))
      (config_options
       (((id model) (name Model) (description ()) (category (model))
         (value
          (Select (current sonnet)
           (choices
            (((value sonnet) (name Sonnet) (description ()) (group ()))
             ((value gpt-5) (name GPT-5) (description ()) (group (OpenAI))))))))
        ((id fast) (name Fast) (description ()) (category ())
         (value (Boolean false)))))))
    (Error
     ("not one of the option's choices" model gpt-6 (choices (sonnet gpt-5))))
    (Error ("the option takes one of its choices" model))
    > {"jsonrpc":"2.0","id":3,"method":"session/set_config_option","params":{"sessionId":"ses_1","configId":"fast","type":"boolean","value":true}}
    < {"jsonrpc":"2.0","id":3,"result":{"configOptions":[{"id":"fast","name":"Fast","type":"boolean","currentValue":true}]}}
    (Response
     (Config_set
      (((id fast) (name Fast) (description ()) (category ())
        (value (Boolean true))))))
    (Config_changed
     ((current_mode (plan))
      (modes
       (((id build) (name Build) (description ()))
        ((id plan) (name Plan) (description ("Read only")))))
      (config_options
       (((id fast) (name Fast) (description ()) (category ())
         (value (Boolean true)))))))
    (Error ("the agent offers no such option" nope))
      > {"jsonrpc":"2.0","id":4,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"a"}]}}
    (Turn_started (turn_id turn-1))
    (Error ("a turn is already running" (turn turn-1)))
    |}]
;;

let%expect_test "session/load drops the replay; other sessions and odd lines" =
  let t = C.create () in
  send (C.initialize t ~client_name:"prigh" ~client_version:"0.1");
  recv t init_response;
  send (C.load_session t ~session_id:"ses_old" ~cwd:"/w" ~mcp_servers:[]);
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"user_message_chunk","content":{"type":"text","text":"old"}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"old reply"}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"available_commands_update","availableCommands":[{"name":"review","description":"Review","input":{"hint":"what"}}]}}}|};
  recv t {|{"jsonrpc":"2.0","id":1,"result":{"modes":null}}|};
  ok (C.prompt t [ Text "hi" ]);
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"other","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"not ours"}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"current_mode_update","currentModeId":"plan"}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"session_info_update","title":"Fix the build"}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"notice","severity":"warning","title":"Rate limited","description":"retrying in 5s"}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"some_future_update"}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","id":99,"method":"fs/read_text_file","params":{"sessionId":"ses_old","path":"/etc/passwd"}}|};
  recv t "not json";
  [%expect
    {|
    > {"jsonrpc":"2.0","id":0,"method":"initialize","params":{"protocolVersion":1,"clientCapabilities":{"fs":{"readTextFile":false,"writeTextFile":false},"terminal":false,"session":{"notices":{},"compaction":{}}},"clientInfo":{"name":"prigh","version":"0.1"}}}
    < {"jsonrpc":"2.0","id":0,"result":{"protocolVersion":1,"agentCapabilities":{"loadSession":true,"promptCapabilities":{"image":true,"embeddedContext":true},"mcpCapabilities":{"http":true},"sessionCapabilities":{"resume":{}}},"agentInfo":{"name":"opencode","version":"1.2.3"},"authMethods":[{"id":"login","name":"Log in","type":"terminal","description":"Run opencode auth login"}]}}
    (Response
     (Initialized (protocol_version 1)
      (agent (((name opencode) (title ()) (version (1.2.3)))))
      (capabilities
       ((load_session true) (resume_session true) (close_session false)
        (images true) (audio false) (embedded_context true) (mcp_http true)
        (mcp_sse false)))
      (auth_methods
       (((id login) (name "Log in") (description ("Run opencode auth login"))
         (terminal true))))))
    > {"jsonrpc":"2.0","id":1,"method":"session/load","params":{"sessionId":"ses_old","cwd":"/w","mcpServers":[]}}
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"user_message_chunk","content":{"type":"text","text":"old"}}}}
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"old reply"}}}}
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"available_commands_update","availableCommands":[{"name":"review","description":"Review","input":{"hint":"what"}}]}}}
    (Commands (((name review) (description Review) (input_hint (what)))))
    < {"jsonrpc":"2.0","id":1,"result":{"modes":null}}
    (Response
     (Session_opened (kind Load_session) (session_id ses_old)
      (config ((current_mode ()) (modes ()) (config_options ())))))
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_old","prompt":[{"type":"text","text":"hi"}]}}
    (Turn_started (turn_id turn-1))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"other","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"not ours"}}}}
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"current_mode_update","currentModeId":"plan"}}}
    (Config_changed ((current_mode (plan)) (modes ()) (config_options ())))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"session_info_update","title":"Fix the build"}}}
    (Title ("Fix the build"))
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"notice","severity":"warning","title":"Rate limited","description":"retrying in 5s"}}}
    (Notice  "warning: Rate limited\
            \nretrying in 5s")
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_old","update":{"sessionUpdate":"some_future_update"}}}
    < {"jsonrpc":"2.0","id":99,"method":"fs/read_text_file","params":{"sessionId":"ses_old","path":"/etc/passwd"}}
      > {"jsonrpc":"2.0","id":99,"error":{"code":-32601,"message":"method not found: fs/read_text_file (this client does not offer it)"}}
    < not json
    (Notice "the ACP agent wrote a line that is not JSON: not json")
    |}]
;;

let%expect_test
    "the agent exits mid-turn; cancel_request withdraws a permission"
  =
  let t = started () in
  ok (C.prompt t [ Text "go" ]);
  ignore ([%expect.output] : string);
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call","toolCallId":"c1","title":"sleep 100","kind":"execute","status":"in_progress"}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","id":5,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c2","title":"rm x","kind":"delete"},"options":[{"optionId":"y","name":"Yes","kind":"allow_once"}]}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"$/cancel_request","params":{"requestId":5}}|};
  send (C.set_mode t ~mode_id:"plan" |> Or_error.ok_exn);
  List.iter
    (C.process_exited t ~code:(Some 1) ~stderr_tail:"panic")
    ~f:print_output;
  print_s [%sexp (C.in_turn t : bool)];
  [%expect
    {|
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"tool_call","toolCallId":"c1","title":"sleep 100","kind":"execute","status":"in_progress"}}}
    (Tool_start (id c1) (kind Command) (native_name execute) (title "sleep 100")
     (input (Object ((command (String "sleep 100"))))) (parent ()))
    (Tool_update (id c1) (output ""))
    < {"jsonrpc":"2.0","id":5,"method":"session/request_permission","params":{"sessionId":"ses_1","toolCall":{"toolCallId":"c2","title":"rm x","kind":"delete"},"options":[{"optionId":"y","name":"Yes","kind":"allow_once"}]}}
    (Tool_start (id c2) (kind File_change) (native_name delete) (title "rm x")
     (input (Object ())) (parent ()))
    (Approval_request (request_id 5) (tool_id (c2)) (kind File_change)
     (summary "rm x") (input (Object ())) (offers_always false))
    < {"jsonrpc":"2.0","method":"$/cancel_request","params":{"requestId":5}}
      > {"jsonrpc":"2.0","id":5,"error":{"code":-32800,"message":"request cancelled"}}
    (Request_withdrawn (request_id 5))
    > {"jsonrpc":"2.0","id":3,"method":"session/set_mode","params":{"sessionId":"ses_1","modeId":"plan"}}
    (Tool_end (id c1) (ok false) (output "") (changes ()) (images ()))
    (Tool_end (id c2) (ok false) (output "") (changes ()) (images ()))
    (Turn_end (turn_id (turn-1))
     (stop
      (Failed (kind (Crashed (stderr_tail panic)))
       (message "the ACP agent exited (code 1)"))))
    (Response
     (Failed (kind Set_mode) (code -32603)
      (message "the ACP agent exited (code 1)")))
    false
    |}]
;;

let quirks name = (Option.value_exn (Acp_agent.find_known name)).quirks

let%expect_test "Antigravity: steering with another prompt, and its usage" =
  let t = started ~quirks:(quirks "antigravity") () in
  ignore ([%expect.output] : string);
  ok (C.prompt t [ Text "build it" ]);
  ok (C.steer t [ Text "use tabs" ]);
  (* the first prompt ends [end_turn]: the turn goes on under the second *)
  recv
    t
    {|{"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn","usage":{"inputTokens":100,"cachedReadTokens":900,"outputTokens":50,"thoughtTokens":20,"totalTokens":1050}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Tabs it is."}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","id":3,"result":{"stopReason":"end_turn","usage":{"inputTokens":10,"cachedReadTokens":1000,"outputTokens":5}}}|};
  ok (C.steer t [ Text "too late" ]);
  [%expect
    {|
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"build it"}]}}
    (Turn_started (turn_id turn-1))
      > {"jsonrpc":"2.0","id":3,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"use tabs"}]}}
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn","usage":{"inputTokens":100,"cachedReadTokens":900,"outputTokens":50,"thoughtTokens":20,"totalTokens":1050}}}
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Tabs it is."}}}}
    (Text_delta (item turn-1-m1) (text "Tabs it is."))
    < {"jsonrpc":"2.0","id":3,"result":{"stopReason":"end_turn","usage":{"inputTokens":10,"cachedReadTokens":1000,"outputTokens":5}}}
    (Usage
     ((usage ((input 1010) (output 50) (cache_read 1000))) (cost_usd ())
      (context_tokens ()) (context_window ())))
    (Text_done (item turn-1-m1) (text "Tabs it is."))
    (Turn_end (turn_id (turn-1)) (stop Done))
    (Error "no turn is running")
    |}]
;;

let%expect_test "an agent without steering, and prompt usage it does not read" =
  let t = started () in
  ignore ([%expect.output] : string);
  ok (C.prompt t [ Text "hi" ]);
  ok (C.steer t [ Text "more" ]);
  recv
    t
    {|{"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn","usage":{"inputTokens":100,"outputTokens":50}}}|};
  [%expect
    {|
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"hi"}]}}
    (Turn_started (turn_id turn-1))
    (Error "this agent takes new messages between turns")
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn","usage":{"inputTokens":100,"outputTokens":50}}}
    (Turn_end (turn_id (turn-1)) (stop Done))
    |}]
;;

let%expect_test "Grok: interjecting, its questions, its plan, background tasks" =
  let t = started ~quirks:(quirks "grok") () in
  ignore ([%expect.output] : string);
  ok (C.prompt t [ Text "fix it" ]);
  ok (C.steer t [ Text "and the docs" ]);
  recv t {|{"jsonrpc":"2.0","id":3,"result":{"status":"queued"}}|};
  recv
    t
    {|{"jsonrpc":"2.0","id":"q1","method":"_x.ai/ask_user_question","params":{"sessionId":"ses_1","toolCallId":"tc1","mode":"default","questions":[{"question":"Which file?","options":[{"label":"a.ml"},{"label":"b.ml","description":"the other"}],"multiSelect":false}]}}|};
  print_s
    [%sexp
      (C.answer t ~request_id:"q1" (Answered [ [ "b.ml" ] ]) : string option)];
  recv
    t
    {|{"jsonrpc":"2.0","method":"x.ai/task_backgrounded","params":{"sessionId":"ses_1","update":{"task_id":"t1","task_snapshot":{"kind":"bash","command":"make watch"}}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","id":"p1","method":"x.ai/exit_plan_mode","params":{"method":"x.ai/exit_plan_mode","params":{"sessionId":"ses_1","toolCallId":"tc2","planContent":"1. Fix\n2. Test"}}}|};
  recv
    t
    {|{"jsonrpc":"2.0","id":"u1","method":"x.ai/something_new","params":{}}|};
  recv
    t
    {|{"jsonrpc":"2.0","id":"q2","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","toolCallId":"tc3","mode":"default","questions":[{"question":"Sure?","options":[]}]}}|};
  recv t {|{"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn"}}|};
  [%expect
    {|
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"fix it"}]}}
    (Turn_started (turn_id turn-1))
      > {"jsonrpc":"2.0","id":3,"method":"_x.ai/interject","params":{"sessionId":"ses_1","text":"and the docs"}}
    < {"jsonrpc":"2.0","id":3,"result":{"status":"queued"}}
    < {"jsonrpc":"2.0","id":"q1","method":"_x.ai/ask_user_question","params":{"sessionId":"ses_1","toolCallId":"tc1","mode":"default","questions":[{"question":"Which file?","options":[{"label":"a.ml"},{"label":"b.ml","description":"the other"}],"multiSelect":false}]}}
    (User_question (request_id q1)
     (questions
      (((question "Which file?") (header ())
        (choices
         (((label a.ml) (description ()))
          ((label b.ml) (description ("the other")))))
        (multiple false)))))
    ("{\"jsonrpc\":\"2.0\",\"id\":\"q1\",\"result\":{\"outcome\":\"accepted\",\"answers\":{\"Which file?\":[\"b.ml\"]}}}")
    < {"jsonrpc":"2.0","method":"x.ai/task_backgrounded","params":{"sessionId":"ses_1","update":{"task_id":"t1","task_snapshot":{"kind":"bash","command":"make watch"}}}}
    (Notice "the agent runs a background task (make watch)")
    < {"jsonrpc":"2.0","id":"p1","method":"x.ai/exit_plan_mode","params":{"method":"x.ai/exit_plan_mode","params":{"sessionId":"ses_1","toolCallId":"tc2","planContent":"1. Fix\n2. Test"}}}
    (Approval_request (request_id p1) (tool_id (tc2)) (kind (Other ExitPlanMode))
     (summary  "1. Fix\
              \n2. Test")
     (input
      (Object
       ((sessionId (String ses_1)) (toolCallId (String tc2))
        (planContent (String  "1. Fix\
                             \n2. Test")))))
     (offers_always false))
    < {"jsonrpc":"2.0","id":"u1","method":"x.ai/something_new","params":{}}
      > {"jsonrpc":"2.0","id":"u1","error":{"code":-32601,"message":"method not found: x.ai/something_new"}}
    < {"jsonrpc":"2.0","id":"q2","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","toolCallId":"tc3","mode":"default","questions":[{"question":"Sure?","options":[]}]}}
    (User_question (request_id q2)
     (questions (((question Sure?) (header ()) (choices ()) (multiple false)))))
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn"}}
    (Request_withdrawn (request_id p1))
      > {"jsonrpc":"2.0","id":"q2","result":{"outcome":"cancelled"}}
    (Request_withdrawn (request_id q2))
    (Turn_end (turn_id (turn-1)) (stop Done))
    |}]
;;

let%expect_test "x.ai methods are only Grok's" =
  let t = started () in
  ignore ([%expect.output] : string);
  recv
    t
    {|{"jsonrpc":"2.0","id":"q1","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","questions":[]}}|};
  [%expect
    {|
    < {"jsonrpc":"2.0","id":"q1","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","questions":[]}}
      > {"jsonrpc":"2.0","id":"q1","error":{"code":-32601,"message":"method not found: x.ai/ask_user_question (this client does not offer it)"}}
    |}]
;;

let%expect_test "Grok: questions after a stop or outside a turn; typed answers" =
  let t = started ~quirks:(quirks "grok") () in
  ignore ([%expect.output] : string);
  let ask id =
    recv
      t
      (sprintf
         {|{"jsonrpc":"2.0","id":"%s","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","questions":[{"question":"Which file?","options":[{"label":"a.ml"}]},{"question":"Why?","options":[]},{"question":"","options":[]}]}}|}
         id)
  in
  (* outside a turn: cancelled at once *)
  ask "q0";
  ok (C.prompt t [ Text "fix it" ]);
  ask "q1";
  print_s
    [%sexp
      (C.answer
         t
         ~request_id:"q1"
         (Answered [ [ "a.ml"; "and c.ml" ]; [ "because" ]; [ "x" ] ])
       : string option)];
  ask "q2";
  (* stopping answers it, and one asked while stopping is cancelled *)
  ok (Ok (C.cancel t));
  ask "q3";
  [%expect
    {|
    < {"jsonrpc":"2.0","id":"q0","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","questions":[{"question":"Which file?","options":[{"label":"a.ml"}]},{"question":"Why?","options":[]},{"question":"","options":[]}]}}
      > {"jsonrpc":"2.0","id":"q0","result":{"outcome":"cancelled"}}
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"fix it"}]}}
    (Turn_started (turn_id turn-1))
    < {"jsonrpc":"2.0","id":"q1","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","questions":[{"question":"Which file?","options":[{"label":"a.ml"}]},{"question":"Why?","options":[]},{"question":"","options":[]}]}}
    (User_question (request_id q1)
     (questions
      (((question "Which file?") (header ())
        (choices (((label a.ml) (description ())))) (multiple false))
       ((question Why?) (header ()) (choices ()) (multiple false))
       ((question "") (header ()) (choices ()) (multiple false)))))
    ("{\"jsonrpc\":\"2.0\",\"id\":\"q1\",\"result\":{\"outcome\":\"accepted\",\"answers\":{\"Which file?\":[\"a.ml\"],\"Why?\":[\"Other\"]},\"annotations\":{\"Which file?\":{\"notes\":\"and c.ml\"},\"Why?\":{\"notes\":\"because\"}}}}")
    < {"jsonrpc":"2.0","id":"q2","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","questions":[{"question":"Which file?","options":[{"label":"a.ml"}]},{"question":"Why?","options":[]},{"question":"","options":[]}]}}
    (User_question (request_id q2)
     (questions
      (((question "Which file?") (header ())
        (choices (((label a.ml) (description ())))) (multiple false))
       ((question Why?) (header ()) (choices ()) (multiple false))
       ((question "") (header ()) (choices ()) (multiple false)))))
      > {"jsonrpc":"2.0","method":"session/cancel","params":{"sessionId":"ses_1"}}
      > {"jsonrpc":"2.0","id":"q2","result":{"outcome":"cancelled"}}
    (Request_withdrawn (request_id q2))
    < {"jsonrpc":"2.0","id":"q3","method":"x.ai/ask_user_question","params":{"sessionId":"ses_1","questions":[{"question":"Which file?","options":[{"label":"a.ml"}]},{"question":"Why?","options":[]},{"question":"","options":[]}]}}
      > {"jsonrpc":"2.0","id":"q3","result":{"outcome":"cancelled"}}
    |}]
;;

let%expect_test "a message for the running turn that the agent refuses" =
  let check name =
    let t = started ~quirks:(quirks name) () in
    ignore ([%expect.output] : string);
    ok (C.prompt t [ Text "go" ]);
    ok (C.steer t [ Text "also this" ]);
    recv t {|{"jsonrpc":"2.0","id":3,"error":{"code":-32603,"message":"busy"}}|};
    recv t {|{"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn"}}|}
  in
  check "antigravity";
  [%expect
    {|
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"go"}]}}
    (Turn_started (turn_id turn-1))
      > {"jsonrpc":"2.0","id":3,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"also this"}]}}
    < {"jsonrpc":"2.0","id":3,"error":{"code":-32603,"message":"busy"}}
    (Notice
     "the agent did not take your message sent during its turn (busy): it never saw it, so send it again once this turn ends")
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn"}}
    (Turn_end (turn_id (turn-1)) (stop Done))
    |}];
  check "grok";
  [%expect
    {|
      > {"jsonrpc":"2.0","id":2,"method":"session/prompt","params":{"sessionId":"ses_1","prompt":[{"type":"text","text":"go"}]}}
    (Turn_started (turn_id turn-1))
      > {"jsonrpc":"2.0","id":3,"method":"_x.ai/interject","params":{"sessionId":"ses_1","text":"also this"}}
    < {"jsonrpc":"2.0","id":3,"error":{"code":-32603,"message":"busy"}}
    (Notice
     "the agent did not take your message sent during its turn (busy): it never saw it, so send it again once this turn ends")
    < {"jsonrpc":"2.0","id":2,"result":{"stopReason":"end_turn"}}
    (Turn_end (turn_id (turn-1)) (stop Done))
    |}]
;;

let%expect_test "the agent changes its model itself" =
  let t = C.create () in
  send (C.initialize t ~client_name:"prigh" ~client_version:"0.1");
  recv t init_response;
  send (C.new_session t ~cwd:"/work" ~mcp_servers:[]);
  recv
    t
    {|{"jsonrpc":"2.0","id":1,"result":{"sessionId":"ses_1","models":{"currentModelId":"flash","availableModels":[{"modelId":"flash","name":"Flash"},{"modelId":"pro","name":"Pro"}]}}}|};
  ignore ([%expect.output] : string);
  recv
    t
    {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"current_model_update","currentModelId":"pro"}}}|};
  ok_line (C.set_config_option t ~config_id:"prigh:session-model" (`Id "flash"));
  [%expect
    {|
    < {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":{"sessionUpdate":"current_model_update","currentModelId":"pro"}}}
    (Config_changed
     ((current_mode ()) (modes ())
      (config_options
       (((id prigh:session-model) (name Model) (description ())
         (category (model))
         (value
          (Select (current pro)
           (choices
            (((value flash) (name Flash) (description ()) (group ()))
             ((value pro) (name Pro) (description ()) (group ())))))))))))
    > {"jsonrpc":"2.0","id":2,"method":"session/set_model","params":{"sessionId":"ses_1","modelId":"flash"}}
    |}]
;;

(* ACP's [search] kind is web searches as well as code searches: the
   agent's tool name, input and title decide which prigh tool a call is
   recorded as. Each line is labelled; under it, what the recorder makes of
   it (no deltas), so that a call said to be [in_progress] starts (its
   message ends) then, not when it completes. *)
let%expect_test "Antigravity's web searches, a fetch and code searches" =
  let t = started ~quirks:(quirks "antigravity") () in
  ok (C.prompt t [ Text "does Ultra include API credits?" ]);
  ignore ([%expect.output] : string);
  let recorder =
    Composer_recorder.create
      ~adapter:"acp:antigravity"
      ~model:None
      ~cwd:"/work"
      ~emit:(fun e ->
        match e with
        | Message_update _
        | Message_start (User _ | Tool_result _)
        | Message_end (User _ | Tool_result _) -> ()
        | e -> print_endline ("  " ^ Test_agent_loop.show_event e))
      ~notice:(fun n -> print_endline ("  notice: " ^ n))
      ~prompts:[]
  in
  let line label update =
    print_endline label;
    List.iter
      (C.handle_line
         t
         (sprintf
            {|{"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses_1","update":%s}}|}
            update))
      ~f:(function
        | Event e -> Composer_recorder.feed recorder e
        | o -> print_output o)
  in
  line
    "search_web, pending"
    {|{"sessionUpdate":"tool_call","toolCallId":"s1","title":"Running search_web","kind":"search","status":"pending","rawInput":{"query":"\"Google AI Ultra\" Gemini API usage credits included"}}|};
  line
    "search_web, in progress"
    {|{"sessionUpdate":"tool_call_update","toolCallId":"s1","status":"in_progress"}|};
  line
    "search_web, completed"
    {|{"sessionUpdate":"tool_call_update","toolCallId":"s1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"Gemini API pricing\nhttps://ai.google.dev/pricing"}}]}|};
  line
    "a search titled with its query, only completed"
    {|{"sessionUpdate":"tool_call","toolCallId":"s2","title":"site:developers.google.com OR site:ai.google.dev Ultra API credits","kind":"search","status":"completed","rawInput":{"query":"site:developers.google.com OR site:ai.google.dev Ultra API credits","domain":""}}|};
  line
    "a fetch"
    {|{"sessionUpdate":"tool_call","toolCallId":"f1","title":"Fetching https://ai.google.dev/pricing","kind":"fetch","status":"completed","rawInput":{"Url":"https://ai.google.dev/pricing"},"content":[{"type":"content","content":{"type":"text","text":"Pricing"}}]}|};
  line
    "grep_search"
    {|{"sessionUpdate":"tool_call","toolCallId":"g1","title":"Running grep_search","kind":"search","status":"completed","rawInput":{"Query":"usage_limit","SearchPath":"/work/backend","CaseInsensitive":true},"content":[{"type":"content","content":{"type":"text","text":"lib/a.ml:3: usage_limit"}}]}|};
  line
    "find_by_name"
    {|{"sessionUpdate":"tool_call","toolCallId":"g2","title":"Running find_by_name","kind":"search","status":"completed","rawInput":{"Pattern":"*.ml","SearchDirectory":"/work"}}|};
  line
    "a grep without a tool name"
    {|{"sessionUpdate":"tool_call","toolCallId":"g3","title":"Search for TODO","kind":"search","status":"completed","rawInput":{"pattern":"TODO","path":"src"}}|};
  line
    "a search that says what it is nowhere"
    {|{"sessionUpdate":"tool_call","toolCallId":"o1","title":"Looking things up","kind":"search","status":"completed"}|};
  line
    "a web search named in its _meta, of kind other"
    {|{"sessionUpdate":"tool_call","toolCallId":"m1","title":"Searching","kind":"other","status":"completed","rawInput":{"q":"ocaml eio"},"_meta":{"agent":{"toolName":"google_web_search"}}}|};
  Composer_recorder.feed recorder (Turn_end { turn_id = None; stop = Done });
  ignore (Composer_recorder.finish recorder : Message.t list);
  [%expect
    {|
      agent_start
    search_web, pending
      turn_start
      agent_waiting Antigravity
      message_start assistant
    search_web, in progress
      message_end assistant ""
      tool_start web_search {"query":"\"Google AI Ultra\" Gemini API usage credits included"}
    search_web, completed
        tool_output "Gemini API pricing\nhttps://ai.google.dev/pricing"
      tool_end "Gemini API pricing\nhttps://ai.google.dev/pricing"
      turn_end stop=Tool_use tool_results=1
    a search titled with its query, only completed
      turn_start
      agent_waiting Antigravity
      message_start assistant
      message_end assistant ""
      tool_start web_search {"query":"site:developers.google.com OR site:ai.google.dev Ultra API credits"}
      tool_end ""
      turn_end stop=Tool_use tool_results=1
    a fetch
      turn_start
      agent_waiting Antigravity
      message_start assistant
      message_end assistant ""
      tool_start web_fetch {"url":"https://ai.google.dev/pricing"}
        tool_output "Pricing"
      tool_end "Pricing"
      turn_end stop=Tool_use tool_results=1
    grep_search
      turn_start
      agent_waiting Antigravity
      message_start assistant
      message_end assistant ""
      tool_start grep {"pattern":"usage_limit","path":"/work/backend"}
        tool_output "lib/a.ml:3: usage_limit"
      tool_end "lib/a.ml:3: usage_limit"
      turn_end stop=Tool_use tool_results=1
    find_by_name
      turn_start
      agent_waiting Antigravity
      message_start assistant
      message_end assistant ""
      tool_start find {"pattern":"*.ml","path":"/work"}
      tool_end ""
      turn_end stop=Tool_use tool_results=1
    a grep without a tool name
      turn_start
      agent_waiting Antigravity
      message_start assistant
      message_end assistant ""
      tool_start grep {"pattern":"TODO","path":"src"}
      tool_end ""
      turn_end stop=Tool_use tool_results=1
    a search that says what it is nowhere
      turn_start
      agent_waiting Antigravity
      message_start assistant
      message_end assistant ""
      tool_start Looking things up {}
      tool_end ""
      turn_end stop=Tool_use tool_results=1
    a web search named in its _meta, of kind other
      turn_start
      agent_waiting Antigravity
      message_start assistant
      message_end assistant ""
      tool_start web_search {"query":"ocaml eio"}
      tool_end ""
      turn_end stop=Tool_use tool_results=1
      agent_end (16 new messages)
    |}]
;;
