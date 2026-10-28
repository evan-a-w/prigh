open! Core
open! Prigh
module H = Acp_harness

(* [prigh acp]'s protocol: initialization, sessions, turns and their
   updates, permissions, cancellation, modes and config options, errors. *)

let%expect_test "initialize advertises prigh's capabilities and extensions" =
  H.run ~initialized:false []
  @@ fun h ->
  H.initialize h;
  H.dump h;
  [%expect
    {|
    > request 0 initialize {"protocolVersion":1,"clientCapabilities":{"_meta":{"prigh":{"state":true}}},"clientInfo":{"name":"test-editor","version":"1"}}
    < result 0 {"protocolVersion":1,"agentCapabilities":{"loadSession":true,"promptCapabilities":{"image":true,"audio":false,"embeddedContext":true},"mcpCapabilities":{"http":false,"sse":false},"sessionCapabilities":{"list":{},"resume":{},"close":{}},"_meta":{"prigh":{"version":1,"extensions":["_prigh/state","_prigh/notice"]}}},"authMethods":[{"id":"prigh-login","name":"prigh login","description":"Log in to a model provider: run `prigh login` in a terminal (or /login in the prigh TUI), then try again"}],"agentInfo":{"name":"prigh","title":"prigh","version":"<version>"},"_meta":{"prigh":{"version":1,"extensions":["_prigh/state","_prigh/notice"]}}}
    |}]
;;

let%expect_test
    "session/new: the session, its modes and options, commands, state"
  =
  H.run ~initialized:false []
  @@ fun h ->
  H.verbose := true;
  H.initialize h;
  H.dump h;
  [%expect
    {|
    > request 0 initialize {"protocolVersion":1,"clientCapabilities":{"_meta":{"prigh":{"state":true}}},"clientInfo":{"name":"test-editor","version":"1"}}
    < result 0 {"protocolVersion":1,"agentCapabilities":{"loadSession":true,"promptCapabilities":{"image":true,"audio":false,"embeddedContext":true},"mcpCapabilities":{"http":false,"sse":false},"sessionCapabilities":{"list":{},"resume":{},"close":{}},"_meta":{"prigh":{"version":1,"extensions":["_prigh/state","_prigh/notice"]}}},"authMethods":[{"id":"prigh-login","name":"prigh login","description":"Log in to a model provider: run `prigh login` in a terminal (or /login in the prigh TUI), then try again"}],"agentInfo":{"name":"prigh","title":"prigh","version":"<version>"},"_meta":{"prigh":{"version":1,"extensions":["_prigh/state","_prigh/notice"]}}}
    |}];
  H.new_session h;
  H.dump h;
  [%expect
    {|
    > request 1 session/new {"cwd":"$DIR","mcpServers":[]}
    < result 1 {"sessionId":"<id>","modes":{"currentModeId":"auto","availableModes":[{"id":"ask","name":"Ask","description":"ask before every tool that changes something"},{"id":"auto-edit","name":"Auto-edit","description":"edit files without asking; ask before commands"},{"id":"auto","name":"Auto","description":"run every tool without asking"},{"id":"plan","name":"Plan","description":"investigate and plan, changing no files, until you approve the plan"}]},"configOptions":[{"id":"mode","name":"Mode","category":"mode","type":"select","currentValue":"auto","options":[{"value":"ask","name":"Ask","description":"ask before every tool that changes something"},{"value":"auto-edit","name":"Auto-edit","description":"edit files without asking; ask before commands"},{"value":"auto","name":"Auto","description":"run every tool without asking"},{"value":"plan","name":"Plan","description":"investigate and plan, changing no files, until you approve the plan"}]},{"id":"model","name":"Model","category":"model","type":"select","currentValue":"deepseek/deepseek-flash","options":[{"value":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","description":"deepseek/deepseek-flash"}]},{"id":"thinking","name":"Thinking","category":"thought_level","type":"select","currentValue":"on","options":[{"value":"off","name":"off"},{"value":"on","name":"on"},{"value":"low","name":"low"},{"value":"high","name":"high"},{"value":"max","name":"max"}]}]}
    < update available_commands_update {"availableCommands":[{"name":"compact","description":"summarise older messages to free context","input":{"hint":"[instructions]"}},{"name":"goal","description":"set a goal the agent keeps working toward until it is met; show, clear or resume it","input":{"hint":"[text|off|resume]"}},{"name":"remind","description":"remind the agent when it ends its turn (stop) or every N turns; list or remove reminders","input":{"hint":"[stop [N] TEXT | every N TEXT | rm NAME | off]"}},{"name":"budget","description":"show or set the session's cost budget: no turn starts once it is spent","input":{"hint":"[dollars|off]"}},{"name":"model","description":"show or set the model (id, name or prefix)","input":{"hint":"[model]"}},{"name":"thinking","description":"show or set the thinking level","input":{"hint":"[off|on|low|high|max]"}},{"name":"plan","description":"toggle plan mode: the model investigates and plans, changing no files","input":{"hint":"[on|off]"}},{"name":"review","description":"review the uncommitted changes, or those against a base branch","input":{"hint":"[base] [instructions]"}},{"name":"init","description":"write or improve the project's AGENTS.md"},{"name":"btw","description":"ask a side question without adding it to the session","input":{"hint":"question"}},{"name":"name","description":"set the session name","input":{"hint":"text"}},{"name":"diff","description":"show the uncommitted changes (git diff)","input":{"hint":"[path]"}}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}]
;;

let%expect_test "without _meta.prigh.state there is no _prigh/state" =
  H.run ~initialized:false []
  @@ fun h ->
  H.initialize ~caps:"{}" h;
  H.new_session h;
  H.dump h;
  [%expect
    {|
    > request 0 initialize {"protocolVersion":1,"clientCapabilities":{},"clientInfo":{"name":"test-editor","version":"1"}}
    < result 0 {"protocolVersion":1,"agentCapabilities":{"loadSession":true,"promptCapabilities":{"image":true,"audio":false,"embeddedContext":true},"mcpCapabilities":{"http":false,"sse":false},"sessionCapabilities":{"list":{},"resume":{},"close":{}},"_meta":{"prigh":{"version":1,"extensions":["_prigh/state","_prigh/notice"]}}},"authMethods":[{"id":"prigh-login","name":"prigh login","description":"Log in to a model provider: run `prigh login` in a terminal (or /login in the prigh TUI), then try again"}],"agentInfo":{"name":"prigh","title":"prigh","version":"<version>"},"_meta":{"prigh":{"version":1,"extensions":["_prigh/state","_prigh/notice"]}}}
    > request 1 session/new {"cwd":"$DIR","mcpServers":[]}
    < result 1 {"sessionId":"<id>","modes":"auto[ask|auto-edit|auto|plan]","configOptions":"mode=auto[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=on[off|on|low|high|max]"}
    < update available_commands_update {"availableCommands":"compact goal remind budget model thinking plan review init btw name diff"}
    |}]
;;

let%expect_test
    "a turn streams thoughts, text, tool calls with locations and diffs"
  =
  H.run
    [ {|{"thinking": "Let me look.", "text": "Reading.", "tool_calls": [{"id": "c1", "name": "read", "arguments": {"path": "a.txt", "offset": 2}}]}|}
    ; {|{"text": "Editing.", "tool_calls": [{"id": "c2", "name": "edit", "arguments": {"path": "a.txt", "edits": [{"old_text": "two", "new_text": "2"}]}}, {"id": "c3", "name": "bash", "arguments": {"command": "echo hi"}}]}|}
    ; {|{"text": "Done.", "usage": {"input": 1200, "output": 30}}|}
    ]
  @@ fun h ->
  Tool_test_helpers.write h.sandbox "a.txt" "one\ntwo\nthree\n";
  ignore (H.prompt h "fix a.txt" : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"fix a.txt"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update agent_thought_chunk {"content":{"type":"text","text":"Let me look."}}
    < update agent_message_chunk {"content":{"type":"text","text":"Reading."}}
    < update tool_call {"toolCallId":"c1","title":"Read","kind":"read","status":"pending"}
    < update tool_call_update {"toolCallId":"c1","title":"Read a.txt","kind":"read","status":"in_progress","rawInput":{"path":"a.txt","offset":2},"locations":[{"path":"$DIR/a.txt","line":2}]}
    < update tool_call_update {"toolCallId":"c1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"two\nthree\n"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"Editing."}}
    < update tool_call {"toolCallId":"c2","title":"Edit","kind":"edit","status":"pending"}
    < update tool_call {"toolCallId":"c3","title":"bash","kind":"execute","status":"pending"}
    < update tool_call_update {"toolCallId":"c2","title":"Edit a.txt","kind":"edit","status":"in_progress","rawInput":{"path":"a.txt","edits":[{"old_text":"two","new_text":"2"}]},"locations":[{"path":"$DIR/a.txt"}]}
    < update tool_call_update {"toolCallId":"c2","status":"completed","content":[{"type":"diff","path":"$DIR/a.txt","oldText":"one\ntwo\nthree\n","newText":"one\n2\nthree\n"}]}
    < update tool_call_update {"toolCallId":"c3","title":"echo hi","kind":"execute","status":"in_progress","rawInput":{"command":"echo hi"}}
    < update tool_call_update {"toolCallId":"c3","content":[{"type":"content","content":{"type":"text","text":"```\nhi\n```"}}]}
    < update tool_call_update {"toolCallId":"c3","status":"completed","content":[{"type":"content","content":{"type":"text","text":"```\nhi\n```"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"Done."}}
    < update usage_update {"used":1200,"size":1000000,"cost":{"amount":0.000396,"currency":"USD"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":1200,"context_window":1000000,"cost_usd":0.000396,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 2 {"stopReason":"end_turn"}
    |}];
  print_string (Tool_test_helpers.read h.sandbox "a.txt");
  [%expect
    {|
    one
    2
    three
    |}]
;;

let%expect_test "permissions: allow, always, reject, and cancelled denies" =
  let bash id command =
    sprintf
      {|{"tool_calls": [{"id": %S, "name": "bash", "arguments": {"command": %S}}]}|}
      id
      command
  in
  H.run
    [ bash "c1" "echo one"
    ; {|{"text": "ran"}|}
    ; bash "c2" "echo two"
    ; {|{"text": "ran"}|}
    ; bash "c3" "pwd"
    ; {|{"text": "rejected"}|}
    ; bash "c4" "ls"
    ; {|{"text": "cancelled"}|}
    ]
  @@ fun h ->
  ignore
    (H.session_request
       h
       "session/set_mode"
       {|{"sessionId": "%s", "modeId": "ask"}|}
     : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 2 session/set_mode {"sessionId":"<id>","modeId":"ask"}
    < result 2 {}
    < update current_mode_update {"currentModeId":"ask"}
    < update config_option_update {"configOptions":"mode=ask[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=on[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  let pick option_id ~method_ ~params:_ =
    match method_ with
    | "session/request_permission" ->
      H.Answer.Result
        (sprintf
           {|{"outcome": {"outcome": "selected", "optionId": %S}}|}
           option_id)
    | _ -> H.default_answer ~method_ ~params:()
  in
  h.answer <- pick "allow";
  ignore (H.prompt h "one" : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"one"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call {"toolCallId":"c1","title":"bash","kind":"execute","status":"pending"}
    < update tool_call_update {"toolCallId":"c1","title":"echo one","kind":"execute","status":"in_progress","rawInput":{"command":"echo one"}}
    < request 0 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"c1","title":"echo one","kind":"execute","status":"pending","rawInput":{"command":"echo one"}},"options":[{"optionId":"allow","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always allow bash(echo:*)","kind":"allow_always"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    > result 0 {"outcome":{"outcome":"selected","optionId":"allow"}}
    < update tool_call_update {"toolCallId":"c1","content":[{"type":"content","content":{"type":"text","text":"```\none\n```"}}]}
    < update tool_call_update {"toolCallId":"c1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"```\none\n```"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"ran"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 3 {"stopReason":"end_turn"}
    |}];
  h.answer <- pick "always";
  ignore (H.prompt h "two" : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 4 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"two"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call {"toolCallId":"c2","title":"bash","kind":"execute","status":"pending"}
    < update tool_call_update {"toolCallId":"c2","title":"echo two","kind":"execute","status":"in_progress","rawInput":{"command":"echo two"}}
    < request 1 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"c2","title":"echo two","kind":"execute","status":"pending","rawInput":{"command":"echo two"}},"options":[{"optionId":"allow","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always allow bash(echo:*)","kind":"allow_always"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    > result 1 {"outcome":{"outcome":"selected","optionId":"always"}}
    < update tool_call_update {"toolCallId":"c2","content":[{"type":"content","content":{"type":"text","text":"```\ntwo\n```"}}]}
    < update tool_call_update {"toolCallId":"c2","status":"completed","content":[{"type":"content","content":{"type":"text","text":"```\ntwo\n```"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"ran"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 4 {"stopReason":"end_turn"}
    |}];
  h.answer <- pick "reject";
  ignore (H.prompt h "three" : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 5 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"three"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call {"toolCallId":"c3","title":"bash","kind":"execute","status":"pending"}
    < update tool_call_update {"toolCallId":"c3","title":"pwd","kind":"execute","status":"in_progress","rawInput":{"command":"pwd"}}
    < request 2 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"c3","title":"pwd","kind":"execute","status":"pending","rawInput":{"command":"pwd"}},"options":[{"optionId":"allow","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always allow bash(pwd:*)","kind":"allow_always"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    > result 2 {"outcome":{"outcome":"selected","optionId":"reject"}}
    < update tool_call_update {"toolCallId":"c3","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[denied by user]"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"rejected"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 5 {"stopReason":"end_turn"}
    |}];
  h.answer
  <- (fun ~method_:_ ~params:_ ->
       H.Answer.Result {|{"outcome": {"outcome": "cancelled"}}|});
  ignore (H.prompt h "four" : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 6 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"four"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call {"toolCallId":"c4","title":"bash","kind":"execute","status":"pending"}
    < update tool_call_update {"toolCallId":"c4","title":"ls","kind":"execute","status":"in_progress","rawInput":{"command":"ls"}}
    < request 3 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"c4","title":"ls","kind":"execute","status":"pending","rawInput":{"command":"ls"}},"options":[{"optionId":"allow","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always allow bash(ls:*)","kind":"allow_always"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    > result 3 {"outcome":{"outcome":"cancelled"}}
    < update tool_call_update {"toolCallId":"c4","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[denied by user]"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"cancelled"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 6 {"stopReason":"end_turn"}
    |}]
;;

let%expect_test
    "permission requests for file changes show the files and the diff"
  =
  H.run
    ~config:{|{"approval_mode": "ask"}|}
    [ {|{"tool_calls": [{"id": "w1", "name": "write", "arguments": {"path": "a.txt", "content": "new\n"}}]}|}
    ; {|{"tool_calls": [{"id": "e1", "name": "edit", "arguments": {"path": "a.txt", "edits": [{"old_text": "old", "new_text": "OLD"}]}}]}|}
    ; {|{"text": "both rejected"}|}
    ]
  @@ fun h ->
  Out_channel.write_all (Filename.concat h.sandbox.dir "a.txt") ~data:"old\n";
  h.answer
  <- (fun ~method_ ~params:_ ->
       match method_ with
       | "session/request_permission" ->
         H.Answer.Result
           {|{"outcome": {"outcome": "selected", "optionId": "reject"}}|}
       | _ -> H.default_answer ~method_ ~params:());
  ignore (H.prompt h "change a.txt" : Jsonaf.t);
  Queue.filter_inplace h.log ~f:(fun line ->
    String.is_substring line ~substring:"request_permission");
  H.dump h;
  [%expect
    {|
    < request 0 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"w1","title":"Write a.txt","kind":"edit","status":"pending","rawInput":{"path":"a.txt","content":"new\n"},"content":[{"type":"diff","path":"$DIR/a.txt","oldText":"old\n","newText":"new\n"}],"locations":[{"path":"$DIR/a.txt"}]},"options":[{"optionId":"allow","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always allow edit","kind":"allow_always"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    < request 1 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"e1","title":"Edit a.txt","kind":"edit","status":"pending","rawInput":{"path":"a.txt","edits":[{"old_text":"old","new_text":"OLD"}]},"content":[{"type":"diff","path":"$DIR/a.txt","oldText":"old","newText":"OLD"}],"locations":[{"path":"$DIR/a.txt"}]},"options":[{"optionId":"allow","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always allow edit","kind":"allow_always"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    |}]
;;

let%expect_test "session/cancel mid-turn: the pending permission is withdrawn" =
  H.run
    [ {|{"tool_calls": [{"id": "c1", "name": "write", "arguments": {"path": "new.txt", "content": "x"}}]}|}
    ; {|{"text": "never"}|}
    ]
  @@ fun h ->
  ignore
    (H.session_request
       h
       "session/set_mode"
       {|{"sessionId": "%s", "modeId": "ask"}|}
     : Jsonaf.t);
  h.answer <- (fun ~method_:_ ~params:_ -> H.Answer.Hold);
  H.dump h;
  [%expect
    {|
    > request 2 session/set_mode {"sessionId":"<id>","modeId":"ask"}
    < result 2 {}
    < update current_mode_update {"currentModeId":"ask"}
    < update config_option_update {"configOptions":"mode=ask[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=on[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  ignore (H.prompt ~wait:false h "write it" : Jsonaf.t);
  H.wait_for h "session/request_permission";
  H.notify h "session/cancel" (sprintf {|{"sessionId": %S}|} h.session);
  H.wait_for h "stopReason";
  H.respond_held
    h
    "session/request_permission"
    (Result {|{"outcome": {"outcome": "cancelled"}}|});
  H.settle h;
  H.dump h;
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"write it"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call {"toolCallId":"c1","title":"Write","kind":"edit","status":"pending"}
    < update tool_call_update {"toolCallId":"c1","title":"Write new.txt","kind":"edit","status":"in_progress","rawInput":{"path":"new.txt","content":"x"},"locations":[{"path":"$DIR/new.txt"}]}
    < request 0 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"c1","title":"Write new.txt","kind":"edit","status":"pending","rawInput":{"path":"new.txt","content":"x"},"content":[{"type":"diff","path":"$DIR/new.txt","oldText":null,"newText":"x"}],"locations":[{"path":"$DIR/new.txt"}]},"options":[{"optionId":"allow","name":"Allow","kind":"allow_once"},{"optionId":"always","name":"Always allow edit","kind":"allow_always"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    > session/cancel {"sessionId":"<id>"}
    < $/cancel_request {"requestId":0}
    < update tool_call_update {"toolCallId":"c1","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[cancelled]"}}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 3 {"stopReason":"cancelled"}
    > result 0 {"outcome":{"outcome":"cancelled"}}
    |}];
  print_s
    [%sexp
      (Sys_unix.file_exists_exn (Filename.concat h.sandbox.dir "new.txt")
       : bool)];
  [%expect {| false |}]
;;

let%expect_test "session/cancel while a command runs" =
  H.run
    [ {|{"tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "echo started; sleep 30"}}]}|}
    ; {|{"text": "never"}|}
    ]
  @@ fun h ->
  ignore (H.prompt ~wait:false h "wait" : Jsonaf.t);
  H.wait_for h "started";
  H.notify h "session/cancel" (sprintf {|{"sessionId": %S}|} h.session);
  H.wait_for h "stopReason";
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"wait"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call {"toolCallId":"c1","title":"bash","kind":"execute","status":"pending"}
    < update tool_call_update {"toolCallId":"c1","title":"echo started; sleep 30","kind":"execute","status":"in_progress","rawInput":{"command":"echo started; sleep 30"}}
    < update tool_call_update {"toolCallId":"c1","content":[{"type":"content","content":{"type":"text","text":"```\nstarted\n```"}}]}
    > session/cancel {"sessionId":"<id>"}
    < update tool_call_update {"toolCallId":"c1","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[cancelled]"}}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 2 {"stopReason":"cancelled"}
    |}]
;;

let%expect_test "session/load replays the history; session/list lists it" =
  H.run
    [ {|{"thinking": "Hmm.", "text": "Writing.", "tool_calls": [{"id": "c1", "name": "write", "arguments": {"path": "b.txt", "content": "new\n"}}]}|}
    ; {|{"text": "Wrote b.txt."}|}
    ]
  @@ fun h ->
  ignore (H.prompt h "write b.txt" : Jsonaf.t);
  let id = h.session in
  ignore
    (H.session_request h "session/close" {|{"sessionId": "%s"}|} : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"write b.txt"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update agent_thought_chunk {"content":{"type":"text","text":"Hmm."}}
    < update agent_message_chunk {"content":{"type":"text","text":"Writing."}}
    < update tool_call {"toolCallId":"c1","title":"Write","kind":"edit","status":"pending"}
    < update tool_call_update {"toolCallId":"c1","title":"Write b.txt","kind":"edit","status":"in_progress","rawInput":{"path":"b.txt","content":"new\n"},"locations":[{"path":"$DIR/b.txt"}]}
    < update tool_call_update {"toolCallId":"c1","status":"completed","content":[{"type":"diff","path":"$DIR/b.txt","oldText":null,"newText":"new\n"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"Wrote b.txt."}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 2 {"stopReason":"end_turn"}
    > request 3 session/close {"sessionId":"<id>"}
    < result 3 {}
    |}];
  ignore (H.request h "session/list" "{}" : Jsonaf.t);
  ignore (H.request h "session/list" {|{"cwd": "/elsewhere"}|} : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 4 session/list {}
    < result 4 {"sessions":[{"sessionId":"<id>","cwd":"$DIR","title":"write b.txt","updatedAt":"<time>"}]}
    > request 5 session/list {"cwd":"/elsewhere"}
    < result 5 {"sessions":[]}
    |}];
  ignore
    (H.request
       h
       "session/load"
       (sprintf
          {|{"sessionId": %S, "cwd": %S, "mcpServers": []}|}
          id
          h.sandbox.dir)
     : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 6 session/load {"sessionId":"<id>","cwd":"$DIR","mcpServers":[]}
    < update user_message_chunk {"content":{"type":"text","text":"write b.txt"}}
    < update agent_thought_chunk {"content":{"type":"text","text":"Hmm."}}
    < update agent_message_chunk {"content":{"type":"text","text":"Writing."}}
    < update tool_call {"toolCallId":"c1","title":"Write b.txt","kind":"edit","status":"in_progress","rawInput":{"path":"b.txt","content":"new\n"},"locations":[{"path":"$DIR/b.txt"}]}
    < update tool_call_update {"toolCallId":"c1","status":"completed","content":[{"type":"diff","path":"$DIR/b.txt","oldText":null,"newText":"new\n"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"Wrote b.txt."}}
    < result 6 {"modes":"auto[ask|auto-edit|auto|plan]","configOptions":"mode=auto[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=on[off|on|low|high|max]"}
    < update available_commands_update {"availableCommands":"compact goal remind budget model thinking plan review init btw name diff"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  ignore
    (H.request
       h
       "session/resume"
       (sprintf {|{"sessionId": %S, "cwd": %S}|} id h.sandbox.dir)
     : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 7 session/resume {"sessionId":"<id>","cwd":"$DIR"}
    < result 7 {"modes":"auto[ask|auto-edit|auto|plan]","configOptions":"mode=auto[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=on[off|on|low|high|max]"}
    < update available_commands_update {"availableCommands":"compact goal remind budget model thinking plan review init btw name diff"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}]
;;

let%expect_test "modes and config options" =
  H.run []
  @@ fun h ->
  let set_mode mode =
    ignore
      (H.session_request
         h
         "session/set_mode"
         (sprintf {|{"sessionId": "%%s", "modeId": %S}|} mode)
       : Jsonaf.t);
    H.settle h;
    H.dump h
  in
  let set_option id value =
    ignore
      (H.session_request
         h
         "session/set_config_option"
         (sprintf
            {|{"sessionId": "%%s", "configId": %S, "value": %S}|}
            id
            value)
       : Jsonaf.t);
    H.settle h;
    H.dump h
  in
  set_mode "auto-edit";
  [%expect
    {|
    > request 2 session/set_mode {"sessionId":"<id>","modeId":"auto-edit"}
    < result 2 {}
    < update current_mode_update {"currentModeId":"auto-edit"}
    < update config_option_update {"configOptions":"mode=auto-edit[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=on[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto-edit","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  set_mode "plan";
  [%expect
    {|
    > request 3 session/set_mode {"sessionId":"<id>","modeId":"plan"}
    < result 3 {}
    < update current_mode_update {"currentModeId":"plan"}
    < update config_option_update {"configOptions":"mode=plan[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=on[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto-edit","plan_mode":true,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  set_mode "yolo";
  [%expect
    {|
    > request 4 session/set_mode {"sessionId":"<id>","modeId":"yolo"}
    < error 4 {"code":-32602,"message":"unknown mode \"yolo\": the modes are ask, auto-edit, auto, plan"}
    |}];
  set_option "thinking" "high";
  [%expect
    {|
    > request 5 session/set_config_option {"sessionId":"<id>","configId":"thinking","value":"high"}
    < result 5 {"configOptions":"mode=plan[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=high[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto-edit","plan_mode":true,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  set_option "model" "claude-sonnet-4-5";
  [%expect
    {|
    > request 6 session/set_config_option {"sessionId":"<id>","configId":"model","value":"claude-sonnet-4-5"}
    < result 6 {"configOptions":"mode=plan[ask|auto-edit|auto|plan] model=anthropic/claude-sonnet-4-5[anthropic/claude-sonnet-4-5] thinking=high[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"anthropic/claude-sonnet-4-5","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto-edit","plan_mode":true,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  set_option "mode" "ask";
  [%expect
    {|
    > request 7 session/set_config_option {"sessionId":"<id>","configId":"mode","value":"ask"}
    < result 7 {"configOptions":"mode=ask[ask|auto-edit|auto|plan] model=anthropic/claude-sonnet-4-5[anthropic/claude-sonnet-4-5] thinking=high[off|on|low|high|max]"}
    < update current_mode_update {"currentModeId":"ask"}
    < _prigh/state {"sessionId":"<id>","model":"anthropic/claude-sonnet-4-5","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"ask","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    |}];
  set_option "thinking" "lots";
  [%expect
    {|
    > request 8 session/set_config_option {"sessionId":"<id>","configId":"thinking","value":"lots"}
    < error 8 {"code":-32602,"message":"unknown thinking level \"lots\": the levels are off, on, low, high, max"}
    |}];
  set_option "model" "no-such-model";
  [%expect
    {|
    > request 9 session/set_config_option {"sessionId":"<id>","configId":"model","value":"no-such-model"}
    < error 9 {"code":-32602,"message":"unknown model \"no-such-model\"; did you mean: openai/gpt-5.3-codex (GPT-5.3 Codex), openai/gpt-5.6-sol (GPT-5.6 Sol), openai/gpt-6.1-sol (GPT-6.1 Sol)"}
    |}];
  set_option "colour" "blue";
  [%expect
    {|
    > request 10 session/set_config_option {"sessionId":"<id>","configId":"colour","value":"blue"}
    < error 10 {"code":-32602,"message":"unknown config option \"colour\": prigh's are mode, model, thinking"}
    |}]
;;

let%expect_test "errors say what to do" =
  H.run []
  @@ fun h ->
  let req method_ params = ignore (H.request h method_ params : Jsonaf.t) in
  req "session/prompt" {|{"sessionId": "nope", "prompt": []}|};
  req "session/prompt" {|{"prompt": []}|};
  req "session/new" {|{"mcpServers": []}|};
  req "session/new" {|{"cwd": "relative", "mcpServers": []}|};
  req
    "session/load"
    {|{"sessionId": "0123456789abcdef", "cwd": "/tmp", "mcpServers": []}|};
  req "session/set_mode" {|{"sessionId": "nope", "modeId": "ask"}|};
  req "session/frobnicate" "{}";
  req "session/new" "[1]";
  req "authenticate" {|{"methodId": "prigh-login"}|};
  req "authenticate" {|{"methodId": "oauth"}|};
  ignore
    (H.session_request
       h
       "session/prompt"
       {|{"sessionId": "%s", "prompt": [{"type": "audio", "data": "", "mimeType": "audio/wav"}]}|}
     : Jsonaf.t);
  Eio.Stream.add h.input (Some "not json");
  H.settle h;
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"nope","prompt":[]}
    < error 2 {"code":-32002,"message":"unknown session \"nope\": open one with session/new or session/load (session/list lists them)"}
    > request 3 session/prompt {"prompt":[]}
    < error 3 {"code":-32602,"message":"missing sessionId (a string)"}
    > request 4 session/new {"mcpServers":[]}
    < error 4 {"code":-32602,"message":"missing cwd (an absolute path)"}
    > request 5 session/new {"cwd":"relative","mcpServers":[]}
    < error 5 {"code":-32602,"message":"cwd \"relative\" must be an absolute path"}
    > request 6 session/load {"sessionId":"<id>","cwd":"/tmp","mcpServers":[]}
    < error 6 {"code":-32002,"message":"unknown session \"<id>\": open one with session/new or session/load (session/list lists them)"}
    > request 7 session/set_mode {"sessionId":"nope","modeId":"ask"}
    < error 7 {"code":-32002,"message":"unknown session \"nope\": open one with session/new or session/load (session/list lists them)"}
    > request 8 session/frobnicate {}
    < error 8 {"code":-32601,"message":"prigh does not implement \"session/frobnicate\"; its methods are initialize, authenticate, session/new, session/load, session/resume, session/list, session/close, session/prompt, session/set_mode, session/set_config_option"}
    > request 9 session/new [1]
    < error 9 {"code":-32602,"message":"session/new: params must be an object"}
    > request 10 authenticate {"methodId":"prigh-login"}
    < error 10 {"code":-32000,"message":"prigh is not logged in to any provider: run `prigh login` in a terminal (or /login in the prigh TUI), then try again"}
    > request 11 authenticate {"methodId":"oauth"}
    < error 11 {"code":-32602,"message":"unknown auth method \"oauth\": prigh's is \"prigh-login\""}
    > request 12 session/prompt {"sessionId":"<id>","prompt":[{"type":"audio","data":"","mimeType":"audio/wav"}]}
    < error 12 {"code":-32602,"message":"prompt block 0: prigh does not take audio"}
    < error null {"code":-32700,"message":"invalid JSON: json: unexpected string: 'not'"}
    |}]
;;

let%expect_test
    "a provider error fails the prompt; not logged in asks to log in"
  =
  H.run
    [ {|{"stop_reason": "error", "error": "not logged in to anthropic: use /login anthropic"}|}
    ; {|{"stop_reason": "error", "error": "HTTP 400: the request is malformed"}|}
    ]
  @@ fun h ->
  ignore (H.prompt h "one" : Jsonaf.t);
  ignore (H.prompt h "two" : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"one"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < error 2 {"code":-32000,"message":"not logged in to anthropic: use prigh login anthropic (in a terminal; or /login in the prigh TUI)"}
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"two"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < error 3 {"code":-32603,"message":"HTTP 400: the request is malformed"}
    |}]
;;
