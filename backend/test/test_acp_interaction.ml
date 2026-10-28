open! Core
open! Prigh
module H = Acp_harness

(* [prigh acp] and the editor: questions, the editor's files, prompt
   content, slash commands and prigh's state. *)

let ask_user ?(id = "q1") questions =
  sprintf
    {|{"tool_calls": [{"id": %S, "name": "ask_user", "arguments": {"questions": %s}}]}|}
    id
    questions
;;

let one_choice =
  {|[{"question": "Which colour?", "header": "Colour", "options": [{"label": "red"}, {"label": "blue", "description": "calmer"}]}]|}
;;

let free_text = {|[{"question": "What is the project called?"}]|}

let two_questions =
  {|[{"question": "Which colours?", "header": "Colours", "options": [{"label": "red"}, {"label": "blue"}], "multiple": true}, {"question": "Why?", "header": "Reason"}]|}
;;

let elicitation_caps =
  {|{"elicitation": {"form": {}}, "_meta": {"prigh": {"state": true}}}|}
;;

let quiet_state h =
  Queue.filter_inplace h.H.log ~f:(fun line ->
    not (String.is_substring line ~substring:"_prigh/state"))
;;

let%expect_test "ask_user as an elicitation form" =
  H.run
    ~caps:elicitation_caps
    [ ask_user two_questions
    ; {|{"text": "thanks"}|}
    ; ask_user ~id:"q2" one_choice
    ; {|{"text": "ok"}|}
    ]
  @@ fun h ->
  h.answer
  <- (fun ~method_ ~params:_ ->
       match method_ with
       | "elicitation/create" ->
         Result
           {|{"action": "accept", "content": {"q1": ["red", "blue"], "q1_other": "", "q2": "they are nice"}}|}
       | _ -> H.default_answer ~method_ ~params:());
  ignore (H.prompt h "pick colours" : Jsonaf.t);
  quiet_state h;
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"pick colours"}]}
    < update tool_call {"toolCallId":"q1","title":"Question for you","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"q1","title":"Question for you","kind":"other","status":"in_progress","rawInput":{"questions":[{"question":"Which colours?","header":"Colours","options":[{"label":"red"},{"label":"blue"}],"multiple":true},{"question":"Why?","header":"Reason"}]}}
    < request 0 elicitation/create {"sessionId":"<id>","toolCallId":"q1","mode":"form","message":"1. Colours\n2. Reason","requestedSchema":{"type":"object","properties":{"q1":{"type":"array","title":"Which colours?","items":{"anyOf":[{"const":"red","title":"red"},{"const":"blue","title":"blue"}]}},"q1_other":{"type":"string","title":"Or your own answer"},"q2":{"type":"string","title":"Why?"}},"required":[]}}
    > result 0 {"action":"accept","content":{"q1":["red","blue"],"q1_other":"","q2":"they are nice"}}
    < update tool_call_update {"toolCallId":"q1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"The user answered:\nQ: Which colours?\nA: red; blue\nQ: Why?\nA: they are nice\n"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"thanks"}}
    < result 2 {"stopReason":"end_turn"}
    |}];
  h.answer <- (fun ~method_:_ ~params:_ -> Result {|{"action": "decline"}|});
  ignore (H.prompt h "pick one" : Jsonaf.t);
  quiet_state h;
  H.dump h;
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"pick one"}]}
    < update tool_call {"toolCallId":"q2","title":"Question for you","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"q2","title":"Question for you","kind":"other","status":"in_progress","rawInput":{"questions":[{"question":"Which colour?","header":"Colour","options":[{"label":"red"},{"label":"blue","description":"calmer"}]}]}}
    < request 1 elicitation/create {"sessionId":"<id>","toolCallId":"q2","mode":"form","message":"Which colour?","requestedSchema":{"type":"object","properties":{"q1":{"type":"string","title":"Which colour?","oneOf":[{"const":"red","title":"red"},{"const":"blue","title":"blue: calmer"}]},"q1_other":{"type":"string","title":"Or your own answer"}},"required":[]}}
    > result 1 {"action":"decline"}
    < update tool_call_update {"toolCallId":"q2","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[the user declined to answer; carry on with your best judgement or ask differently]"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"ok"}}
    < result 3 {"stopReason":"end_turn"}
    |}]
;;

let%expect_test
    "ask_user without forms: one choice as a permission request, else \
     unavailable"
  =
  H.run
    [ ask_user one_choice
    ; {|{"text": "blue it is"}|}
    ; ask_user ~id:"q2" one_choice
    ; {|{"text": "fine"}|}
    ; ask_user ~id:"q3" free_text
    ; {|{"text": "I will decide"}|}
    ]
  @@ fun h ->
  let pick option =
    h.answer
    <- (fun ~method_ ~params:_ ->
         match method_ with
         | "session/request_permission" ->
           Result
             (sprintf
                {|{"outcome": {"outcome": "selected", "optionId": %S}}|}
                option)
         | _ -> H.default_answer ~method_ ~params:())
  in
  pick "choice-1";
  ignore (H.prompt h "pick" : Jsonaf.t);
  quiet_state h;
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"pick"}]}
    < update tool_call {"toolCallId":"q1","title":"Question for you","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"q1","title":"Question for you","kind":"other","status":"in_progress","rawInput":{"questions":[{"question":"Which colour?","header":"Colour","options":[{"label":"red"},{"label":"blue","description":"calmer"}]}]}}
    < request 0 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"q1","title":"Which colour?","kind":"other","status":"pending"},"options":[{"optionId":"choice-0","name":"red","kind":"allow_once"},{"optionId":"choice-1","name":"blue: calmer","kind":"allow_once"},{"optionId":"decline","name":"Decline to answer","kind":"reject_once"}]}
    > result 0 {"outcome":{"outcome":"selected","optionId":"choice-1"}}
    < update tool_call_update {"toolCallId":"q1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"The user answered:\nQ: Which colour?\nA: blue\n"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"blue it is"}}
    < result 2 {"stopReason":"end_turn"}
    |}];
  pick "decline";
  ignore (H.prompt h "pick again" : Jsonaf.t);
  quiet_state h;
  H.dump h;
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"pick again"}]}
    < update tool_call {"toolCallId":"q2","title":"Question for you","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"q2","title":"Question for you","kind":"other","status":"in_progress","rawInput":{"questions":[{"question":"Which colour?","header":"Colour","options":[{"label":"red"},{"label":"blue","description":"calmer"}]}]}}
    < request 1 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"q2","title":"Which colour?","kind":"other","status":"pending"},"options":[{"optionId":"choice-0","name":"red","kind":"allow_once"},{"optionId":"choice-1","name":"blue: calmer","kind":"allow_once"},{"optionId":"decline","name":"Decline to answer","kind":"reject_once"}]}
    > result 1 {"outcome":{"outcome":"selected","optionId":"decline"}}
    < update tool_call_update {"toolCallId":"q2","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[the user declined to answer; carry on with your best judgement or ask differently]"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"fine"}}
    < result 3 {"stopReason":"end_turn"}
    |}];
  ignore (H.prompt h "name it" : Jsonaf.t);
  quiet_state h;
  H.dump h;
  [%expect
    {|
    > request 4 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"name it"}]}
    < update tool_call {"toolCallId":"q3","title":"Question for you","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"q3","title":"Question for you","kind":"other","status":"in_progress","rawInput":{"questions":[{"question":"What is the project called?"}]}}
    < update tool_call_update {"toolCallId":"q3","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[cannot ask: test-editor cannot show these questions (it has no forms): decide yourself, or ask them in your reply]"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"I will decide"}}
    < result 4 {"stopReason":"end_turn"}
    |}]
;;

let fs_caps =
  {|{"fs": {"readTextFile": true, "writeTextFile": true}, "_meta": {"prigh": {"state": true}}}|}
;;

let%expect_test
    "the editor's files: reads and writes go through fs/*, the rest uses the \
     disk"
  =
  H.run
    ~caps:fs_caps
    [ {|{"tool_calls": [{"id": "c1", "name": "read", "arguments": {"path": "a.txt"}}, {"id": "c2", "name": "grep", "arguments": {"pattern": "saved", "path": "a.txt"}}]}|}
    ; {|{"tool_calls": [{"id": "c3", "name": "edit", "arguments": {"path": "a.txt", "edits": [{"old_text": "unsaved", "new_text": "edited"}]}}, {"id": "c4", "name": "write", "arguments": {"path": "sub/new.txt", "content": "fresh\n"}}]}|}
    ; {|{"text": "done"}|}
    ; {|{"tool_calls": [{"id": "c5", "name": "read", "arguments": {"path": "a.txt"}}]}|}
    ; {|{"text": "read the disk"}|}
    ]
  @@ fun h ->
  Tool_test_helpers.write h.sandbox "a.txt" "saved on disk\n";
  h.answer
  <- (fun ~method_ ~params:_ ->
       match method_ with
       | "fs/read_text_file" ->
         Result {|{"content": "unsaved in the editor\n"}|}
       | "fs/write_text_file" -> Result "null"
       | _ -> H.default_answer ~method_ ~params:());
  ignore (H.prompt h "change a.txt" : Jsonaf.t);
  quiet_state h;
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"change a.txt"}]}
    < update tool_call {"toolCallId":"c1","title":"Read","kind":"read","status":"pending"}
    < update tool_call {"toolCallId":"c2","title":"grep ","kind":"search","status":"pending"}
    < request 0 fs/read_text_file {"sessionId":"<id>","path":"$DIR/a.txt"}
    > result 0 {"content":"unsaved in the editor\n"}
    < update tool_call_update {"toolCallId":"c1","title":"Read a.txt","kind":"read","status":"in_progress","rawInput":{"path":"a.txt"},"locations":[{"path":"$DIR/a.txt"}]}
    < update tool_call_update {"toolCallId":"c2","title":"grep \"saved\" in a.txt","kind":"search","status":"in_progress","rawInput":{"pattern":"saved","path":"a.txt"}}
    < update tool_call_update {"toolCallId":"c1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"unsaved in the editor\n"}}]}
    < update tool_call_update {"toolCallId":"c2","status":"completed","content":[{"type":"content","content":{"type":"text","text":"```\na.txt:1:saved on disk\n```"}}]}
    < update tool_call {"toolCallId":"c3","title":"Edit","kind":"edit","status":"pending"}
    < update tool_call {"toolCallId":"c4","title":"Write","kind":"edit","status":"pending"}
    < request 1 fs/read_text_file {"sessionId":"<id>","path":"$DIR/a.txt"}
    > result 1 {"content":"unsaved in the editor\n"}
    < update tool_call_update {"toolCallId":"c3","title":"Edit a.txt","kind":"edit","status":"in_progress","rawInput":{"path":"a.txt","edits":[{"old_text":"unsaved","new_text":"edited"}]},"locations":[{"path":"$DIR/a.txt"}]}
    < request 2 fs/write_text_file {"sessionId":"<id>","path":"$DIR/a.txt","content":"edited in the editor\n"}
    > result 2 null
    < request 3 fs/write_text_file {"sessionId":"<id>","path":"$DIR/sub/new.txt","content":"fresh\n"}
    > result 3 null
    < update tool_call_update {"toolCallId":"c3","status":"completed","content":[{"type":"diff","path":"$DIR/a.txt","oldText":"unsaved in the editor\n","newText":"edited in the editor\n"}]}
    < update tool_call_update {"toolCallId":"c4","title":"Write sub/new.txt","kind":"edit","status":"in_progress","rawInput":{"path":"sub/new.txt","content":"fresh\n"},"locations":[{"path":"$DIR/sub/new.txt"}]}
    < update tool_call_update {"toolCallId":"c4","status":"completed","content":[{"type":"diff","path":"$DIR/sub/new.txt","oldText":null,"newText":"fresh\n"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"done"}}
    < result 2 {"stopReason":"end_turn"}
    |}];
  print_endline (Tool_test_helpers.read h.sandbox "a.txt");
  print_s
    [%sexp
      (Sys_unix.file_exists_exn (Filename.concat h.sandbox.dir "sub/new.txt")
       : bool)];
  [%expect
    {|
    saved on disk

    false
    |}];
  h.answer <- (fun ~method_:_ ~params:_ -> Error (-32603, "the buffer is gone"));
  ignore (H.prompt h "read again" : Jsonaf.t);
  quiet_state h;
  H.dump h;
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"read again"}]}
    < update tool_call {"toolCallId":"c5","title":"Read","kind":"read","status":"pending"}
    < request 4 fs/read_text_file {"sessionId":"<id>","path":"$DIR/a.txt"}
    > error 4 {"code":-32603,"message":"the buffer is gone"}
    < update tool_call_update {"toolCallId":"c5","title":"Read a.txt","kind":"read","status":"in_progress","rawInput":{"path":"a.txt"},"locations":[{"path":"$DIR/a.txt"}]}
    log: fs/read_text_file $DIR/a.txt failed (the buffer is gone): reading the disk
    < update tool_call_update {"toolCallId":"c5","status":"completed","content":[{"type":"content","content":{"type":"text","text":"saved on disk\n"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"read the disk"}}
    < result 3 {"stopReason":"end_turn"}
    |}]
;;

let%expect_test "prompt content: links, embedded files, selections, images" =
  H.run
    ~caps:fs_caps
    [ {|{"text": "one"}|}; {|{"text": "two"}|}; {|{"text": "three"}|} ]
  @@ fun h ->
  Tool_test_helpers.write h.sandbox "a.txt" "on disk\n";
  h.answer
  <- (fun ~method_ ~params:_ ->
       match method_ with
       | "fs/read_text_file" ->
         Result {|{"content": "one\ntwo (unsaved)\nthree\n"}|}
       | _ -> H.default_answer ~method_ ~params:());
  let prompt blocks =
    ignore
      (H.session_request
         h
         "session/prompt"
         (sprintf {|{"sessionId": "%%s", "prompt": %s}|} blocks)
       : Jsonaf.t);
    quiet_state h;
    H.dump h;
    H.print_prompts h
  in
  let dir = h.sandbox.dir in
  prompt
    (sprintf
       {|[{"type": "text", "text": "look at"}, {"type": "resource_link", "name": "a.txt", "uri": "file://%s/a.txt"}, {"type": "text", "text": "and"}, {"type": "resource_link", "name": "docs", "uri": "https://example.com/docs"}]|}
       dir);
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"look at"},{"type":"resource_link","name":"a.txt","uri":"file://$DIR/a.txt"},{"type":"text","text":"and"},{"type":"resource_link","name":"docs","uri":"https://example.com/docs"}]}
    < request 0 fs/read_text_file {"sessionId":"<id>","path":"$DIR/a.txt"}
    > result 0 {"content":"one\ntwo (unsaved)\nthree\n"}
    < update agent_message_chunk {"content":{"type":"text","text":"one"}}
    < result 2 {"stopReason":"end_turn"}
    model got: look at @$DIR/a.txt and [docs](https://example.com/docs)

    <file path="$DIR/a.txt">
    one
    two (unsaved)
    three
    </file>
    |}];
  prompt
    (sprintf
       {|[{"type": "text", "text": "explain"}, {"type": "resource", "resource": {"uri": "file://%s/a.txt#L2-L3", "mimeType": "text/plain", "text": "two (unsaved)\nthree\n"}}]|}
       dir);
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"explain"},{"type":"resource","resource":{"uri":"file://$DIR/a.txt#L2-L3","mimeType":"text/plain","text":"two (unsaved)\nthree\n"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"two"}}
    < result 3 {"stopReason":"end_turn"}
    model got: explain @$DIR/a.txt (lines 2-3)

    (The line ranges are the user's selection.)

    <file path="$DIR/a.txt">
    two (unsaved)
    three
    </file>
    |}];
  prompt
    (sprintf
       {|[{"type": "text", "text": "what is this?"}, {"type": "image", "mimeType": "image/png", "data": %S}]|}
       Image_fixtures.png_3x2);
  [%expect
    {|
    > request 4 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"what is this?"},{"type":"image","mimeType":"image/png","data":"iVBORw0KGgoAAAANSUhEUgAAAAMAAAACAQMAAACnuvRZAAAAA1BMVEX/AAAZ4gk3AAAADElEQVQI12NgYGAAAAAEAAEnNCcKAAAAAElFTkSuQmCC"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"three"}}
    < result 4 {"stopReason":"end_turn"}
    model got: what is this?
    (1 image(s))
    |}]
;;

let%expect_test "slash commands run prigh's commands" =
  H.run
    [ {|{"text": "a side answer"}|}
    ; {|{"text": "done"}|}
    ; {|{"text": "checked the tests"}|}
    ; {|{"text": "echoed"}|}
    ]
  @@ fun h ->
  let run text =
    ignore (H.prompt h text : Jsonaf.t);
    H.dump h
  in
  run "/budget";
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/budget"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"no budget ($0.00 spent); /budget DOLLARS sets one"}}
    < result 2 {"stopReason":"end_turn"}
    |}];
  run "/budget 5";
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/budget 5"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update agent_message_chunk {"content":{"type":"text","text":"budget set to $5.00"}}
    < result 3 {"stopReason":"end_turn"}
    |}];
  run "/budget lots";
  [%expect
    {|
    > request 4 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/budget lots"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"usage: /budget DOLLARS (a positive number) or /budget off"}}
    < result 4 {"stopReason":"end_turn"}
    |}];
  run "/name Feature X";
  [%expect
    {|
    > request 5 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/name Feature X"}]}
    < update session_info_update {"title":"Feature X"}
    < update agent_message_chunk {"content":{"type":"text","text":"session named Feature X"}}
    < result 5 {"stopReason":"end_turn"}
    |}];
  run "/thinking high";
  [%expect
    {|
    > request 6 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/thinking high"}]}
    < update config_option_update {"configOptions":"mode=auto[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=high[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update agent_message_chunk {"content":{"type":"text","text":"thinking set to high"}}
    < result 6 {"stopReason":"end_turn"}
    |}];
  run "/goal";
  [%expect
    {|
    > request 7 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/goal"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"no goal; /goal TEXT sets one"}}
    < result 7 {"stopReason":"end_turn"}
    |}];
  run "/btw what is 2+2?";
  [%expect
    {|
    > request 8 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/btw what is 2+2?"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"a side answer"}}
    < result 8 {"stopReason":"end_turn"}
    |}];
  run "/remind stop 1 check the tests";
  [%expect
    {|
    > request 9 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/remind stop 1 check the tests"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":1,"active_host":"testhost"}
    < update agent_message_chunk {"content":{"type":"text","text":"reminder added: r1 · on stop (0/1) · check the tests"}}
    < result 9 {"stopReason":"end_turn"}
    |}];
  run "go";
  [%expect
    {|
    > request 10 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"go"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":1,"active_host":"testhost"}
    < update agent_message_chunk {"content":{"type":"text","text":"done"}}
    < update user_message_chunk {"content":{"type":"text","text":"Reminder r1: check the tests (1/1)"}}
    < update agent_message_chunk {"content":{"type":"text","text":"checked the tests"}}
    < _prigh/notice {"sessionId":"<id>","text":"reminder r1 fired once; it fires again after your next message"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":1,"active_host":"testhost"}
    < result 10 {"stopReason":"end_turn"}
    |}];
  run "/remind";
  [%expect
    {|
    > request 11 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/remind"}]}
    < update agent_message_chunk {"content":{"type":"text","text":"r1 · on stop (1/1) · check the tests"}}
    < result 11 {"stopReason":"end_turn"}
    |}];
  run "/plan";
  [%expect
    {|
    > request 12 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/plan"}]}
    < update current_mode_update {"currentModeId":"plan"}
    < update config_option_update {"configOptions":"mode=plan[ask|auto-edit|auto|plan] model=deepseek/deepseek-flash[deepseek/deepseek-flash] thinking=high[off|on|low|high|max]"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":true,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":1,"active_host":"testhost"}
    < update agent_message_chunk {"content":{"type":"text","text":"plan mode on"}}
    < result 12 {"stopReason":"end_turn"}
    |}];
  run "/unknown thing";
  [%expect
    {|
    > request 13 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"/unknown thing"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":true,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":1,"active_host":"testhost"}
    < update agent_message_chunk {"content":{"type":"text","text":"echoed"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"high","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":5,"approval_mode":"auto","plan_mode":true,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":1,"active_host":"testhost"}
    < result 13 {"stopReason":"end_turn"}
    |}];
  H.print_prompts h;
  [%expect
    {|
    model got: <btw>The user asks a side question (/btw) while you may be in the middle of a task. Answer it briefly from the conversation so far. You cannot use tools in this reply, and neither the question nor your answer will be added to the conversation.</btw>

    what is 2+2?
    model got: go
    model got: [prigh reminder: r1, as you ended your turn (1 of 1)] check the tests
    model got: [Plan mode is on: you are planning, not implementing. Investigate with read-only tools (bash only for read-only commands; the user confirms each one), ask the user about anything unclear with ask_user, and do not change any files: edits are refused. Keep the steps in update_plan. When the plan is ready, call exit_plan_mode with it; the user approves it before you implement it.]

    /unknown thing
    |}]
;;

let%expect_test "_prigh/state follows the queue another frontend fills" =
  H.run
    [ {|{"tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "echo started; sleep 0.5"}}]}|}
    ; {|{"text": "first"}|}
    ; {|{"text": "queued one answered"}|}
    ]
  @@ fun h ->
  ignore (H.prompt ~wait:false h "work" : Jsonaf.t);
  H.wait_for h "started";
  let tui = H.other_client h in
  tui "follow_up" {|{"text": "and then this"}|};
  H.wait_for h "stopReason";
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"work"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call {"toolCallId":"c1","title":"bash","kind":"execute","status":"pending"}
    < update tool_call_update {"toolCallId":"c1","title":"echo started; sleep 0.5","kind":"execute","status":"in_progress","rawInput":{"command":"echo started; sleep 0.5"}}
    < update tool_call_update {"toolCallId":"c1","content":[{"type":"content","content":{"type":"text","text":"```\nstarted\n```"}}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":1},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update tool_call_update {"toolCallId":"c1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"```\nstarted\n```"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"first"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < update user_message_chunk {"content":{"type":"text","text":"and then this"}}
    < update agent_message_chunk {"content":{"type":"text","text":"queued one answered"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"on","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"testhost"}
    < result 2 {"stopReason":"end_turn"}
    |}]
;;
