open! Core
open! Prigh
open! Test_acp

let%expect_test
    "computer use: one permission request for a period of control, in any \
     mode, without always"
  =
  let click id =
    sprintf
      {|{"tool_calls": [{"id": %S, "name": "computer", "arguments": {"action": "click", "x": 10, "y": 20}}]}|}
      id
  in
  H.run
    [ click "c1"
    ; click "c2"
    ; {|{"text": "clicked"}|}
    ; click "c3"
    ; {|{"text": "declined"}|}
    ]
  @@ fun h ->
  Eio.Switch.run
  @@ fun sw ->
  let fake = computer_host h ~sw in
  (H.other_client h) "set_active_host" {|{"host": "host-desk"}|};
  Queue.clear h.log;
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
  ignore (H.prompt h "click twice" : Jsonaf.t);
  H.dump h;
  [%expect
    {|
    > request 2 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"click twice"}]}
    < _prigh/notice {"sessionId":"<id>","text":"tools now run on desk in $DIR"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"default","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"chat_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"desk"}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"default","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"chat_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"desk"}
    < update tool_call {"toolCallId":"c1","title":"computer","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"c1","title":"computer","kind":"other","status":"in_progress","rawInput":{"action":"click","x":10,"y":20}}
    < request 0 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"c1","title":"Let prigh control desk's screen, mouse and keyboard?","kind":"other","status":"pending","rawInput":{"action":"click","x":10,"y":20}},"options":[{"optionId":"allow","name":"Allow until control ends","kind":"allow_once"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    > result 0 {"outcome":{"outcome":"selected","optionId":"allow"}}
    < update tool_call_update {"toolCallId":"c1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"click (10, 20) → (20, 40) on the screen\ncursor at (10, 20)\nscreenshot 1280×800"}}]}
    < update tool_call {"toolCallId":"c2","title":"computer","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"c2","title":"computer","kind":"other","status":"in_progress","rawInput":{"action":"click","x":10,"y":20}}
    < update tool_call_update {"toolCallId":"c2","status":"completed","content":[{"type":"content","content":{"type":"text","text":"click (10, 20) → (20, 40) on the screen\ncursor at (10, 20)\nscreenshot 1280×800"}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"clicked"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"default","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"chat_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"desk"}
    < result 2 {"stopReason":"end_turn"}
    |}];
  h.answer <- pick "reject";
  ignore (H.prompt h "once more" : Jsonaf.t);
  H.dump h;
  List.iter (Computer_fake.log fake) ~f:print_endline;
  [%expect
    {|
    > request 3 session/prompt {"sessionId":"<id>","prompt":[{"type":"text","text":"once more"}]}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"default","running":true,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"chat_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"desk"}
    < update tool_call {"toolCallId":"c3","title":"computer","kind":"other","status":"pending"}
    < update tool_call_update {"toolCallId":"c3","title":"computer","kind":"other","status":"in_progress","rawInput":{"action":"click","x":10,"y":20}}
    < request 1 session/request_permission {"sessionId":"<id>","toolCall":{"toolCallId":"c3","title":"Let prigh control desk's screen, mouse and keyboard?","kind":"other","status":"pending","rawInput":{"action":"click","x":10,"y":20}},"options":[{"optionId":"allow","name":"Allow until control ends","kind":"allow_once"},{"optionId":"reject","name":"Reject","kind":"reject_once"}]}
    > result 1 {"outcome":{"outcome":"selected","optionId":"reject"}}
    < update tool_call_update {"toolCallId":"c3","status":"failed","content":[{"type":"content","content":{"type":"text","text":"[computer use declined] The user did not allow prigh to control desk's screen. Do not use the computer tool again unless the user asks you to."}}]}
    < update agent_message_chunk {"content":{"type":"text","text":"declined"}}
    < _prigh/state {"sessionId":"<id>","model":"deepseek/deepseek-flash","thinking":"default","running":false,"context_tokens":0,"context_window":1000000,"cost_usd":0,"budget_usd":null,"approval_mode":"auto","plan_mode":false,"chat_mode":false,"queued":{"steer":0,"follow_up":0},"subagents":[],"jobs":0,"goal":null,"reminders":0,"active_host":"desk"}
    < result 3 {"stopReason":"end_turn"}
    move 20,40
    click left×1
    move 20,40
    click left×1
    |}]
;;
