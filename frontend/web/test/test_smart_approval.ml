open! Core
open Prigh_web
open Agent_events
module H = Harness

(* The smart approval mode: its place among the modes, the approval model
   (Settings → Models → Approvals, /approval-model), the note on a call it
   let run and its reason on a call it left to the user. *)

let config ?model () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; ( "approval_model"
          , Option.value_map model ~default:`Null ~f:(fun m -> `String m) )
        ])
;;

let state mode =
  H.state_json ~fields:[ "approval_mode", `String mode; "plan_mode", `False ] ()
;;

let%expect_test "the mode chip in smart mode, and Alt+M on from it" =
  let h = H.create ~state:(state "auto-edit") () in
  H.key h "m" ~alt:true;
  H.reply h "set_mode" (state "smart");
  H.quiet h (fun () -> H.click h ~selector:".model-chip");
  H.text h ~selector:".model-access-button";
  H.quiet h (fun () -> H.click h ~selector:".model-access-button");
  H.text h ~selector:".model-access-menu";
  [%expect
    {|
    Cycle_mode
    (Rpc (method_ set_mode)
     (params ((approval_mode smart) (plan_mode false) (chat_mode false)))
     (tag
      (Mode_set
       "Mode: smart (edits in the project run; the approval model lets other calls run or asks you, saying why (/approval-model))")))
    (Expire_toast (id 0) (after_ms 6000))
    (Open_chip Model)
    (Smart)
    Model_menu_access
    (Ask first Asks: edits & commands) (Auto-edit Asks: commands) (Smart Smart: a model decides ✓) (Full access Auto: never asks) (Plan Plan mode) (Chat Chat: no tool host)
    |}];
  H.quiet h (fun () -> H.key h "Escape" ~target:Field);
  H.quiet h (fun () -> H.key h "Escape" ~target:Field);
  H.key h "m" ~alt:true;
  [%expect
    {|
    Close_dialog
    Close_dialog
    Cycle_mode
    (Rpc (method_ set_mode)
     (params ((approval_mode auto) (plan_mode false) (chat_mode false)))
     (tag (Mode_set "Mode: auto (tools run without asking)")))
    |}]
;;

let settings () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h Open_settings;
    H.act h (Settings_section Models));
  h
;;

let%expect_test
    "Settings → Models → Approvals: shown, picked, back to the default"
  =
  let h = settings () in
  H.text h ~selector:".setting.approval-model";
  H.names h ~selector:".setting.approval-model button";
  H.click h ~selector:".approval-model-choice";
  H.text h ~selector:".modal";
  [%expect
    {|
    Approvals
    In smart mode, lets a tool call run or asks you, saying why. Default: the session's model.
    (Default)
    button "Approval model: Default" -- Choose the model (/approval-model)
    (Run /approval-model)
    (Focus picker-input)
    Approval model
    (Close (Esc))
    []
    Default the session's model ✓
    Claude Opus 5.5 anthropic, 200k ctx
    Claude Sonnet 5 anthropic, 200k ctx
    GPT-6 openai, 200k ctx, not logged in
    DeepSeek Chat deepseek, 200k ctx, not logged in
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.quiet h (fun () -> H.act h (Picker_query "sonnet"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config ~model:"anthropic/claude-sonnet-5" ());
  H.text h ~selector:".setting.approval-model";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config)
     (params ((config ((approval_model anthropic/claude-sonnet-5)))))
     (tag Approval_model_saved))
    (Expire_toast (id 0) (after_ms 5700))
    Approvals
    In smart mode, lets a tool call run or asks you, saying why. Default: the session's model.
    (Claude Sonnet 5)
    In smart mode, anthropic/claude-sonnet-5 judges the tool calls that would ask (/approval-model MODEL|off)
    |}];
  H.quiet h (fun () -> H.click h ~selector:".approval-model-choice");
  H.quiet h (fun () -> H.act h (Picker_query "default"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config ());
  H.text h ~selector:".setting.approval-model";
  [%expect
    {|
    (Run /approval-model)
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ set_config) (params ((config ((approval_model null)))))
     (tag Approval_model_saved))
    (Expire_toast (id 1) (after_ms 5460))
    Approvals
    In smart mode, lets a tool call run or asks you, saying why. Default: the session's model.
    (Default)
    |}]
;;

let%expect_test
    "/approval-model from the composer, and a model the backend refuses"
  =
  let h = H.create () in
  H.type_ h "/approval-model ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Approval model ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    off the session's model judges them
    openai/gpt-6 GPT-6
    anthropic/claude-opus-5-5 Claude Opus 5.5
    anthropic/claude-sonnet-5 Claude Sonnet 5
    deepseek/deepseek-chat DeepSeek Chat
    |}];
  H.quiet h (fun () ->
    H.type_ h "/approval-model nosuch";
    H.act h Send);
  H.fail
    h
    "set_config"
    {|approval_model: unknown model "nosuch"; did you mean: anthropic/claude-sonnet-5|};
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ get_config) (params ()) (tag Config))
    Couldn't set the approval model: approval_model: unknown model "nosuch"; did you mean: anthropic/claude-sonnet-5. Pick one with /approval-model (Dismiss)
    |}]
;;

let allowed ?agent id reason =
  sprintf
    {|{"event":"tool_allowed","call_id":"%s","agent_id":%s,"model":"anthropic/claude-haiku-4-5","reason":%s}|}
    id
    (Option.value_map agent ~default:"null" ~f:(sprintf "%S"))
    (Jsonaf.to_string (`String reason))
;;

let%expect_test
    "a call the approval model let run: a chip, its note in the card"
  =
  let chat =
    Chat_harness.chat
      [ tool_start ~id:"c1" ~name:"bash" {|{"command":"make test"}|}
      ; allowed "c1" "runs the project's tests"
      ; tool_end ~id:"c1" ~name:"bash" "ok"
      ]
  in
  Chat_harness.show chat ~selector:".tool-bash .tool-head";
  Chat_harness.text
    chat
    ~fold:
      (Fold.create
         ~verbosity:Normal
         ~toggled:(String.Set.singleton "c1#open")
         ())
    ~selector:".tool-bash .tool-body";
  [%expect
    {|
    <button data-slot="collapsible-trigger"
            type="button"
            data-fold="c1#open"
            aria-expanded="true"
            class="tool-head">
      <span title="Done" role="img" aria-label="Done" class="done mark">
        <icon class="check"> </icon>
      </span>
      <span aria-hidden="true" class="tool-icon">
        <icon class="terminal"> </icon>
      </span>
      <span class="name"> bash </span>
      <span class="arg command"> make test </span>
      <span title="allowed by anthropic/claude-haiku-4-5: runs the project's tests"
            class="chip judged"> judged </span>
      <span class="chev">  </span>
    </button>
    allowed by anthropic/claude-haiku-4-5: runs the project's tests ok
    |}]
;;

let%expect_test "a subagent's allowed call: the note goes to its transcript" =
  let events =
    [ call ~id:"s1" ~name:"subagent" {|{"task":"Build"}|}
    ; tool_start ~id:"s1" ~name:"subagent" {|{"task":"Build"}|}
    ; start ~call:"s1" ~agent:"a1" "Build"
    ; inner
        ~call:"s1"
        ~agent:"a1"
        (tool_start ~id:"c1" ~name:"bash" {|{"command":"make"}|})
    ; allowed ~agent:"a1" "c1" "builds the project"
    ; allowed "c1" "not this one: the main agent has no c1"
    ]
  in
  let chat = Chat_harness.chat ~running:true events in
  let note chat id = Option.bind (Chat.tool chat id) ~f:(fun t -> t.allowed) in
  let sub =
    Option.bind (Chat.tool chat "s1") ~f:(fun t -> t.subagent)
    |> Option.value_exn
  in
  print_s
    [%message
      ""
        ~main:(note chat "c1" : string option)
        ~subagent:(note sub.chat "c1" : string option)];
  [%expect
    {|
    ((main ())
     (subagent ("allowed by anthropic/claude-haiku-4-5: builds the project")))
    |}]
;;

let%expect_test
    "a call the approval model left to the user: its reason on the card"
  =
  let h = H.create ~state:(state "smart") () in
  H.event
    h
    {|{"event":"tool_confirm","call_id":"c1","agent_id":null,"name":"bash","summary":"rm -rf ~/build","rules":["bash(rm:*)"],"kind":"judged","model":"anthropic/claude-haiku-4-5","reason":"deletes a directory outside the project"}|};
  H.text h ~selector:".modal";
  H.names h ~selector:".confirm-dialog button";
  [%expect
    {|
    (Focus confirm)
    Confirm (smart)
    (Close (Esc))
    Run bash: rm -rf ~/build?
    anthropic/claude-haiku-4-5 left this to you: deletes a directory outside the project
    (y Allow once) (a Always allow bash(rm:*) this session) (n Deny) (r Deny, saying why)
    button "Close" -- Close (Esc)
    button "Allow once" -- y or Enter
    button "Always allow bash(rm:*) this session" -- a
    button "Deny" -- n or Esc
    button "Deny, saying why" -- r
    |}]
;;
