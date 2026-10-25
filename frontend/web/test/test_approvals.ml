open! Core
module H = Harness

(* Approval modes, plan mode, the plan, tool confirmations with rules and
   reasons, and the model's questions. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let status h =
  H.text h ~selector:".status";
  H.text h ~selector:".status-extras"
;;

let modal h = H.text h ~selector:".modal"
let toasts h = H.text h ~selector:".toast"

let state ?(mode = "auto") ?(plan_mode = false) ?plan ?(rules = []) () =
  H.state_json
    ~fields:
      ([ "approval_mode", `String mode
       ; ("plan_mode", if plan_mode then `True else `False)
       ; "session_rules", `Array (List.map rules ~f:(fun r -> `String r))
       ]
       @ Option.value_map plan ~default:[] ~f:(fun p ->
         [ "plan", Jsonaf.of_string p ]))
    ()
;;

let plan =
  {|{"explanation":"first pass","steps":[{"step":"read the code","status":"completed"},{"step":"change it","status":"in_progress"},{"step":"test","status":"pending"}]}|}
;;

let mode_chip h = H.text h ~selector:".mode-chip"

let%expect_test "the mode chip, Alt+M, and set_mode's reply or failure" =
  let h = H.create () in
  (* Always shown beside the model, [auto] too, saying what asks first. *)
  H.show h ~selector:".mode-chip";
  [%expect
    {|
    <button type="button"
            title="Tools run without asking (Alt+M cycles, /mode picks)"
            data-short="auto"
            class="btn chip-button mode-chip"
            @on_click>
      <icon class="shield"> </icon>
      <span class="chip-label"> Auto: never asks </span>
      <icon class="chevron"> </icon>
    </button>
    |}];
  (* Shown at once, waiting on the backend. *)
  H.key h "m" ~alt:true;
  H.show h ~selector:".mode-chip";
  [%expect
    {|
    Cycle_mode
    (Rpc (method_ set_mode) (params ((approval_mode auto) (plan_mode true)))
     (tag
      (Mode_set
       "Mode: plan (read-only planning: no file edits; bash, job_input and MCP tools ask first)")))
    <button type="button"
            title="Switching to plan…"
            disabled=""
            aria-busy="true"
            data-short="plan"
            class="awaiting btn chip-button mode-chip"
            @on_click>
      <span aria-hidden="true" class="lamp"> </span>
      <span class="chip-label"> Plan mode </span>
    </button>
    |}];
  H.reply h "set_mode" (state ~mode:"auto" ~plan_mode:true ());
  H.show h ~selector:".mode-chip";
  toasts h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4980))
    <button type="button"
            title="Read-only planning: no file edits; bash, job_input and MCP tools ask first (Alt+M cycles, /mode picks)"
            data-short="plan"
            class="btn chip-button mode-chip plan-mode"
            @on_click>
      <icon class="shield"> </icon>
      <span class="chip-label"> Plan mode </span>
      <icon class="chevron"> </icon>
    </button>
    Mode: plan (read-only planning: no file edits; bash, job_input and MCP tools ask first)
    |}];
  (* Option+M types a symbol on a Mac: the physical key counts. *)
  H.key h "µ" ~alt:true ~code:"KeyM";
  mode_chip h;
  toasts h;
  [%expect
    {|
    Cycle_mode
    (Rpc (method_ set_mode) (params ((approval_mode ask) (plan_mode false)))
     (tag
      (Mode_set "Mode: ask (bash, job_input, edits and MCP tools ask first)")))
    (Asks: edits & commands)
    Mode: plan (read-only planning: no file edits; bash, job_input and MCP tools ask first)
    |}];
  (* Not taken: said, and the backend's state shown again. *)
  H.fail h "set_mode" "no session";
  H.reply h "get_state" (state ~mode:"auto" ~plan_mode:true ());
  mode_chip h;
  toasts h;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag State))
    (Plan mode)
    Mode: plan (read-only planning: no file edits; bash, job_input and MCP tools ask first)
    Couldn't set the mode: no session (Dismiss)
    |}];
  (* Clicking the chip picks the mode: what each asks first. *)
  H.click h ~selector:".mode-chip";
  modal h;
  [%expect
    {|
    (Run /mode)
    (Focus picker-input)
    When tools ask first (Alt+M cycles)
    (Close (Esc))
    []
    Asks: edits & commands bash, job_input, edits and MCP tools ask first
    Asks: commands file edits run; bash, job_input and MCP tools ask first
    Auto: never asks tools run without asking
    Plan mode read-only planning: no file edits; bash, job_input and MCP tools ask first ✓
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  (* The filter knows the short names too. *)
  H.act h (Picker_query "auto-e");
  H.text h ~selector:".picker-items";
  [%expect
    {| Asks: commands file edits run; bash, job_input and MCP tools ask first |}];
  H.key h "Enter" ~target:Field;
  mode_chip h;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_mode)
     (params ((approval_mode auto-edit) (plan_mode false)))
     (tag
      (Mode_set
       "Mode: auto-edit (file edits run; bash, job_input and MCP tools ask first)")))
    (Asks: commands)
    |}];
  (* The status line leaves the mode to the chip. *)
  status h;
  [%expect {| (0%) ↑0 ↓0 $0.0000 Ready |}]
;;

let%expect_test "/mode NAME, /plan, /confirm" =
  let h = H.create ~state:(state ~mode:"ask" ()) () in
  mode_chip h;
  [%expect {| (Asks: edits & commands) |}];
  run h "/mode bogus";
  toasts h;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/mode bogus") (max 500))) (tag Ignore))
    Unknown mode “bogus”: use ask, auto-edit, auto, plan (/mode picks one). (Dismiss)
    "/mode bogus"
    |}];
  run h "/mode auto";
  mode_chip h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/mode auto") (max 500))) (tag Ignore))
    (Rpc (method_ set_mode) (params ((approval_mode auto) (plan_mode false)))
     (tag (Mode_set "Mode: auto (tools run without asking)")))
    (Auto: never asks)
    |}];
  (* /plan toggles, keeping the approval mode to go back to. *)
  run h "/plan";
  mode_chip h;
  run h "/plan";
  mode_chip h;
  run h "/plan on";
  run h "/plan off";
  run h "/plan maybe";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /plan) (max 500)))
     (tag Ignore))
    (Rpc (method_ set_mode) (params ((approval_mode auto) (plan_mode true)))
     (tag
      (Mode_set
       "Mode: plan (read-only planning: no file edits; bash, job_input and MCP tools ask first)")))
    (Plan mode)
    (Rpc (method_ push_pref) (params ((key history) (value /plan) (max 500)))
     (tag Ignore))
    (Rpc (method_ set_mode) (params ((approval_mode auto) (plan_mode false)))
     (tag (Mode_set "Mode: auto (tools run without asking)")))
    (Auto: never asks)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/plan on") (max 500))) (tag Ignore))
    (Rpc (method_ set_mode) (params ((approval_mode auto) (plan_mode true)))
     (tag
      (Mode_set
       "Mode: plan (read-only planning: no file edits; bash, job_input and MCP tools ask first)")))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/plan off") (max 500))) (tag Ignore))
    (Rpc (method_ set_mode) (params ((approval_mode auto) (plan_mode false)))
     (tag (Mode_set "Mode: auto (tools run without asking)")))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/plan maybe") (max 500))) (tag Ignore))
    Unknown argument “maybe”: use /plan on, /plan off, or /plan to toggle. (Dismiss)
    |}];
  (* /confirm sets the default for new sessions, which is this one's too. *)
  run h "/confirm on";
  mode_chip h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/confirm on") (max 500))) (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((approval_mode ask)))))
     (tag
      (Config_saved
       "Tool confirmation on (mode ask, also for new sessions): bash, job_input, edits and MCP tools ask first")))
    (Auto: never asks)
    |}];
  (* The arguments complete. *)
  H.type_ h "/mode ";
  H.text h ~selector:".popup";
  H.type_ h "/plan ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Modes ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    ask bash, job_input, edits and MCP tools ask first
    auto-edit file edits run; bash, job_input and MCP tools ask first
    auto tools run without asking
    plan read-only planning: no file edits; bash, job_input and MCP tools ask first
    Plan mode ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    on read-only planning until you approve the plan
    off leave plan mode
    |}]
;;

let%expect_test "the plan: its chip, /todos" =
  let h = H.create () in
  run h "/todos";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /todos) (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 4220))
    No plan: the model has not made one (it makes one with update_plan).
    |}];
  H.event h (sprintf {|{"event":"state","state":%s}|} (state ~plan ()));
  status h;
  [%expect
    {|
    (0%) ↑0 ↓0 $0.0000 Ready
    (plan 1/3: change it)
    |}];
  H.click h ~selector:".status-extras .plan";
  modal h;
  [%expect
    {|
    (Run /todos)
    (Focus dialog)
    Plan 1/3
    (Close (Esc))
    first pass
    ✓ read the code
    ▸ change it
    ○ test
    (Done)
    |}];
  (* Done: no chip, the plan is still there. *)
  H.act h Close_dialog;
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (state
          ~plan:
            {|{"steps":[{"step":"read the code","status":"completed"},{"step":"test","status":"completed"}]}|}
          ()));
  status h;
  run h "/todos";
  modal h;
  [%expect
    {|
    (Focus editor)
    (0%) ↑0 ↓0 $0.0000 Ready
    (Rpc (method_ push_pref) (params ((key history) (value /todos) (max 500)))
     (tag Ignore))
    (Focus dialog)
    Plan 2/2
    (Close (Esc))
    ✓ read the code
    ✓ test
    (Done)
    |}]
;;

let%expect_test "/permissions" =
  let h =
    H.create ~state:(state ~mode:"auto-edit" ~rules:[ "bash(make:*)" ] ()) ()
  in
  run h "/permissions";
  modal h;
  H.act h Close_dialog;
  H.act
    h
    (Reply
       ( Config
       , Ok
           (Jsonaf.of_string
              {|{"scoped_models":[],"approval_mode":"ask","permissions":{"allow":["bash(git status:*)"],"ask":[],"deny":["bash(rm:*)","write(/etc/**)"]}}|})
       ));
  H.event
    h
    (sprintf {|{"event":"state","state":%s}|} (state ~plan_mode:true ()));
  run h "/permissions";
  modal h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /permissions) (max 500))) (tag Ignore))
    (Focus dialog)
    Permissions
    (Close (Esc))
    Mode: auto-edit (file edits run; bash, job_input and MCP tools ask first)
    Allowed this session: bash(make:*)

    Rules are permissions in ~/.prigh/config.json, e.g. {"allow": ["bash(git status:*)"], "deny": ["bash(rm:*)"]}.
    /mode or Alt+M switches the mode.
    (Done)
    (Focus editor)
    (Rpc (method_ push_pref)
     (params ((key history) (value /permissions) (max 500))) (tag Ignore))
    (Focus dialog)
    Permissions
    (Close (Esc))
    Mode: plan (read-only planning: no file edits; bash, job_input and MCP tools ask first)
    Allowed this session: nothing yet (a confirmation's "always" adds to it)
    allow (config): bash(git status:*)
    deny (config): bash(rm:*), write(/etc/**)

    Rules are permissions in ~/.prigh/config.json, e.g. {"allow": ["bash(git status:*)"], "deny": ["bash(rm:*)"]}.
    /mode or Alt+M switches the mode.
    (Done)
    |}]
;;

let confirm ?agent ?(rules = []) ?(name = "bash") id summary =
  sprintf
    {|{"event":"tool_confirm","call_id":"%s","agent_id":%s,"name":"%s","summary":%s,"rules":[%s]}|}
    id
    (Option.value_map agent ~default:"null" ~f:(sprintf "%S"))
    name
    (Jsonaf.to_string (`String summary))
    (String.concat ~sep:"," (List.map rules ~f:(sprintf "%S")))
;;

let%expect_test "a confirmation: allow once, always, deny, deny with a reason" =
  let h = H.create () in
  H.event h (confirm ~rules:[ "bash(make:*)" ] "c1" "make test");
  H.event h (confirm "c2" "rm -rf build");
  H.event
    h
    (confirm ~agent:"a1" ~name:"write" ~rules:[ "edit" ] "c3" "notes.md");
  modal h;
  [%expect
    {|
    (Focus confirm)
    Confirm
    (Close (Esc))
    Run bash: make test?
    (y Allow once) (a Always allow bash(make:*) this session) (n Deny) (r Deny, saying why)
    2 more waiting
    |}];
  H.show h ~selector:".confirm-answers";
  [%expect
    {|
    <div class="confirm-answers">
      <button type="button"
              title="y or Enter"
              aria-label="Allow once"
              aria-keyshortcuts="y"
              class="allow btn confirm-answer primary"
              @on_click>
        <kbd> y </kbd>
        Allow once
      </button>
      <button type="button"
              title="a"
              aria-label="Always allow bash(make:*) this session"
              aria-keyshortcuts="a"
              class="always btn confirm-answer"
              @on_click>
        <kbd> a </kbd>
        Always allow bash(make:*) this session
      </button>
      <button type="button"
              title="n or Esc"
              aria-label="Deny"
              aria-keyshortcuts="n"
              class="btn confirm-answer deny"
              @on_click>
        <kbd> n </kbd>
        Deny
      </button>
      <button type="button"
              title="r"
              aria-label="Deny, saying why"
              aria-keyshortcuts="r"
              class="btn confirm-answer reason"
              @on_click>
        <kbd> r </kbd>
        Deny, saying why
      </button>
    </div>
    |}];
  (* Always: the rules for the rest of the session. *)
  H.key h "a";
  toasts h;
  modal h;
  [%expect
    {|
    (Answer_confirm (call_id c1) (agent_id ()) (answer Always))
    (Rpc (method_ tool_confirm_respond)
     (params ((call_id c1) (allow true) (always true))) (tag Answer))
    (Focus confirm)
    (Expire_toast (id 0) (after_ms 2860))
    Allowed bash; always: bash(make:*)
    Confirm
    (Close (Esc))
    Run bash: rm -rf build?
    (y Allow once) (n Deny) (r Deny, saying why)
    1 more waiting
    |}];
  (* No rules, no always. *)
  H.key h "a";
  H.key h "r";
  modal h;
  [%expect
    {|
    (browser default)
    (Answer_confirm (call_id c2) (agent_id ()) (answer Deny_with_reason))
    (Focus confirm-reason)
    Confirm
    (Close (Esc))
    Run bash: rm -rf build?
    Why not? (the model sees it)
    []
    1 more waiting (Back) (Deny)
    |}];
  H.input h ~selector:"#confirm-reason" "it deletes the cache";
  (* Esc goes back to the answers, nothing sent; the reason starts afresh. *)
  H.key h "Escape" ~target:Field;
  modal h;
  [%expect
    {|
    (Confirm_reason_input "it deletes the cache")
    Cancel_confirm_reason
    (Focus confirm)
    Confirm
    (Close (Esc))
    Run bash: rm -rf build?
    (y Allow once) (n Deny) (r Deny, saying why)
    1 more waiting
    |}];
  H.click h ~selector:".confirm-answer.reason";
  H.input h ~selector:"#confirm-reason" "it deletes the cache";
  H.key h "Enter" ~target:Field;
  toasts h;
  modal h;
  [%expect
    {|
    (Answer_confirm (call_id c2) (agent_id ()) (answer Deny_with_reason))
    (Focus confirm-reason)
    (Confirm_reason_input "it deletes the cache")
    Send_confirm_reason
    (Rpc (method_ tool_confirm_respond)
     (params ((call_id c2) (allow false) (reason "it deletes the cache")))
     (tag Answer))
    (Focus confirm)
    (Expire_toast (id 1) (after_ms 2500))
    Allowed bash; always: bash(make:*)
    Denied bash
    Confirm
    (Close (Esc))
    [a1] Write notes.md?
    (y Allow once) (a Always allow edit this session) (n Deny) (r Deny, saying why)
    |}];
  (* A subagent's: its id is sent back. *)
  H.click h ~selector:".confirm-answer.allow";
  modal h;
  [%expect
    {|
    (Answer_confirm (call_id c3) (agent_id (a1)) (answer Allow))
    (Rpc (method_ tool_confirm_respond)
     (params ((call_id c3) (allow true) (agent_id a1))) (tag Answer))
    (Focus editor)
    (Expire_toast (id 2) (after_ms 2500))
    |}]
;;

let%expect_test "approving the plan" =
  let h = H.create ~state:(state ~plan_mode:true ()) () in
  H.event
    h
    (confirm
       ~name:"exit_plan_mode"
       ~rules:[ "never shown" ]
       "p1"
       "## Plan\n\n1. Change `main.ml`\n2. Test");
  modal h;
  H.show h ~selector:".confirm-plan";
  [%expect
    {|
    (Focus confirm)
    Plan
    (Close (Esc))
    Approve the plan and start implementing?
    Plan
    Change main.ml
    Test
    (y Approve: leave plan mode and implement) (n Keep planning) (r Keep planning, saying what to change)
    <div class="confirm-plan">
      <div class="markdown">
        <div class="md-part">
          <h2 dir="auto"> Plan </h2>
        </div>
        <div class="md-part">
          <ol>
            <li dir="auto">
              Change
              <code> main.ml </code>
            </li>
            <li dir="auto"> Test </li>
          </ol>
        </div>
      </div>
    </div>
    |}];
  H.key h "a";
  H.key h "r";
  H.input h ~selector:"#confirm-reason" "add docs";
  modal h;
  H.click h ~selector:".modal-buttons .danger";
  toasts h;
  [%expect
    {|
    (browser default)
    (Answer_confirm (call_id p1) (agent_id ()) (answer Deny_with_reason))
    (Focus confirm-reason)
    (Confirm_reason_input "add docs")
    Plan
    (Close (Esc))
    Approve the plan and start implementing?
    What should change in the plan?
    [add docs]
    (Back) (Keep planning)
    Send_confirm_reason
    (Rpc (method_ tool_confirm_respond)
     (params ((call_id p1) (allow false) (reason "add docs"))) (tag Answer))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2820))
    Plan not approved: still planning
    |}];
  H.event h (confirm ~name:"exit_plan_mode" "p2" "1. Do it");
  H.key h "y";
  toasts h;
  [%expect
    {|
    (Focus confirm)
    (Answer_confirm (call_id p2) (agent_id ()) (answer Allow))
    (Rpc (method_ tool_confirm_respond) (params ((call_id p2) (allow true)))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 1) (after_ms 2500))
    Plan not approved: still planning
    Plan approved
    |}]
;;

let%expect_test "a confirmation answered elsewhere, or withdrawn, closes" =
  let h = H.create () in
  H.event h (confirm "c1" "make");
  H.event h (confirm ~agent:"a1" "c1" "make");
  H.key h "r";
  H.input h ~selector:"#confirm-reason" "half typed";
  (* Another agent's call with the same id is another confirmation. *)
  H.event h {|{"event":"tool_confirm_done","call_id":"c1","agent_id":"a1"}|};
  modal h;
  [%expect
    {|
    (Focus confirm)
    (Answer_confirm (call_id c1) (agent_id ()) (answer Deny_with_reason))
    (Focus confirm-reason)
    (Confirm_reason_input "half typed")
    (Expire_toast (id 0) (after_ms 2740))
    Confirm
    (Close (Esc))
    Run bash: make?
    Why not? (the model sees it)
    [half typed]
    (Back) (Deny)
    |}];
  H.event h {|{"event":"tool_confirm_done","call_id":"c1","agent_id":null}|};
  modal h;
  toasts h;
  [%expect
    {|
    (Focus editor)
    (Expire_toast (id 1) (after_ms 2740))
    Confirmation answered elsewhere ×2
    |}];
  (* Answered here: the backend's word that it is done changes nothing. *)
  H.event h (confirm "c2" "make");
  H.key h "y";
  H.event h {|{"event":"tool_confirm_done","call_id":"c2","agent_id":null}|};
  toasts h;
  [%expect
    {|
    (Focus confirm)
    (Answer_confirm (call_id c2) (agent_id ()) (answer Allow))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c2) (allow true)))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 2) (after_ms 2500))
    Confirmation answered elsewhere ×2
    Allowed bash
    |}]
;;

let ask id questions =
  sprintf {|{"event":"ask_user","call_id":"%s","questions":%s}|} id questions
;;

let questions =
  {|[{"question":"Which database?","header":"Storage","options":[{"label":"Postgres","description":"the one in prod"},{"label":"SQLite"}]},
     {"question":"Which checks?","options":[{"label":"lint"},{"label":"tests"},{"label":"bench"}],"multiple":true},
     {"question":"Anything else?"}]|}
;;

let%expect_test "a question dialog: keys" =
  let h = H.create () in
  H.event h (ask "q1" questions);
  modal h;
  H.elements h ~selector:"[inert]";
  [%expect
    {|
    (Focus question)
    Storage (1/3)
    (Close (Esc))
    Which database?
    ○ Postgres the one in prod
    ○ SQLite
    Other:
    []
    Space or a click picks, ↑↓ choose, Enter accepts, Esc declines (Decline) (Next)
    <aside class="sidebar">
    <header class="topbar">
    <div class="chat-frame">
    <footer class="composer">
    |}];
  H.show h ~selector:".question-choices";
  [%expect
    {|
    <div role="radiogroup" class="question-choices">
      <div role="radio" aria-checked="false" class="question-choice selected" @on_click>
        <span aria-hidden="true" class="check radio"> </span>
        <span class="question-label"> Postgres </span>
        <span class="question-detail"> the one in prod </span>
      </div>
      <div role="radio" aria-checked="false" class="question-choice" @on_click>
        <span aria-hidden="true" class="check radio"> </span>
        <span class="question-label"> SQLite </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
    </div>
    |}];
  (* ↓ to SQLite, Enter takes it. *)
  H.key h "ArrowDown" ~target:Page;
  H.key h "Enter" ~target:Page;
  modal h;
  [%expect
    {|
    (Question Down)
    (Question Accept)
    Question 2/3
    (Close (Esc))
    Which checks?
    ☐ lint
    ☐ tests
    ☐ bench
    Other:
    []
    Space toggles, ↑↓ choose, Enter accepts, Esc declines (Decline) (Back) (Next)
    |}];
  (* Several: Space toggles; typing goes to the own answer. *)
  H.key h " " ~target:Page;
  H.key h "ArrowDown" ~target:Page;
  H.key h "ArrowDown" ~target:Page;
  H.key h " " ~target:Page;
  H.key h "x" ~target:Page;
  H.key h "Backspace" ~target:Page;
  H.key h "f" ~target:Page;
  H.input h ~selector:"#question-own" "fuzz";
  (* In its field, ← → move the caret and Space types. *)
  H.key h "ArrowLeft" ~target:Field;
  H.key h " " ~target:Field;
  modal h;
  [%expect
    {|
    (Question Toggle)
    (Question Down)
    (Question Down)
    (Question Toggle)
    (Question (Insert x))
    (Focus question-own)
    (Question Backspace)
    (Question (Insert f))
    (Question (Set_own fuzz))
    (browser default)
    (browser default)
    Question 2/3
    (Close (Esc))
    Which checks?
    ☑ lint
    ☐ tests
    ☑ bench
    Other:
    [fuzz]
    Space toggles, ↑↓ choose, Enter accepts, Esc declines (Decline) (Back) (Next)
    |}];
  (* ← back to the first question: its answer is kept. *)
  H.key h "ArrowUp" ~target:Field;
  H.key h "ArrowLeft" ~target:Page;
  modal h;
  H.key h "ArrowRight" ~target:Page;
  H.key h "Enter" ~target:Page;
  modal h;
  [%expect
    {|
    (Question Up)
    (Focus question)
    (Question Prev)
    Storage (1/3)
    (Close (Esc))
    Which database?
    ○ Postgres the one in prod
    ◉ SQLite
    Other:
    []
    Space or a click picks, ↑↓ choose, Enter accepts, Esc declines (Decline) (Next)
    (Question Next)
    (Question Accept)
    (Focus question-own)
    Question 3/3
    (Close (Esc))
    Anything else?
    []
    type your answer, ↑↓ choose, Enter accepts, Esc declines (Decline) (Back) (Send)
    |}];
  (* The last question has no choices: only the own answer. *)
  H.key h "n" ~target:Page;
  H.input h ~selector:"#question-own" "no";
  H.key h "Enter" ~target:Field;
  modal h;
  toasts h;
  [%expect
    {|
    (Question (Insert n))
    (Question (Set_own no))
    (Question Accept)
    (Rpc (method_ ask_user_respond)
     (params ((call_id q1) (answers ((SQLite) (lint bench fuzz) (no)))))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2500))
    Answered
    |}]
;;

let%expect_test "a question dialog: clicks" =
  let h = H.create () in
  H.event h (ask "q1" questions);
  H.click h ~selector:".question-choice" ~nth:1;
  H.click h ~selector:".modal-buttons .primary";
  H.click h ~selector:".question-choice" ~nth:0;
  H.click h ~selector:".question-choice" ~nth:2;
  H.click h ~selector:".question-choice" ~nth:0;
  H.show h ~selector:".question-choices";
  H.click h ~selector:"#question-own";
  H.click h ~selector:".modal-buttons .primary";
  [%expect
    {|
    (Focus question)
    (Question (Choose 1))
    (Question Accept)
    (Question (Choose 0))
    (Question (Choose 2))
    (Question (Choose 0))
    <div role="group" class="question-choices">
      <div role="checkbox" aria-checked="false" class="question-choice selected" @on_click>
        <span aria-hidden="true" class="check"> </span>
        <span class="question-label"> lint </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <div role="checkbox" aria-checked="false" class="question-choice" @on_click>
        <span aria-hidden="true" class="check"> </span>
        <span class="question-label"> tests </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <div role="checkbox" aria-checked="true" class="picked question-choice" @on_click>
        <span aria-hidden="true" class="check on">
          <icon class="check"> </icon>
        </span>
        <span class="question-label"> bench </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
    </div>
    (Question (Row 3))
    (Focus question-own)
    (Question Accept)
    |}];
  H.click h ~selector:".modal-buttons button" ~nth:1;
  modal h;
  [%expect
    {|
    (Question Prev)
    (Focus question)
    Question 2/3
    (Close (Esc))
    Which checks?
    ☐ lint
    ☐ tests
    ☑ bench
    Other:
    []
    Space toggles, ↑↓ choose, Enter accepts, Esc declines (Decline) (Back) (Next)
    |}];
  H.click h ~selector:".modal-buttons .primary";
  H.input h ~selector:"#question-own" "ship it";
  H.click h ~selector:".modal-buttons .primary";
  [%expect
    {|
    (Question Accept)
    (Focus question-own)
    (Question (Set_own "ship it"))
    (Question Accept)
    (Rpc (method_ ask_user_respond)
     (params ((call_id q1) (answers ((SQLite) (bench) ("ship it")))))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2500))
    |}]
;;

let%expect_test
    "questions: decline, answered elsewhere, queued behind confirmations"
  =
  let h = H.create () in
  H.event h (ask "q1" {|[{"question":"Ready?"}]|});
  H.event h (confirm "c1" "make");
  H.event h (ask "q2" {|[{"question":"Sure?"}]|});
  (* The confirmation first, then the questions in turn. *)
  modal h;
  [%expect
    {|
    (Focus question-own)
    (Focus confirm)
    Confirm
    (Close (Esc))
    Run bash: make?
    (y Allow once) (n Deny) (r Deny, saying why)
    |}];
  H.key h "y";
  modal h;
  [%expect
    {|
    (Answer_confirm (call_id c1) (agent_id ()) (answer Allow))
    (Rpc (method_ tool_confirm_respond) (params ((call_id c1) (allow true)))
     (tag Answer))
    (Focus question-own)
    (Expire_toast (id 0) (after_ms 2500))
    Question
    (Close (Esc))
    Ready?
    []
    type your answer, ↑↓ choose, Enter accepts, Esc declines, 1 more waiting (Decline) (Send)
    |}];
  H.key h "Escape" ~target:Page;
  modal h;
  toasts h;
  [%expect
    {|
    (Question Decline)
    (Rpc (method_ ask_user_respond) (params ((call_id q1) (decline true)))
     (tag Answer))
    (Focus question-own)
    (Expire_toast (id 1) (after_ms 3460))
    Question
    (Close (Esc))
    Sure?
    []
    type your answer, ↑↓ choose, Enter accepts, Esc declines (Decline) (Send)
    Allowed bash
    Declined to answer: the model goes on without one
    |}];
  (* Answered in another tab, or the run stopped. *)
  H.event h {|{"event":"ask_user_done","call_id":"q2"}|};
  modal h;
  toasts h;
  H.elements h ~selector:"[inert]";
  [%expect
    {|
    (Focus editor)
    (Expire_toast (id 2) (after_ms 3380))
    Declined to answer: the model goes on without one
    Question answered elsewhere, or the run stopped
    |}];
  (* Esc after the question: the editor's again. *)
  H.key h "Escape";
  [%expect {| (browser default) |}]
;;

let%expect_test
    "while reconnecting the confirmation stays; get_pending then says what is \
     waiting"
  =
  let h = H.create () in
  H.event h (confirm "c1" "make");
  H.act h Backend_closed;
  modal h;
  H.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
  H.reply h "get_state" (state ());
  H.reply
    h
    "get_pending"
    {|{"steer_texts":[],"follow_up_texts":["later"],
       "confirms":[{"call_id":"c1","agent_id":null,"name":"bash","summary":"make","rules":["bash(make:*)"]},
                   {"call_id":"c2","agent_id":"a1","name":"write","summary":"x","rules":["edit"]}],
       "questions":[{"call_id":"q3","questions":[{"question":"Sure?","options":[{"label":"yes"}]}]}]}|};
  modal h;
  status h;
  [%expect
    {|
    (Focus confirm)
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    Confirm
    (Close (Esc))
    Run bash: make?
    (y Allow once) (n Deny) (r Deny, saying why)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ get_prefs)
     (params
      ((keys
        (web.theme web.accent web.notify web.verbosity web.terminal-open
         web.sidebar-folded history web.migrated web.sidebar-width
         web.files-width web.files-split web.agents-width web.terminal-height))))
     (tag Prefs))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 1000000)))
     (tag (Resync (session s1) (from ()))))
    (Rpc (method_ get_pending) (params ()) (tag Resync_pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    Confirm
    (Close (Esc))
    Run bash: make?
    (y Allow once) (a Always allow bash(make:*) this session) (n Deny) (r Deny, saying why)
    1 more waiting
    (0%) ↑0 ↓0 $0.0000 Ready
    (1 queued)
    |}];
  (* Already shown ones are not added twice. *)
  H.event h (confirm ~rules:[ "bash(make:*)" ] "c1" "make");
  H.event h (ask "q3" {|[{"question":"Sure?"}]|});
  let m = H.model h in
  print_s
    [%message
      ""
        ~confirms:(List.length m.confirms : int)
        ~questions:(List.length m.questions : int)];
  [%expect {| ((confirms 2) (questions 1)) |}];
  H.key h "a";
  H.key h "n";
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Answer_confirm (call_id c1) (agent_id ()) (answer Always))
    (Rpc (method_ tool_confirm_respond)
     (params ((call_id c1) (allow true) (always true))) (tag Answer))
    (Focus confirm)
    (Expire_toast (id 0) (after_ms 2860))
    (Answer_confirm (call_id c2) (agent_id (a1)) (answer Deny))
    (Rpc (method_ tool_confirm_respond)
     (params ((call_id c2) (allow false) (agent_id a1))) (tag Answer))
    (Focus question)
    (Expire_toast (id 1) (after_ms 2500))
    (Question Accept)
    (Rpc (method_ ask_user_respond) (params ((call_id q3) (answers ((yes)))))
     (tag Answer))
    (Focus editor)
    (Expire_toast (id 2) (after_ms 2500))
    |}]
;;

let%expect_test "switching session drops what the old one waited for" =
  let h = H.create () in
  H.event h (confirm "c1" "make");
  H.event h (ask "q1" {|[{"question":"Ready?"}]|});
  H.key h "r";
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "session_id", `String "s2" ] ()));
  modal h;
  let m = H.model h in
  print_s
    [%message
      ""
        ~confirms:(List.length m.confirms : int)
        ~reason:(m.confirm_reason : string option)
        ~questions:(List.length m.questions : int)];
  [%expect
    {|
    (Focus confirm)
    (Answer_confirm (call_id c1) (agent_id ()) (answer Deny_with_reason))
    (Focus confirm-reason)
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    ((confirms 0) (reason ()) (questions 0))
    |}]
;;

let tool ?result ?(is_error = false) name id arguments =
  let call =
    sprintf
      {|{"id":"%s","name":"%s","arguments":%s}|}
      id
      name
      (Jsonaf.to_string (`String arguments))
  in
  sprintf {|{"event":"tool_start","call":%s}|} call
  :: Option.value_map result ~default:[] ~f:(fun text ->
    [ sprintf
        {|{"event":"tool_end","call":%s,"result":{"tool_call_id":"%s","tool_name":"%s","text":%s,"is_error":%b}}|}
        call
        id
        name
        (Jsonaf.to_string (`String text))
        is_error
    ])
;;

let%expect_test "update_plan, ask_user and exit_plan_mode in the transcript" =
  let chat =
    Chat_harness.chat
      ~running:true
      (tool
         "update_plan"
         "t1"
         {|{"explanation":"first pass","plan":[{"step":"read the code","status":"completed"},{"step":"change it","status":"in_progress"},{"step":"test","status":"pending"}]}|}
         ~result:"Plan updated (1 of 3 steps completed)"
       @ tool
           "ask_user"
           "t2"
           {|{"questions":[{"question":"Which database?"}]}|}
           ~result:"The user answered:\nQ: Which database?\nA: Postgres\n"
       @ tool
           "ask_user"
           "t3"
           {|{"questions":[{"question":"Ready?"}]}|}
           ~is_error:true
           ~result:"The user declined to answer."
       @ tool
           "exit_plan_mode"
           "t4"
           {|{"plan":"1. Change `main.ml`\n2. Test"}|}
           ~is_error:true
           ~result:
             "[the user did not approve the plan; their feedback: docs too] \
              Stay in plan mode."
       @ tool "exit_plan_mode" "t5" {|{"plan":"1. Do it"}|} ~result:"Approved."
       @ tool
           "ask_user"
           "t6"
           {|{"questions":[{"question":"Ready?"},{"question":"Sure?"}]}|}
       @ tool "exit_plan_mode" "t7" {|{"plan":"1. Again"}|})
  in
  Chat_harness.text chat;
  [%expect
    {|
    ✓ update_plan 1/3 done first pass ✓ read the code ▸ change it ○ test ✓ ask_user Q: Which database?
    A: Postgres ✕ ask_user no answer The user declined to answer. ✕ exit_plan_mode not approved Change main.ml Test [the user did not approve the plan; their feedback: docs too] Stay in plan mode. ✓ exit_plan_mode approved Do it ask_user waiting for your answer Ready? Sure? exit_plan_mode waiting for your approval Again
    |}];
  Chat_harness.show ~selector:".tool-update_plan" chat;
  [%expect
    {|
    <div class="ok tool tool-update_plan">
      <div class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> update_plan </span>
        <span class="chip"> 1/3 done </span>
      </div>
      <div class="tool-body">
        <div class="plan">
          <p class="plan-explanation"> first pass </p>
          <ul class="plan-steps">
            <li class="plan-step step-done">
              <span title="Done" role="img" aria-label="Done" class="done mark">
                <icon class="check"> </icon>
              </span>
              read the code
            </li>
            <li class="plan-step step-now">
              <span title="In progress" role="img" aria-label="In progress" class="mark now">
                <icon class="next"> </icon>
              </span>
              change it
            </li>
            <li class="plan-step step-todo">
              <span title="To do" role="img" aria-label="To do" class="mark todo">
                <icon class="ring"> </icon>
              </span>
              test
            </li>
          </ul>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "plan mode notes are margin notes, not the user's words" =
  let chat =
    Chat_harness.chat
      [ {|{"event":"message_start","message":{"role":"user","text":"[Plan mode is on: investigate, do not edit files.]\n\nfix the build","origin":{"body":"fix the build","notes":[{"kind":"plan_mode","title":"Plan mode on: the model was told to plan, not change files","text":"[Plan mode is on: investigate, do not edit files.]"}]}}}|}
      ; {|{"event":"message_start","message":{"role":"user","text":"[Plan mode is off: you may edit files now.]","origin":{"body":"","notes":[{"kind":"plan_mode","title":"Plan mode off: the model was told it can change files","text":"[Plan mode is off: you may edit files now.]"}]}}}|}
      ]
  in
  Chat_harness.text chat;
  Chat_harness.show ~selector:".entries" chat;
  [%expect
    {|
    ◇ Plan mode on: the model was told to plan, not change files [Plan mode is on: investigate, do not edit files.] fix the build ◇ Plan mode off: the model was told it can change files [Plan mode is off: you may edit files now.]
    <div class="entries">
      <div data-entry="0" class="msg noted">
        <details data-kind="plan_mode" class="note">
          <summary>
            <span aria-hidden="true" class="glyph"> ◇ </span>
            <span class="title"> Plan mode on: the model was told to plan, not change files </span>
          </summary>
          <pre class="note-text"> [Plan mode is on: investigate, do not edit files.] </pre>
        </details>
        <div class="msg user">
          <div dir="auto" class="bubble"> fix the build </div>
        </div>
      </div>
      <div class="msg noted">
        <details data-kind="plan_mode" class="note">
          <summary>
            <span aria-hidden="true" class="glyph"> ◇ </span>
            <span class="title"> Plan mode off: the model was told it can change files </span>
          </summary>
          <pre class="note-text"> [Plan mode is off: you may edit files now.] </pre>
        </details>
      </div>
    </div>
    |}];
  let h = H.create () in
  run h "/fork";
  H.reply
    h
    "get_entries"
    {|{"head":"e2","entries":[
      {"id":"e1","parent":null,"kind":"message","message":{"role":"user","text":"[Plan mode is on: investigate.]\n\nfix the build","origin":{"body":"fix the build","notes":[{"kind":"plan_mode","title":"Plan mode on: the model was told to plan, not change files","text":"[Plan mode is on: investigate.]"}]}}},
      {"id":"e2","parent":"e1","kind":"message","message":{"role":"user","text":"[Plan mode is off: go.]","origin":{"body":"","notes":[{"kind":"plan_mode","title":"Plan mode off: the model was told it can change files","text":"[Plan mode is off: go.]"}]}}}]}|};
  H.text h ~selector:".picker-items";
  H.key h "Enter" ~target:Field;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /fork) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Focus picker-input)
    fix the build #1
    ◇ Plan mode off: the model was told it can change files #2 ✓
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3700))
    (Rpc (method_ fork) (params ((at e2))) (tag Reload_state))
    ""
    |}]
;;

(* An answer the backend did not take (e.g. the run was busy elsewhere):
   what is still waiting is shown again, rather than lost. *)
let%expect_test "an answer that fails is asked again" =
  let h = H.create () in
  H.quiet h (fun () -> H.event h (confirm "c1" "make test"));
  H.quiet h (fun () -> H.key h "y");
  H.fail h "tool_confirm_respond" "no such confirmation pending yet";
  H.reply
    h
    "get_pending"
    (sprintf
       {|{"confirms":[%s],"questions":[],"steer_texts":[],"follow_up_texts":[]}|}
       {|{"call_id":"c1","agent_id":null,"name":"bash","summary":"make test","rules":[]}|});
  toasts h;
  modal h;
  [%expect
    {|
    (Answer_confirm (call_id c1) (agent_id ()) (answer Allow))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Focus confirm)
    Allowed bash
    Couldn't send your answer: no such confirmation pending yet. It is asked again. (Dismiss)
    Confirm
    (Close (Esc))
    Run bash: make test?
    (y Allow once) (n Deny) (r Deny, saying why)
    |}]
;;

let%expect_test
    "a question dialog says when the agent carries on; a timeout closes it \
     quietly, the backend's notice says why"
  =
  let h = H.create () in
  H.act h (Set_utc_offset (Time_ns.Span.of_hr 2.));
  H.event
    h
    {|{"event":"ask_user","call_id":"q1","questions":[{"question":"Sure?"}],"deadline_ms":1760191500000}|};
  modal h;
  H.show h ~selector:".question-deadline";
  [%expect
    {|
    (Focus question-own)
    Question
    (Close (Esc))
    Sure?
    []
    unanswered, the agent carries on by itself at 16:05
    type your answer, ↑↓ choose, Enter accepts, Esc declines (Decline) (Send)
    <p class="question-deadline"> unanswered, the agent carries on by itself at 16:05 </p>
    |}];
  H.event h {|{"event":"ask_user_done","call_id":"q1","timed_out":true}|};
  H.event
    h
    {|{"event":"notice","text":"the question was not answered within 10m; the agent carries on without it"}|};
  modal h;
  toasts h;
  [%expect
    {|
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4420))
    the question was not answered within 10m; the agent carries on without it
    |}];
  (* Without a deadline, no note. *)
  H.event h (ask "q2" {|[{"question":"Sure?"}]|});
  H.show h ~selector:".question-deadline";
  [%expect {| (Focus question-own) |}]
;;
