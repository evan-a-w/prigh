open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let alt_m h = H.key h (Key.alt (Char "m"))

let type_quietly h text =
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)))
;;

let config ?model () =
  sprintf
    {|{"scoped_models":[],"approval_model":%s}|}
    (match model with
     | Some key -> P.Json.to_string (P.Json.str key)
     | None -> "null")
;;

let%expect_test "Alt+M: auto-edit → smart → auto; the status line shows smart" =
  let h = Test_app.connected () in
  H.event h (State { (state ()) with approval_mode = Auto_edit });
  alt_m h;
  [%expect
    {|
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode smart)
        (plan_mode     false)
        (chat_mode     false)))
      (tag (
        Mode_set
        "mode: smart \226\128\148 edits in the project run; the approval model lets other calls run or asks you, saying why (/approval-model)")))
    |}];
  H.reply ~quiet:true h (Mode_set "mode: smart") (state_json ());
  H.event h (State { (state ()) with approval_mode = Smart });
  H.show h;
  alt_m h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────── mode: smart ───
    > ▏
    deepseek-flash  think:off  mode:smart  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto)
        (plan_mode     false)
        (chat_mode     false)))
      (tag (Mode_set "mode: auto \226\128\148 tools run without asking")))
    |}]
;;

let%expect_test "an allowed call carries the approval model's note" =
  let h = Test_app.connected ~height:12 () in
  H.event h (State { (state ~running:true ()) with approval_mode = Smart });
  let call = tool_call ~arguments:{|{"command":"make test"}|} "c1" in
  H.event h (Tool_start call);
  H.event
    h
    (Tool_allowed
       { call_id = "c1"
       ; agent_id = None
       ; model = "anthropic/claude-haiku-4-5"
       ; reason = "runs the project's tests"
       });
  (* Another agent's call with the same id is not this one. *)
  H.event
    h
    (Tool_allowed
       { call_id = "c1"; agent_id = Some "a1"; model = "m"; reason = "not it" });
  H.event h (Tool_end { call; result = tool_result ~id:"c1" "ok" });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ✓ bash $ make test
        allowed by anthropic/claude-haiku-4-5: runs the
        project's tests
        ok
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ⠋ running tools · Esc aborts · Enter steers
    |}]
;;

let%expect_test "a subagent's allowed call: the note goes to its transcript" =
  let agents =
    Agent_view.apply_all
      []
      (Subagent_start
         { call_id = "s1"
         ; agent_id = "a1"
         ; task = "build"
         ; name = None
         ; model = "m"
         ; tools = []
         ; worktree = None
         ; forked = false
         ; continued = false
         })
  in
  let call = tool_call ~arguments:{|{"command":"make"}|} "c1" in
  let inner e : P.Event.t =
    Subagent { call_id = "s1"; agent_id = "a1"; event = e }
  in
  let agents = Agent_view.apply_all agents (inner (Tool_start call)) in
  let agents =
    Agent_view.apply_all
      agents
      (Tool_allowed
         { call_id = "c1"
         ; agent_id = Some "a1"
         ; model = "m"
         ; reason = "builds"
         })
  in
  List.iter agents ~f:(fun (a : Agent_view.t) ->
    List.iter (Transcript.items a.transcript) ~f:(function
      | Tool { call; allowed; _ } ->
        print_s [%message a.id call.id (allowed : string option)]
      | _ -> ()));
  [%expect {| (a1 c1 (allowed ("allowed by m: builds"))) |}]
;;

let%expect_test "a call the approval model left to the user shows its reason" =
  let h = Test_app.connected ~height:16 () in
  H.event h (State { (state ()) with approval_mode = Smart });
  H.event
    h
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; name = "bash"
       ; summary = "rm -rf ~/build"
       ; rules = []
       ; kind =
           Judged
             { model = "anthropic/claude-haiku-4-5"
             ; reason = "deletes a directory outside the project"
             }
       });
  H.show h;
  H.keys h "y";
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm (smart) ────────────────────────────────────────┐
    │ Run bash: rm -rf ~/build?                                │
    │ anthropic/claude-haiku-4-5 left this to you: deletes a   │
    │ directory outside the project                            │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  mode:smart  ctx:0.1%/1.0M  confirm: y/n/r
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   true)))
      (tag Show_error))
    |}]
;;

let%expect_test "parsing /approval-model's arguments" =
  List.iter [ ""; "claude-haiku-4-5"; " off "; "model haiku" ] ~f:(fun rest ->
    print_s
      [%message
        rest
          ~_:
            (Approval_model_command.parse rest
             : Approval_model_command.Action.t)]);
  [%expect
    {|
    ("" Show)
    (claude-haiku-4-5 (Set_model (claude-haiku-4-5)))
    (" off " (Set_model ()))
    ("model haiku" (Error "usage: /approval-model or /approval-model MODEL|off"))
    |}]
;;

let%expect_test "/approval-model shows, completes, sets, clears, and a refusal" =
  let h = Test_app.connected ~height:14 () in
  H.reply ~quiet:true h Models_catalog models_json;
  H.reply ~quiet:true h Config (config ());
  type_quietly h "/approval-model";
  H.enter h;
  H.enter h;
  H.reply h Approval_model_shown (config ());
  H.show h;
  type_quietly h "/approval-model ";
  H.show h;
  type_quietly h "haiku";
  H.enter h;
  H.enter h;
  H.reply
    h
    (Approval_model_saved "/approval-model anthropic/claude-haiku-4-5")
    (config ~model:"anthropic/claude-haiku-4-5" ());
  H.show h;
  type_quietly h "/approval-model off";
  H.enter h;
  H.enter h;
  type_quietly h "/approval-model claude-haku";
  H.enter h;
  H.reply_error
    h
    (Approval_model_saved "/approval-model claude-haku")
    "approval_model: unknown model \"claude-haku\"; did you mean: \
     anthropic/claude-haiku-4-5 (Claude Haiku 4.5)";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ in smart mode, the session's model judges the tool calls │
    │ that would ask (/approval-model MODEL|off)               │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ off                         the session's model judges th…
      anthropic/claude-fable-5    Claude Fable 5
      anthropic/claude-fable-5-1  Claude Fable 5.1
      openai/gpt-5.5              GPT-5.5
      deepseek/deepseek-flash     DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    > /approval-model ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    (Rpc
      (method_ set_config)
      (params ((config ((approval_model haiku)))))
      (tag (Approval_model_saved "/approval-model haiku")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ in smart mode, anthropic/claude-haiku-4-5 judges the     │
    │ tool calls that would ask (/approval-model MODEL|off)    │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((approval_model null)))))
      (tag (Approval_model_saved "/approval-model off")))
    (Rpc
      (method_ set_config)
      (params ((config ((approval_model claude-haku)))))
      (tag (Approval_model_saved "/approval-model claude-haku")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ approval_model: unknown model "claude-haku"; did you     │
    │ mean: anthropic/claude-haiku-4-5 (Claude Haiku 4.5)      │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /approval-model claude-haku▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/session counts the judged calls and their cost" =
  let h = Test_app.connected ~height:20 () in
  H.keys h "/session";
  H.enter h;
  H.reply
    ~quiet:true
    h
    Session_stats
    {|{"message_count":4,"turns":2,"tool_calls":{"bash":3},"usage":{"input":30,"output":13,"cache_read":5},"cost_usd":0.0031,"context_percent":1.5,"model_changes":0,"compactions":0,"masks":0,"duration_seconds":12.5,"approvals_judged":3,"approvals_cost_usd":0.0012}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ session_stats) (params ()) (tag Session_stats))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Session ────────────────────────────────────────────────┐
    │ messages       4                                         │
    │ turns          2                                         │
    │ tools          bash 3                                    │
    │ usage          in 30 out 13 cache 5                      │
    │ cost           $0.0031                                   │
    │ context        1.5%                                      │
    │ model changes  0                                         │
    │ compactions    0                                         │
    │ maskings       0                                         │
    │ approvals      3 judged, $0.0012                         │
    │ duration       12.5s                                     │
    └───────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ↑↓ scroll · Esc closes
    |}]
;;
