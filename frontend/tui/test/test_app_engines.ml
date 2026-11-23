open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let codex_engine =
  {|,"engine":"composer","adapter":"codex","capabilities":{"fork":"by_handoff","rewind":"unsupported","context_edit":false,"compact":"auto_only","steer":"between_turns","plan_mode":false,"btw":false,"subagents":"observe"}|}
;;

let deployed : P.Deployed_engines.t =
  { engines = [ "harness"; "composer" ]; adapters = [ "codex"; "claude" ] }
;;

let connected ?(deployed = deployed) ?(extra = "") () =
  let h = Test_app.connected ~width:80 ~height:14 () in
  H.step
    ~quiet:true
    h
    (Hello
       { client_id = "c1"
       ; namespace = None
       ; user = None
       ; superuser = false
       ; features = [ "engines" ]
       ; deployed
       });
  if not (String.is_empty extra)
  then
    H.reply
      ~quiet:true
      h
      Initial_state
      (String.chop_suffix_exn (state_json ()) ~suffix:"}" ^ extra ^ "}");
  h
;;

(* Sent as typed (Alt+S), past the completions. *)
let command h text =
  H.keys h text;
  H.key h (Key.alt (Char "s"))
;;

let status h =
  print_endline
    (Screen.to_plain (Render.screen h.H.model)
     |> String.split_lines
     |> List.last_exn)
;;

let%expect_test "a harness session on a harness-only backend: as before" =
  let h = connected ~deployed:P.Deployed_engines.harness_only () in
  status h;
  command h "/new";
  [%expect
    {|
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ new_session)
      (params ())
      (tag (Reload_messages_notice "new session")))
    |}];
  command h "/engine";
  H.show h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ this backend runs no composer adapters: start it with -engines               │
    │ harness,composer -adapters codex,claude to switch between them               │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/new asks which engine; the session's first; Enter sends it" =
  let h = connected ~extra:codex_engine () in
  status h;
  command h "/new";
  H.show h;
  [%expect {|
    /work  Codex · deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    New session on  (3)
    / ▏
    ▸* Codex          OpenAI's Codex CLI on the session's tool host
       prigh harness  prigh's own agent, calling the model directly
       Claude Code    Anthropic's Claude Code CLI on the session's tool host
    ────────────────────────────────────────────────────────────────────────────────
    >
    Codex · deepseek-flash  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "cla";
  H.enter h;
  [%expect {|
    (Rpc
      (method_ new_session)
      (params (
        (engine  composer)
        (adapter claude)))
      (tag (Reload_messages_notice "new Claude Code session")))
    |}]
;;

let%expect_test "/new and /engine take the engine as an argument" =
  let h = connected ~extra:codex_engine () in
  command h "/new harness";
  command h "/new gemini";
  H.show h;
  command h "/engine claude";
  [%expect {|
    (Rpc
      (method_ new_session)
      (params ((engine harness)))
      (tag (Reload_messages_notice "new prigh harness session")))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ────────── no engine "gemini" here: this backend runs harness, codex, claude ───
    > ▏
    /work  Codex · deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ set_adapter) (params ((adapter claude))) (tag Show_error))
    |}]
;;

let%expect_test "/engine picks among the adapters" =
  let h = connected ~extra:codex_engine () in
  command h "/engine";
  H.show h;
  H.key h (Key.plain Down);
  H.enter h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    Switch this session to  (3)
    / ▏
    ▸* Codex          OpenAI's Codex CLI on the session's tool host
       prigh harness  prigh's own agent, calling the model directly
       Claude Code    Anthropic's Claude Code CLI on the session's tool host
    ────────────────────────────────────────────────────────────────────────────────
    >
    Codex · deepseek-flash  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    (Rpc (method_ set_adapter) (params ((adapter harness))) (tag Show_error))
    |}]
;;

let%expect_test "what the engine cannot do is refused, in the backend's words" =
  let h = connected ~extra:codex_engine () in
  List.iter
    [ "/rewind"; "/fork"; "/compact"; "/trim"; "/btw why?"; "/plan on" ]
    ~f:(fun c ->
      command h c;
      print_endline
        (Option.value_map h.H.model.notice ~default:"(no notice)" ~f:(fun n ->
           n.text)));
  [%expect {|
    Codex sessions can't rewind in place: fork from that message instead (/fork)
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork))
    (no notice)
    Codex compacts its context by itself when it fills: /compact isn't available
    Codex sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one
    Codex sessions have no side questions (/btw): ask in the session instead
    Codex sessions have no plan mode: ask for a plan in your message instead
    |}]
;;

let%expect_test "an unavailable session: the status and the prompt say why" =
  let h =
    connected
      ~extra:
        (codex_engine
         ^ {|,"unavailable":"Codex is not installed on srv: install it there (npm i -g @openai/codex), then retry"|}
        )
      ()
  in
  H.show h;
  H.keys h "hello";
  H.enter h;
  print_endline
    (Option.value_map h.H.model.notice ~default:"(no notice)" ~f:(fun n -> n.text));
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ────────────────────────────────────────────────────────────────────────────────
    > ▏can't run here: Codex is not installed on srv: install it there (npm i -g @o…
    Codex · deepseek-flash  unavailable  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Append_history hello)
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value hello)
        (max   500)))
      (tag Ignore))
    Codex is not installed on srv: install it there (npm i -g @openai/codex), then retry
    |}]
;;
