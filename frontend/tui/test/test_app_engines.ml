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

let connected ?(deployed = deployed) ?cost_usd ?(extra = "") () =
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
      (String.chop_suffix_exn (state_json ?cost_usd ()) ~suffix:"}"
       ^ extra
       ^ "}");
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
  [%expect
    {|
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
  [%expect
    {|
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
  [%expect
    {|
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
  [%expect
    {|
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
  [%expect
    {|
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
  [%expect
    {|
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
    (Option.value_map h.H.model.notice ~default:"(no notice)" ~f:(fun n ->
       n.text));
  [%expect
    {|
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

let%expect_test "a Codex session that reported no cost runs on a subscription" =
  status (connected ~cost_usd:0. ~extra:codex_engine ());
  status (connected ~cost_usd:0.5 ~extra:codex_engine ());
  [%expect
    {|
    /work  Codex · deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  subscription
    /work  Codex · deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.50
    |}]
;;

let%expect_test
    "the model picker lists what the CLI's login offers, when it says"
  =
  let h = connected ~extra:codex_engine () in
  let codex id name =
    model_json ~provider:"codex" ~context_window:200000 id name
  in
  H.reply
    ~quiet:true
    h
    Models_catalog
    (sprintf "[%s]" (codex "default" "loading models…"));
  H.step h (Intent Model_picker);
  H.show h;
  (* Codex listed its models (prigh has no OpenAI login): asked again, the
     open picker shows them, none "not logged in". *)
  H.event h Models_changed;
  H.reply
    h
    Models_catalog
    (sprintf
       "[%s,%s,%s]"
       (codex "gpt-6.2-sol" "GPT-6.2-Sol")
       (codex "gpt-6.2-mini" "GPT-6.2-Mini")
       (codex "gpt-6.2-max" "GPT-6.2-Max"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    Model  (1)
    / ▏
    ▸  loading models… @ host  Codex  ctx 200k  $10/$50 per M
    ────────────────────────────────────────────────────────────────────────────────
    >
    Codex · deepseek-flash  Enter selects · Esc closes · Ctrl+N logged-in only
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    Model  (3)
    / ▏
    ▸  GPT-6.2-Sol @ host   Codex  ctx 200k  $10/$50 per M
       GPT-6.2-Mini @ host  Codex  ctx 200k  $10/$50 per M
       GPT-6.2-Max @ host   Codex  ctx 200k  $10/$50 per M
    ────────────────────────────────────────────────────────────────────────────────
    >
    Codex · deepseek-flash  Enter selects · Esc closes · Ctrl+N logged-in only
    |}]
;;

let%expect_test
    "a CLI that cannot list its models: the status line says why, /model what \
     to do"
  =
  let h = connected () in
  H.reply
    ~quiet:true
    h
    Initial_state
    (String.chop_suffix_exn
       (state_json
          ~model:(model_json ~provider:"codex" "default" "loading models…")
          ())
       ~suffix:"}"
     ^ codex_engine
     ^ {|,"harness_models":{"state":"failed","problem":"not_logged_in","summary":"not logged in on srv","message":"Codex on srv could not list its models: Codex is not logged in. Check that it is installed and logged in there (Settings → Engines).","fixes":[{"kind":"engines"},{"kind":"retry"}]}}|}
    );
  status h;
  command h "/model";
  H.reply ~quiet:true h (Models_for_picker "") "[]";
  H.show h;
  [%expect
    {|
    /work  Codex: not logged in on srv  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc (method_ list_models) (params ()) (tag (Models_for_picker "")))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    Model: Codex: not logged in on srv  (2)
    / ▏
    ▸  Settings → Engines  /adapters refresh  Codex on srv could not list its model…
       Retry               /adapters retry  Codex on srv could not list its models:…
    ────────────────────────────────────────────────────────────────────────────────
    >
    Codex: not logged in on srv  Enter selects · Esc closes · Ctrl+N logged-in only
    |}];
  (* The second fix: Retry. *)
  H.key h (Key.plain Down);
  H.enter h;
  command h "/adapters retry";
  [%expect
    {|
    (Rpc (method_ retry_models) (params ()) (tag Show_error))
    (Rpc (method_ retry_models) (params ()) (tag Show_error))
    |}]
;;

let all_modes_deployed : P.Deployed_engines.t =
  { engines = [ "harness"; "composer" ]
  ; adapters = [ "backend:acp:antigravity"; "acp:antigravity"; "codex" ]
  }
;;

let%expect_test
    "/model formats @ native / @ bridge / @ host, uses clean harness names, \
     and skips default placeholders"
  =
  let h = connected ~deployed:all_modes_deployed () in
  let models =
    sprintf
      "[%s,%s,%s,%s,%s]"
      (model_json
         ~provider:"anthropic"
         ~context_window:200000
         "claude-5"
         "Claude 5")
      (model_json
         ~provider:"backend:acp:antigravity"
         ~context_window:200000
         "default"
         "Antigravity default")
      (model_json
         ~provider:"backend:acp:antigravity"
         ~context_window:200000
         "gemini-4-neon"
         "Gemini 4 Neon")
      (model_json
         ~provider:"acp:antigravity"
         ~context_window:200000
         "gemini-4-neon"
         "Gemini 4 Neon")
      (model_json
         ~provider:"codex"
         ~context_window:200000
         "default"
         "loading models…")
  in
  H.reply ~quiet:true h Models_catalog models;
  H.step ~quiet:true h (Intent Model_picker);
  H.show h;
  (* /change_default formats the default model with @ bridge in multi-mode. *)
  H.key h (Key.plain Escape);
  H.reply
    ~quiet:true
    h
    Default_saved
    {|{"default_model":"backend:acp:antigravity/gemini-4-neon","default_thinking":"high"}|};
  print_endline
    (Option.value_map h.H.model.notice ~default:"(no notice)" ~f:(fun n ->
       n.text));
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
    ▸  Claude 5 @ native       anthropic/claude-5  ctx 200k  $10/$50 per M  not log…
       Gemini 4 Neon @ bridge  Antigravity ◆  ctx 200k  $10/$50 per M
       Gemini 4 Neon @ host    Antigravity ◆  ctx 200k  $10/$50 per M
       loading models… @ host  Codex ◆  ctx 200k  $10/$50 per M
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  Enter selects · Esc closes · Ctrl+N logged-in only
    default: Gemini 4 Neon @ bridge, thinking high
    |}]
;;

let%expect_test
    "/engine and /new distinguish bridge vs host and accept native, bridge, \
     host"
  =
  let h = connected ~deployed:all_modes_deployed ~extra:codex_engine () in
  command h "/engine";
  H.show h;
  H.key h (Key.plain Escape);
  (* Unambiguous mode arguments switch or create directly. *)
  command h "/engine native";
  command h "/engine bridge";
  command h "/new native";
  (* Ambiguous mode argument ("host" has Antigravity and Codex) opens a
     filtered picker. *)
  command h "/engine host";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    Switch this session to  (4)
    / ▏
    ▸* Codex                 OpenAI's Codex CLI on the session's tool host
       prigh harness         prigh's own agent, calling the model directly
       Antigravity (bridge)  runs on the prigh server, bridging tools to the host o…
       Antigravity           an ACP agent on the session's tool host
    ────────────────────────────────────────────────────────────────────────────────
    >
    Codex · deepseek-flash  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    (Rpc (method_ set_adapter) (params ((adapter harness))) (tag Show_error))
    (Rpc
      (method_ set_adapter)
      (params ((adapter backend:acp:antigravity)))
      (tag Show_error))
    (Rpc
      (method_ new_session)
      (params ((engine harness)))
      (tag (Reload_messages_notice "new prigh harness session")))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    Switch this session to (Host)  (2)
    / ▏
    ▸* Codex        OpenAI's Codex CLI on the session's tool host
       Antigravity  an ACP agent on the session's tool host
    ────────────────────────────────────────────────────────────────────────────────
    >
    Codex · deepseek-flash  think:off  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;
