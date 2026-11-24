open! Core
open! Prigh_web
module H = Harness

(* Sessions on the composer engine (Codex, Claude Code, ...): the model chip
   names the adapter, /new and the new-session screen pick the engine,
   /engine switches the adapter, what the engine cannot do is refused (or
   disabled) in the backend's words, and a session that cannot run here
   says why. *)

let deployed : Prigh_protocol.Deployed_engines.t =
  { engines = [ "harness"; "composer" ]; adapters = [ "codex"; "claude" ] }
;;

let hello ?(deployed = deployed) h =
  H.act
    h
    (Hello
       { client_id = "c1"
       ; namespace = None
       ; user = None
       ; superuser = false
       ; features = [ "engines" ]
       ; deployed
       })
;;

let codex_capabilities =
  Jsonaf.of_string
    {|{"fork":"by_handoff","rewind":"unsupported","context_edit":false,"compact":"auto_only","steer":"between_turns","plan_mode":false,"btw":false,"subagents":"observe"}|}
;;

let codex_fields ?unavailable () =
  [ "engine", `String "composer"
  ; "adapter", `String "codex"
  ; "capabilities", codex_capabilities
  ]
  @ Option.value_map unavailable ~default:[] ~f:(fun u ->
    [ "unavailable", `String u ])
;;

let session ?deployed ?unavailable ?(fields = []) () =
  let h =
    H.create
      ~state:(H.state_json ~fields:(codex_fields ?unavailable () @ fields) ())
      ()
  in
  H.quiet h (fun () -> hello ?deployed h);
  h
;;

let toasts h = H.text h ~selector:".toast"

let%expect_test "the model chip names the adapter; a harness session's does not"
  =
  let h = session () in
  H.names h ~selector:".model-chip";
  H.text h ~selector:".model-chip";
  let h = H.create () in
  H.names h ~selector:".model-chip";
  [%expect
    {|
    button "Model: Codex \194\183 Claude Opus 5.5, Medium" -- Codex · Claude Opus 5.5, reasoning Medium: models, reasoning and access (Ctrl+L)
    (Codex · Opus 5.5)
    button "Model: Claude Opus 5.5, Medium" -- Claude Opus 5.5, reasoning Medium: models, reasoning and access (Ctrl+L)
    |}]
;;

let%expect_test
    "/new starts a session on this one's engine; /new ENGINE on another"
  =
  let h = session () in
  H.type_ h "/new";
  H.act h Send;
  H.type_ h "/new claude";
  H.act h Send;
  H.type_ h "/new harness";
  H.act h Send;
  H.type_ h "/new gemini";
  H.act h Send;
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /new) (max 500)))
     (tag Ignore))
    (Rpc (method_ new_session) (params ((engine composer) (adapter codex)))
     (tag Reload_state))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/new claude") (max 500))) (tag Ignore))
    (Rpc (method_ new_session) (params ((engine composer) (adapter claude)))
     (tag Reload_state))
    (Focus editor)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/new harness") (max 500))) (tag Ignore))
    (Rpc (method_ new_session) (params ((engine harness))) (tag Reload_state))
    (Focus editor)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/new gemini") (max 500))) (tag Ignore))
    no engine "gemini" here: this backend runs harness, codex, claude (Dismiss)
    |}]
;;

let%expect_test "/new on a harness-only backend: as before" =
  let h = H.create () in
  H.quiet h (fun () ->
    hello ~deployed:Prigh_protocol.Deployed_engines.harness_only h);
  H.type_ h "/new";
  H.act h Send;
  H.type_ h "/new codex";
  H.act h Send;
  H.type_ h "/engine";
  H.act h Send;
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /new) (max 500)))
     (tag Ignore))
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/new codex") (max 500))) (tag Ignore))
    (Rpc (method_ push_pref) (params ((key history) (value /engine) (max 500)))
     (tag Ignore))
    This backend runs no composer adapters: start it with -engines harness,composer -adapters codex,claude to switch between them. (Dismiss)
    |}]
;;

let%expect_test "the new-session screen's engine chip picks the engine" =
  let h = session ~fields:[ "message_count", `Number "0" ] () in
  H.names h ~selector:".tray-engine";
  H.click h ~selector:".tray-engine";
  H.text h ~selector:".modal";
  H.act h (Picker_query "claude");
  H.act h Dialog_accept;
  [%expect
    {|
    button "Engine: Codex. Change it" -- Runs on Codex: start it on another engine (/new ENGINE)
    Pick_engine
    (Focus picker-input)
    New session on
    (Close (Esc))
    []
    Codex OpenAI's Codex CLI on the session's tool host ✓
    prigh harness prigh's own agent, calling the model directly
    Claude Code Anthropic's Claude Code CLI on the session's tool host
    ↑↓ to move, Enter to choose, Esc to close
    (Focus editor)
    (Rpc (method_ new_session) (params ((engine composer) (adapter claude)))
     (tag Reload_state))
    (Focus editor)
    |}]
;;

let%expect_test "/engine switches the adapter (set_adapter)" =
  let h = session () in
  H.type_ h "/engine";
  H.act h Send;
  H.text h ~selector:".modal";
  H.act h Dialog_accept;
  H.type_ h "/engine claude";
  H.act h Send;
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /engine) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    Switch this session to
    (Close (Esc))
    []
    Codex OpenAI's Codex CLI on the session's tool host ✓
    prigh harness prigh's own agent, calling the model directly
    Claude Code Anthropic's Claude Code CLI on the session's tool host
    ↑↓ to move, Enter to choose, Esc to close
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/engine claude") (max 500))) (tag Ignore))
    (Rpc (method_ set_adapter) (params ((adapter claude))) (tag Show_error))
    (Expire_toast (id 1) (after_ms 2580))
    Already on Codex
    Switching to Claude Code…
    |}]
;;

let%expect_test "what the engine cannot do is refused before anything opens" =
  let h = session () in
  H.quiet h (fun () ->
    List.iter
      [ "/rewind"; "/fork"; "/compact"; "/trim"; "/btw why?"; "/plan on" ]
      ~f:(fun c ->
        H.type_ h c;
        H.act h Send;
        printf "%s: " c;
        toasts h;
        H.act h Close_dialog));
  [%expect
    {|
    /rewind: Codex sessions can't rewind in place: fork from that message instead (/fork) (Dismiss)
    /fork: /compact: Codex compacts its context by itself when it fills: /compact isn't available (Dismiss)
    /trim: Codex sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one (Dismiss)
    /btw why?: Codex sessions have no side questions (/btw): ask in the session instead (Dismiss)
    /plan on: Codex sessions have no plan mode: ask for a plan in your message instead (Dismiss)
    |}]
;;

let%expect_test "a message's menu: trim and delete disabled, with why" =
  let h = session () in
  H.quiet h (fun () ->
    H.act
      h
      (Reply
         ( H.newest "s1"
         , Ok
             (Jsonaf.of_string
                (H.page
                   {|[{"role":"user","text":"question"},{"role":"assistant","content":[{"type":"text","text":"answer"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}]|}))
         )));
  H.act
    h
    (Open_menu { target = Message 1; anchor = Point { x = 300; y = 120 } });
  H.names h ~selector:".context-menu [role=menuitem]";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    [menuitem] div "Copy"
    [menuitem] div "Trim everything before this Codex sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one"
    [menuitem] div "Trim everything after this Codex sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one"
    [menuitem] div "Delete Codex sessions can't trim or delete messages, the agent keeps its own context: start a new session (/new) for a fresh one"
    |}]
;;

let%expect_test "the modes: no plan mode where the engine has none" =
  let h = session () in
  H.type_ h "/mode";
  H.act h Send;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /mode) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    When tools ask first (Alt+M cycles)
    (Close (Esc))
    []
    Asks: edits & commands bash, job_input, edits and MCP tools ask first
    Asks: commands file edits run; bash, job_input and MCP tools ask first
    Smart: a model decides edits in the project run; the approval model lets other calls run or asks you, saying why (/approval-model)
    Auto: never asks tools run without asking ✓
    Chat: no tool host a chat without the tool host: no files, commands or project, only the web and other host-free tools, until you leave it (/chat off or Alt+M)
    ↑↓ to move, Enter to choose, Esc to close
    |}]
;;

let%expect_test
    "a session that cannot run here: the banner says why; Send is off"
  =
  let h =
    session
      ~unavailable:
        "Codex is not installed on srv: install it there (npm i -g \
         @openai/codex), then retry"
      ()
  in
  H.text h ~selector:".engine-banner";
  H.type_ h "hello";
  H.names h ~selector:".send";
  H.act h Send;
  toasts h;
  H.click h ~selector:".engine-banner-new";
  [%expect
    {|
    Codex is not installed on srv: install it there (npm i -g @openai/codex), then retry (New session)
    button "Send" -- This session can't run here: see above
    icon "(no name)"
    (Rpc (method_ push_pref) (params ((key history) (value hello) (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    Codex is not installed on srv: install it there (npm i -g @openai/codex), then retry (Dismiss)
    New_session
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Focus editor)
    |}]
;;

let%expect_test "a Codex session that reported no cost runs on a subscription" =
  let show h = H.names h ~selector:".context-ring-anchor > button" in
  show (session ());
  show (session ~fields:[ "cost_usd", `Number "0.5" ] ());
  show (H.create ());
  [%expect
    {|
    button "Context 0% full, subscription: details"
    button "Context 0% full, $0.50: details"
    button "Context 0% full, $0.00: details"
    |}]
;;

let%expect_test "the picker lists what the CLI's login offers, when it says" =
  let h = session () in
  let codex ~id ~name = H.model_json ~provider:"codex" ~id ~name () in
  (* Before Codex has said: only its default. *)
  H.quiet h (fun () ->
    H.event h {|{"type":"event","event":"models_changed"}|};
    H.reply
      h
      "list_models"
      (sprintf "[%s]" (codex ~id:"default" ~name:"Default")));
  H.key h "l" ~ctrl:true;
  H.names h ~selector:".model-rail-tab";
  H.text h ~selector:".model-list";
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    [tab] button "Favourites"
    [tab] button "Codex"
    Default Alt+1 (Star: into the favourites (Alt+F))
    |}];
  (* Codex listed its models (prigh has no OpenAI login): asked again, the
     open menu shows them. *)
  H.event h {|{"type":"event","event":"models_changed"}|};
  H.reply
    h
    "list_models"
    (sprintf
       "[%s,%s,%s]"
       (codex ~id:"default" ~name:"Default (GPT-6.2-Sol)")
       (codex ~id:"gpt-6.2-sol" ~name:"GPT-6.2-Sol")
       (codex ~id:"gpt-6.2-mini" ~name:"GPT-6.2-Mini"));
  H.text h ~selector:".model-list";
  [%expect
    {|
    (Rpc (method_ list_models) (params ()) (tag Models))
    Default (GPT-6.2-Sol) Alt+1 (Star: into the favourites (Alt+F))
    GPT-6.2-Sol Alt+2 (Star: into the favourites (Alt+F))
    GPT-6.2-Mini Alt+3 (Star: into the favourites (Alt+F))
    |}]
;;
