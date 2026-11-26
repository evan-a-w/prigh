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
  (* Before Codex has said: its placeholder. *)
  H.quiet h (fun () ->
    H.event h {|{"type":"event","event":"models_changed"}|};
    H.reply
      h
      "list_models"
      (sprintf "[%s]" (codex ~id:"default" ~name:"loading models…")));
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
    loading models… Alt+1 (Star: into the favourites (Alt+F))
    |}];
  (* Codex listed its models (prigh has no OpenAI login): asked again, the
     open menu shows them. *)
  H.event h {|{"type":"event","event":"models_changed"}|};
  H.reply
    h
    "list_models"
    (sprintf
       "[%s,%s,%s]"
       (codex ~id:"gpt-6.2-sol" ~name:"GPT-6.2-Sol")
       (codex ~id:"gpt-6.2-mini" ~name:"GPT-6.2-Mini")
       (codex ~id:"gpt-6.2-max" ~name:"GPT-6.2-Max"));
  H.text h ~selector:".model-list";
  [%expect
    {|
    (Rpc (method_ list_models) (params ()) (tag Models))
    GPT-6.2-Sol Alt+1 (Star: into the favourites (Alt+F))
    GPT-6.2-Mini Alt+2 (Star: into the favourites (Alt+F))
    GPT-6.2-Max Alt+3 (Star: into the favourites (Alt+F))
    |}]
;;

let%expect_test "picking a CLI's model asks for no prigh login" =
  let h = session () in
  H.quiet h (fun () ->
    H.event h {|{"type":"event","event":"models_changed"}|};
    H.reply
      h
      "list_models"
      (H.model_json ~provider:"codex" ~id:"gpt-6.2-sol" ~name:"GPT-6.2-Sol" ()
       |> sprintf "[%s]"));
  H.act h (Run "/model");
  H.act h (Picker_choose "codex/gpt-6.2-sol");
  H.text h ~selector:".toast";
  [%expect
    {|
    (Focus picker-input)
    (Focus editor)
    (Rpc (method_ set_model) (params ((model codex/gpt-6.2-sol)))
     (tag Model_set))
    |}]
;;

let%expect_test
    "the picker follows the engine: Antigravity's models, then Codex's"
  =
  let agy_fields =
    [ "engine", `String "composer"
    ; "adapter", `String "acp:antigravity"
    ; "capabilities", codex_capabilities
    ; "message_count", `Number "0"
    ; ( "model"
      , Jsonaf.of_string
          (H.model_json
             ~thinking:false
             ~provider:"acp:antigravity"
             ~id:"flash"
             ~name:"Gemini 3.8 Flash"
             ()) )
    ]
  in
  let h = H.create ~state:(H.state_json ~fields:agy_fields ()) () in
  H.quiet h (fun () ->
    hello
      ~deployed:
        { engines = [ "harness"; "composer" ]
        ; adapters = [ "acp:antigravity"; "codex" ]
        }
      h;
    H.event h {|{"type":"event","event":"models_changed"}|};
    H.reply
      h
      "list_models"
      (sprintf
         "[%s,%s,%s]"
         (H.model_json
            ~thinking:false
            ~provider:"acp:antigravity"
            ~id:"flash"
            ~name:"Gemini 3.8 Flash"
            ())
         (H.model_json
            ~thinking:false
            ~provider:"acp:antigravity"
            ~id:"gpt-oss"
            ~name:"GPT-OSS 120B (Medium)"
            ())
         (H.model_json
            ~thinking:false
            ~provider:"acp:antigravity"
            ~id:"sonnet"
            ~name:"Claude Sonnet 4.6 (Thinking)"
            ())));
  let picker () =
    H.quiet h (fun () -> H.key h "l" ~ctrl:true);
    H.names h ~selector:".model-rail-tab";
    H.text h ~selector:".model-list";
    H.text h ~selector:".model-reasoning";
    H.quiet h (fun () -> H.key h "Escape")
  in
  picker ();
  [%expect
    {|
    (Open_chip Model)
    [tab] button "Favourites"
    [tab] button "Antigravity"
    Gemini 3.8 Flash Alt+1 (Star: into the favourites (Alt+F)) ✓
    GPT-OSS 120B (Medium) Alt+2 (Star: into the favourites (Alt+F))
    Claude Sonnet 4.6 (Thinking) Alt+3 (Star: into the favourites (Alt+F))
    Reasoning Gemini 3.8 Flash
    Antigravity chooses how much it reasons.
    Close_dialog
    |}];
  (* The engine chip starts a new session on Codex: none of Antigravity's
     models are offered meanwhile, and Codex's are asked. *)
  H.quiet h (fun () ->
    H.click h ~selector:".tray-engine";
    H.act h (Picker_query "codex");
    H.act h Dialog_accept);
  H.reply
    h
    "new_session"
    (sprintf
       {|{"state":%s}|}
       (H.state_json
          ~fields:
            (codex_fields ()
             @ [ "session_id", `String "s2"
               ; "message_count", `Number "0"
               ; ( "model"
                 , Jsonaf.of_string
                     (H.model_json
                        ~provider:"codex"
                        ~id:"gpt-6.2-sol"
                        ~name:"GPT-6.2-Sol"
                        ()) )
               ])
          ()));
  picker ();
  H.reply
    h
    "list_models"
    (sprintf
       "[%s]"
       (H.model_json ~provider:"codex" ~id:"gpt-6.2-sol" ~name:"GPT-6.2-Sol" ()));
  picker ();
  [%expect
    {|
    Pick_engine
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ get_review) (params ()) (tag (Review Review)))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    (Open_chip Model)
    [tab] button "Favourites"
    Reasoning GPT-6.2-Sol
    (Off) (Default Default ✓) (Minimal) (Low) (Medium) (High) (Extra High) (Max)
    Close_dialog
    (Open_chip Model)
    [tab] button "Favourites"
    [tab] button "Codex"
    GPT-6.2-Sol Alt+1 (Star: into the favourites (Alt+F)) ✓
    Reasoning GPT-6.2-Sol
    (Off) (Default Default ✓) (Minimal) (Low) (Medium) (High) (Extra High) (Max)
    Close_dialog
    |}]
;;

let%expect_test "the model menu's foot switches the engine" =
  let deployed : Prigh_protocol.Deployed_engines.t =
    { engines = [ "harness"; "composer" ]
    ; adapters = [ "codex"; "acp:antigravity" ]
    }
  in
  let h = session ~deployed ~fields:[ "message_count", `Number "4" ] () in
  H.quiet h (fun () -> H.key h "l" ~ctrl:true);
  H.text h ~selector:".model-menu-foot";
  H.names h ~selector:".model-engine";
  [%expect
    {|
    (Open_chip Model)
    ↑ ↓ models · ← → providers · Enter switches · Esc closes
    (Full access)
    (Codex)
    div "Codex"
    |}];
  (* Opened, it asks the adapters' health, shown on each. *)
  H.click h ~selector:".model-engine .model-access-button";
  H.quiet h (fun () ->
    H.reply
      h
      "list_adapters"
      {|{"adapters":[
        {"adapter":"codex","label":"Codex","host":"backend","host_name":"desktop","installed":true,"auth":"logged_in","account":"ChatGPT","version":"0.166.0","checked_at":1791194400},
        {"adapter":"acp:antigravity","label":"Antigravity","host":"backend","host_name":"desktop","installed":true,"auth":"unknown","version":"1.5.13","checked_at":1791194400}]}|});
  H.text h ~selector:".model-engine .model-access-menu";
  [%expect
    {|
    Model_menu_engine
    (Rpc (method_ list_adapters) (params ()) (tag Adapters_loaded))
    (Codex 0.166.0 · logged in (ChatGPT) ✓) (prigh harness prigh's own agent, calling the model directly) (A Antigravity 1.5.13)
    |}];
  (* Choosing one switches the session; the picker stays, and lists the new
     adapter's models once the backend says they changed. *)
  H.click h ~nth:2 ~selector:".model-engine .model-access-item";
  H.names h ~selector:".model-engine .model-access-menu";
  H.quiet h (fun () ->
    H.event
      h
      (sprintf
         {|{"event":"state","state":%s}|}
         (H.state_json
            ~fields:
              [ "engine", `String "composer"
              ; "adapter", `String "acp:antigravity"
              ; "capabilities", codex_capabilities
              ; "message_count", `Number "4"
              ]
            ())));
  H.event h {|{"type":"event","event":"models_changed"}|};
  H.reply
    h
    "list_models"
    (sprintf
       "[%s]"
       (H.model_json
          ~thinking:false
          ~provider:"acp:antigravity"
          ~id:"flash"
          ~name:"Gemini 3.8 Flash"
          ()));
  H.text h ~selector:".model-menu-main";
  H.names h ~selector:".model-engine .model-access-button";
  [%expect
    {|
    (Model_menu_engine_pick acp:antigravity)
    (Rpc (method_ set_adapter) (params ((adapter acp:antigravity)))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 2580))
    (Rpc (method_ list_models) (params ()) (tag Models))
    []
    Antigravity
    Gemini 3.8 Flash Alt+1 (Star: into the favourites (Alt+F))
    button "Engine: Antigravity" -- Runs on Antigravity: switch the engine (Alt+E)
    |}];
  (* Esc closes its menu first; Alt+E is the keyboard's picker. *)
  H.quiet h (fun () -> H.click h ~selector:".model-engine .model-access-button");
  H.key h "Escape";
  H.elements h ~selector:".model-engine .model-access-menu";
  H.names h ~selector:"#chip-popover";
  H.key h "e" ~alt:true;
  H.text h ~selector:".modal";
  [%expect
    {|
    Model_menu_engine
    Close_dialog
    [dialog] div "Model"
    Model_menu_engine_picker
    (Focus picker-input)
    Switch this session to
    (Close (Esc))
    []
    Antigravity an ACP agent on the session's tool host ✓
    prigh harness prigh's own agent, calling the model directly
    Codex OpenAI's Codex CLI on the session's tool host
    ↑↓ to move, Enter to choose, Esc to close
    |}]
;;

let%expect_test "mid-run the engine waits for the turn" =
  let h =
    session ~fields:[ "message_count", `Number "4"; "running", `True ] ()
  in
  H.quiet h (fun () -> H.key h "l" ~ctrl:true);
  H.names h ~selector:".model-engine .model-access-button";
  H.click h ~selector:".model-engine .model-access-button";
  H.text h ~selector:".model-engine .model-access-menu";
  H.elements h ~selector:".model-engine .model-access-item[disabled]";
  H.key h "e" ~alt:true;
  toasts h;
  [%expect
    {|
    (Open_chip Model)
    button "Engine: Codex" -- Switch after this turn (Esc stops it)
    Model_menu_engine
    (Rpc (method_ list_adapters) (params ()) (tag Adapters_loaded))
    Switch after this turn (Esc stops it)
    (Codex OpenAI's Codex CLI on the session's tool host ✓) (prigh harness prigh's own agent, calling the model directly) (Claude Code Anthropic's Claude Code CLI on the session's tool host)
    <button class="model-access-item on">
    <button class="model-access-item">
    <button class="model-access-item">
    Model_menu_engine_picker
    (Expire_toast (id 0) (after_ms 2980))
    Switch after this turn (Esc stops it)
    |}]
;;

let%expect_test "Alt+E with one engine says how to have more" =
  let h = H.create () in
  H.quiet h (fun () ->
    hello ~deployed:Prigh_protocol.Deployed_engines.harness_only h;
    H.key h "l" ~ctrl:true);
  H.key h "e" ~alt:true;
  toasts h;
  [%expect
    {|
    (Open_chip Model)
    Model_menu_engine_picker
    (Expire_toast (id 0) (after_ms 6000))
    This backend runs one engine: start it with more (-engines, -adapters) to switch here; Settings → Engines shows them
    |}]
;;

(* A reply's footer names its model as the chip does, not by the key the
   agent CLI's replies carry; a model it does not know keeps its key. *)
let%expect_test "an agent CLI's reply names its model in the footer" =
  let h =
    session
      ~fields:
        [ "adapter", `String "acp:antigravity"
        ; ( "model"
          , Jsonaf.of_string
              (H.model_json
                 ~thinking:false
                 ~provider:"acp:antigravity"
                 ~id:"flash"
                 ~name:"Gemini 3.8 Flash"
                 ()) )
        ]
      ()
  in
  H.quiet h (fun () ->
    H.event h {|{"type":"event","event":"models_changed"}|};
    H.reply
      h
      "list_models"
      (sprintf
         "[%s,%s]"
         (H.model_json
            ~thinking:false
            ~provider:"acp:antigravity"
            ~id:"flash"
            ~name:"Gemini 3.8 Flash"
            ())
         (H.model_json
            ~thinking:false
            ~provider:"acp:antigravity"
            ~id:"sonnet"
            ~name:"Claude Sonnet 4.6 (Thinking)"
            ())));
  let reply model =
    H.quiet h (fun () ->
      H.event
        h
        (sprintf
           {|{"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"No."}],"stop_reason":{"type":"end_turn"},"usage":{"input":1200,"output":30,"cache_read":0},"model":"%s"}}|}
           model))
  in
  reply "acp:antigravity/flash";
  reply "acp:antigravity/sonnet";
  reply "acp:antigravity/gone";
  H.text h ~selector:".meta";
  [%expect
    {|
    Gemini 3.8 Flash 1.2k in 30 out
    Claude Sonnet 4.6 (Thinking) 1.2k in 30 out
    acp:antigravity/gone 1.2k in 30 out
    |}]
;;
