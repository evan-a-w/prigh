open! Core
module H = Harness

(* The model, the thinking level and the approval mode beside each other
   along the bottom of the composer's box: always shown, each opening its
   picker. The top bar has the session and, in its middle, the status. *)

let controls h = H.text h ~selector:".composer-controls"

let%expect_test "the thinking chip names the level and opens its picker" =
  let h = H.create () in
  controls h;
  H.show h ~selector:".thinking-chip";
  [%expect
    {|
    (Claude Opus 5.5) (Thinking on) (Auto: never asks)
    <button type="button"
            title="Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)"
            class="btn chip-button thinking-chip"
            @on_click>
      <icon class="brain"> </icon>
      <span class="chip-key"> Thinking </span>
      <span class="chip-label"> on </span>
      <icon class="chevron"> </icon>
    </button>
    |}];
  H.click h ~selector:".thinking-chip";
  H.text h ~selector:".modal";
  [%expect
    {|
    Open_thinking_picker
    (Focus picker-input)
    Thinking level
    (Close (Esc))
    []
    off answer straight away
    low a little
    on the provider's default budget ✓
    high more
    max as much as the model allows
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  (* Esc closes it without a change. *)
  H.key h "Escape" ~target:Field;
  controls h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Claude Opus 5.5) (Thinking on) (Auto: never asks)
    |}];
  (* Alt+T: the next level shows at once, waiting on the backend. *)
  H.key h "t" ~alt:true;
  controls h;
  H.elements h ~selector:".thinking-chip";
  [%expect
    {|
    Cycle_thinking
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    (Claude Opus 5.5) (Thinking high) (Auto: never asks)
    <button class="awaiting btn chip-button thinking-chip">
    |}];
  H.text h ~selector:".thinking-chip .chip-label";
  [%expect {| Thinking high |}];
  H.reply h "set_thinking" "{}";
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "thinking", `String "high" ] ()));
  controls h;
  [%expect {| (Claude Opus 5.5) (Thinking high) (Auto: never asks) |}]
;;

let%expect_test "a model without thinking says so, and the chip tells why" =
  let deepseek =
    Jsonaf.of_string
      (H.model_json
         ~thinking:false
         ~provider:"deepseek"
         ~id:"deepseek-chat"
         ~name:"DeepSeek Chat"
         ())
  in
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:[ "model", deepseek; "thinking", `String "off" ]
           ())
      ()
  in
  controls h;
  [%expect {| (DeepSeek Chat) (No thinking) (Auto: never asks) |}];
  H.click h ~selector:".thinking-chip";
  H.elements h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    Open_thinking_picker
    DeepSeek Chat has no thinking levels: switch to a model that thinks with /model (Dismiss)
    |}]
;;

let%expect_test "every approval mode, as the chip names it" =
  List.iter
    [ "ask", false; "auto-edit", false; "auto", false; "ask", true ]
    ~f:(fun (mode, plan) ->
      let h =
        H.create
          ~state:
            (H.state_json
               ~fields:
                 [ "approval_mode", `String mode
                 ; ("plan_mode", if plan then `True else `False)
                 ]
               ())
          ()
      in
      H.text h ~selector:".mode-chip";
      H.elements h ~selector:".mode-chip");
  [%expect
    {|
    (Asks: edits & commands)
    <button class="btn chip-button mode-chip">
    (Asks: commands)
    <button class="btn chip-button mode-chip">
    (Auto: never asks)
    <button class="btn chip-button mode-chip">
    (Plan mode)
    <button class="btn chip-button mode-chip plan-mode">
    |}]
;;

let%expect_test "the top bar: title, cwd and branch, the status, the panels" =
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "cwd", `String "/home/ada/projects/prigh/tui"
             ; "git_branch", `String "feature/narrow-top-bar"
             ; "plan_mode", `True
             ]
           ())
      ()
  in
  H.show h ~selector:".topbar";
  [%expect
    {|
    <header class="topbar">
      <div class="topbar-start">
        <button type="button"
                title="Sessions (Ctrl+B)"
                aria-label="Sessions"
                class="btn ghost icon toggle-sidebar"
                @on_click>
          <icon class="menu"> </icon>
        </button>
        <div class="title">
          <button type="button" title="Rename (/name)" class="session-name" @on_click>
            <span dir="auto"> New session </span>
            <icon class="pencil"> </icon>
          </button>
          <div class="session-where">
            <span title="/home/ada/projects/prigh/tui" class="where">
              <span class="where-head"> ~/projects/prigh </span>
              <span class="where-tail"> /tui </span>
            </span>
            <span title="feature/narrow-top-bar" class="branch">
              <icon class="branch"> </icon>
              <span class="branch-name"> feature/narrow-top-bar </span>
            </span>
          </div>
        </div>
      </div>
      <div class="status">
        <button type="button"
                title="Context: 0 of 200k tokens (/compact frees some). Click for the session's details (/session)"
                aria-label="Context 0% full"
                class="context link status-item"
                @on_click>
          <span class="meter">
            <span class="meter-fill" style={ width: 0.00%; }> </span>
          </span>
          <span class="percent"> 0% </span>
        </button>
        <span title="Tokens: 0 in, 0 out, 0 cache read" class="status-item tokens"> ↑0 ↓0 </span>
        <span title="Session cost" class="cost status-item"> $0.0000 </span>
        <span role="status" aria-atomic="true" class="activity idle status-item">
          <span class="idle-dot"> </span>
          <span class="label"> Ready </span>
        </span>
      </div>
      <div class="topbar-end">
        <div class="status-extras">
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
        <div class="panel-toggles">
          <button type="button"
                  title="Files (Ctrl+Shift+F)"
                  aria-label="Files"
                  aria-expanded="false"
                  class="btn files-toggle ghost icon"
                  @on_click>
            <icon class="folder"> </icon>
          </button>
          <button type="button"
                  title="Terminal (Ctrl+`)"
                  aria-label="Terminal"
                  aria-expanded="false"
                  class="btn ghost icon terminal-toggle"
                  @on_click>
            <icon class="terminal"> </icon>
          </button>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <button type="button"
                  title="Settings (Ctrl+, or /settings)"
                  aria-label="Settings"
                  aria-haspopup="dialog"
                  class="btn ghost icon settings-toggle"
                  @on_click>
            <icon class="gear"> </icon>
          </button>
        </div>
      </div>
    </header>
    |}]
;;

let%expect_test "the cwd's last directory is kept apart: the rest is elided" =
  List.iter
    [ "/work"; "/"; "/home/ada"; "/home/ada/src/a/b/c"; "relative/path" ]
    ~f:(fun cwd ->
      let h =
        H.create ~state:(H.state_json ~fields:[ "cwd", `String cwd ] ()) ()
      in
      H.show h ~selector:".topbar .where");
  [%expect
    {|
    <span title="/work" class="where">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <span class="where-tail"> /work </span>
    </span>
    <span title="/" class="where">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <span class="where-tail"> / </span>
    </span>
    <span title="/home/ada" class="where">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <span class="where-tail"> ~ </span>
    </span>
    <span title="/home/ada/src/a/b/c" class="where">
      <span class="where-head"> ~/src/a/b </span>
      <span class="where-tail"> /c </span>
    </span>
    <span title="relative/path" class="where">
      <span class="where-head"> relative </span>
      <span class="where-tail"> /path </span>
    </span>
    |}]
;;

let%expect_test
    "the composer is one box: the editor, then the chips and the buttons along \
     its bottom"
  =
  let h = H.create () in
  H.elements h ~selector:".composer-box > *";
  H.elements h ~selector:".composer-bar > *";
  H.elements h ~selector:".composer-bar .btn";
  [%expect
    {|
    <Vdom.Node.none-widget class="">
    <Vdom.Node.none-widget class="">
    <textarea class="editor">
    <div class="composer-bar">
    <div class="composer-controls">
    <div class="composer-buttons">
    <button class="btn chip-button model-chip">
    <button class="btn chip-button thinking-chip">
    <button class="btn chip-button mode-chip">
    <button class="attach btn ghost icon">
    <button class="btn primary send">
    |}];
  (* The model's chip opens its picker; so does the mode's. *)
  H.click h ~selector:".model-chip";
  H.text h ~selector:".modal h2";
  H.key h "Escape" ~target:Field;
  H.click h ~selector:".mode-chip";
  H.text h ~selector:".modal h2";
  [%expect
    {|
    Open_model_picker
    (Focus picker-input)
    Switch model
    Close_dialog
    (Focus editor)
    (Run /mode)
    (Focus picker-input)
    When tools ask first (Alt+M cycles)
    |}];
  (* Running: stop beside steer. *)
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "running", `True ] ()));
  H.key h "Escape" ~target:Field;
  H.names h ~selector:".composer-buttons .btn";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    button "Attach files" -- Attach files (or drop them here)
    button "Stop" -- Stop (Esc)
    button "Steer" -- Steer (Enter)
    |}];
  (* Nothing is left under the box. *)
  H.elements h ~selector:".composer .status";
  H.elements h ~selector:".composer .notify";
  [%expect {| |}]
;;

let%expect_test "the status in the top bar: idle, running, and what applies" =
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "context_tokens", `Number "130000"
             ; "cost_usd", `Number "0.42"
             ; ( "usage"
               , Jsonaf.of_string
                   {|{"input":120000,"output":9000,"cache_read":0}|} )
             ]
           ())
      ()
  in
  let status () =
    H.text h ~selector:".topbar .status";
    H.text h ~selector:".topbar .status-extras"
  in
  status ();
  [%expect {| (65%) ↑120k ↓9.0k $0.4200 Ready |}];
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ "running", `True
            ; "context_tokens", `Number "180000"
            ; ( "goal"
              , Jsonaf.of_string
                  {|{"text":"all tests pass","status":"active","continuations":2,"max_continuations":20}|}
              )
            ]
          ()));
  H.event
    h
    {|{"event":"queue_update","steer":0,"follow_up":1,"steer_texts":[],"follow_up_texts":["and the docs"]}|};
  status ();
  H.elements h ~selector:".topbar .status > .status-item";
  H.elements h ~selector:".topbar .status-extras > .status-item";
  (* Last, in a slot of its own width (style.css), saying it all in its
     title. *)
  H.show h ~selector:".topbar .status > .activity";
  [%expect
    {|
    (90%) ↑0 ↓0 $0.0000 Working
    (Goal 2/20) (1 queued)
    <button class="context hot link status-item">
    <span class="status-item tokens">
    <span class="cost status-item">
    <span class="activity running status-item">
    <button class="goal link status-item warm">
    <button class="link queued status-item">
    <span title="Working" role="status" aria-atomic="true" class="activity running status-item">
      <span aria-hidden="true" class="lamp"> </span>
      <span class="label"> Working </span>
    </span>
    |}];
  (* The context opens the session's details, with the counters. *)
  H.click h ~selector:".topbar .context";
  [%expect
    {|
    (Run /session)
    (Focus dialog)
    (Rpc (method_ session_stats) (params ()) (tag Session_stats))
    |}];
  (* Waiting for the backend, the bar says so where the status goes. *)
  let h = H.connecting () in
  H.text h ~selector:".topbar";
  [%expect
    {|
    (Sessions (Ctrl+B))
    Connecting…
    (Settings (Ctrl+, or /settings))
    |}]
;;

(* A phone: the same chips, with the short names CSS shows (style.css's
   container queries), and the status beside the title. *)
let%expect_test "narrow: the chips' short names, the shorter placeholder" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.elements h ~selector:".app";
  H.show h ~selector:".mode-chip";
  H.show h ~selector:"#editor";
  [%expect
    {|
    <div class="app narrow">
    <button type="button"
            title="Tools run without asking (Alt+M cycles, /mode picks)"
            data-short="auto"
            class="btn chip-button mode-chip"
            @on_click>
      <icon class="shield"> </icon>
      <span class="chip-label"> Auto: never asks </span>
      <icon class="chevron"> </icon>
    </button>
    <textarea id="editor"
              placeholder="Ask prigh…  / commands"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}]
;;
