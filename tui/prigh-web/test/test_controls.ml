open! Core
module H = Harness

(* The model, the thinking level and the approval mode beside each other in
   the top bar: always shown, each opening its picker. *)

let controls h = H.text h ~selector:".controls"

let%expect_test "the thinking chip names the level and opens its picker" =
  let h = H.create () in
  controls h;
  H.show h ~selector:".thinking-chip";
  [%expect
    {|
    (Claude Opus 5.5) (Thinking on) (Auto: never asks)
    <button type="button"
            title="Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)"
            popovertarget="topbar-controls"
            popovertargetaction="hide"
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

(* A narrow chat column folds the controls into a menu (style.css): the
   button that opens it names what they are set to, and each control closes
   it as it opens its picker. *)
let%expect_test "the top bar: title, cwd and branch, and the controls' menu" =
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
      <button type="button"
              title="Sessions (Ctrl+B)"
              aria-label="Sessions"
              class="btn ghost icon toggle-sidebar"
              @on_click>
        <icon class="menu"> </icon>
      </button>
      <div class="title">
        <button type="button" title="Rename (/name)" class="session-name" @on_click>
          <span> New session </span>
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
      <button type="button"
              title="Claude Opus 5.5, thinking on, plan mode"
              aria-label="Model, thinking and approvals"
              popovertarget="topbar-controls"
              class="btn controls-more ghost icon plan-mode"
              @on_click>
        <icon class="sliders"> </icon>
      </button>
      <div id="topbar-controls" popover="auto" class="controls">
        <button type="button"
                title="Claude Opus 5.5: switch model (Ctrl+L)"
                popovertarget="topbar-controls"
                popovertargetaction="hide"
                class="btn chip-button model-chip"
                @on_click>
          <icon class="cpu"> </icon>
          <span class="chip-label"> Claude Opus 5.5 </span>
          <icon class="chevron"> </icon>
        </button>
        <button type="button"
                title="Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)"
                popovertarget="topbar-controls"
                popovertargetaction="hide"
                class="btn chip-button thinking-chip"
                @on_click>
          <icon class="brain"> </icon>
          <span class="chip-key"> Thinking </span>
          <span class="chip-label"> on </span>
          <icon class="chevron"> </icon>
        </button>
        <button type="button"
                title="Read-only planning: no file edits; bash, job_input and MCP tools ask first (Alt+M cycles, /mode picks)"
                data-short="plan"
                popovertarget="topbar-controls"
                popovertargetaction="hide"
                class="btn chip-button mode-chip plan-mode"
                @on_click>
          <icon class="shield"> </icon>
          <span class="chip-label"> Plan mode </span>
          <icon class="chevron"> </icon>
        </button>
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
