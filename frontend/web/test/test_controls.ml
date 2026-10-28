open! Core
open Prigh_web
module H = Harness

(* The model, the thinking level and the approval mode beside each other
   along the bottom of the composer's box: always shown, each opening the
   picker its command opens. The session's status is in the docked
   sidebar's card, with the panels' toggles over the chat's corner, else in
   a slim bar over the chat. *)

let controls h = H.text h ~selector:".composer-controls"

let%expect_test "the thinking chip names the level and opens its menu over it" =
  let h = H.create () in
  controls h;
  H.show h ~selector:".thinking-chip";
  [%expect
    {|
    (Claude Opus 5.5)
    (Thinking on)
    (Auto: never asks)
    <button type="button"
            title="Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)"
            aria-haspopup="dialog"
            aria-expanded="false"
            class="btn chip-button thinking-chip"
            @on_click>
      <icon class="brain"> </icon>
      <span class="chip-key"> Thinking </span>
      <span class="chip-label"> on </span>
      <icon class="chevron"> </icon>
    </button>
    |}];
  (* Its menu: the levels, the current one checked, the keyboard's on the
     list. Not the command's dialog. *)
  H.click h ~selector:".thinking-chip";
  H.elements h ~selector:"#chip-popover";
  H.elements h ~selector:"#chip-popover > *";
  H.text h ~selector:"#chip-popover";
  H.show h ~selector:".popover-item.selected";
  H.elements h ~selector:".modal";
  H.show h ~selector:".thinking-chip";
  [%expect
    {|
    (Open_chip Thinking)
    (Focus popover-list)
    Place_popover
    <div class="chip-popover menu-surface">
    <div class="popover-head">
    <div class="popover-items">
    <div class="menu-keys">
    Thinking Alt+T cycles
    off answer straight away
    low a little
    ✓ on the provider's default budget
    high more
    max as much as the model allows
    Enter chooses, ← → the next setting, Esc closes
    <div role="option"
         id="popover-option-2"
         aria-selected="true"
         aria-checked="true"
         class="marked menu-item popover-item selected"
         @on_click>
      <span aria-hidden="true" class="mark picked popover-check">
        <icon class="check"> </icon>
      </span>
      <span class="menu-text">
        <span class="menu-label"> on </span>
        <span class="menu-detail"> the provider's default budget </span>
      </span>
      <span aria-hidden="true" class="thinking-meter">
        <span class="bar on"> </span>
        <span class="bar on"> </span>
        <span class="bar"> </span>
        <span class="bar"> </span>
      </span>
    </div>
    <button type="button"
            title="Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)"
            aria-haspopup="dialog"
            aria-expanded="true"
            aria-controls="chip-popover"
            class="btn chip-button thinking-chip"
            @on_click>
      <icon class="brain"> </icon>
      <span class="chip-key"> Thinking </span>
      <span class="chip-label"> on </span>
      <icon class="chevron"> </icon>
    </button>
    |}];
  (* Esc closes it without a change. *)
  H.key h "Escape" ~target:Field;
  controls h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Claude Opus 5.5)
    (Thinking on)
    (Auto: never asks)
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
    (Claude Opus 5.5)
    (Thinking high)
    (Auto: never asks)
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
  [%expect
    {|
    (Claude Opus 5.5)
    (Thinking high)
    (Auto: never asks)
    |}]
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
  [%expect
    {|
    (DeepSeek Chat)
    (No thinking)
    (Auto: never asks)
    |}];
  H.click h ~selector:".thinking-chip";
  H.elements h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Open_chip Thinking)
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

let%expect_test
    "the docked sidebar's card: status, host and branch; the panels' toggles \
     over the chat"
  =
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "cwd", `String "/home/ada/projects/prigh/tui"
             ; "git_branch", `String "feature/narrow-top-bar"
             ; "plan_mode", `True
             ; "active_host", `String "backend"
             ; ( "hosts"
               , Jsonaf.of_string
                   {|[{"id":"backend","name":"desktop","cwd":"/home/ada/projects/prigh/tui"}]|}
               )
             ]
           ())
      ()
  in
  (* Above the account, at the foot of the sidebar; over the chat only the
     panels' toggles. *)
  H.elements h ~selector:".sidebar > *";
  H.elements h ~selector:".main > *";
  H.show h ~selector:".chat-frame > .chat-tools";
  [%expect
    {|
    <div class="sidebar-head">
    <button class="btn new-session">
    <div class="search">
    <Vdom.Node.none-widget class="">
    <div class="sessions">
    <Vdom.Node.none-widget class="">
    <section class="session-card">
    <div class="sidebar-footer">
    <Vdom.Node.none-widget class="">
    <Vdom.Node.none-widget class="">
    <div class="chat-frame">
    <div class="toast-anchor">
    <Vdom.Node.none-widget class="">
    <footer class="composer">
    <div class="chat-tools">
      <div class="panel-toggles">
        <button type="button"
                title="Filter the transcript (/filter)"
                aria-label="Filter the transcript"
                id="filter-toggle"
                aria-haspopup="menu"
                aria-expanded="false"
                class="btn filter-toggle ghost icon"
                @on_click>
          <icon class="filter"> </icon>
        </button>
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
    </div>
    |}];
  H.show h ~selector:".session-card";
  [%expect
    {|
    <section aria-label="This session" class="session-card">
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
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </div>
      <div class="card-status">
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
        <div class="card-where">
          <button type="button"
                  title="Tools run on desktop, in /home/ada/projects/prigh/tui (/host switches)"
                  class="host link status-item"
                  @on_click>
            <icon class="server"> </icon>
            <span class="status-text"> desktop </span>
          </button>
          <button type="button"
                  title="On branch feature/narrow-top-bar, in /home/ada/projects/prigh/tui: pull requests (/pr)"
                  aria-haspopup="dialog"
                  class="branch"
                  @on_click>
            <icon class="branch"> </icon>
            <span class="branch-name"> feature/narrow-top-bar </span>
          </button>
        </div>
      </div>
    </section>
    |}];
  H.text h ~selector:".session-card";
  [%expect
    {|
    (0%) ↑0 ↓0 $0.0000 Ready
    (desktop) (feature/narrow-top-bar)
    |}]
;;

let%expect_test "the host opens the host picker, as /host does" =
  let hosts =
    {|[{"id":"backend","name":"desktop","cwd":"/work"},{"id":"c7","name":"laptop","cwd":"/home/ann/work","session_id":"s1","session_name":null}]|}
  in
  let h =
    H.create
      ~state:(H.state_json ~fields:[ "hosts", Jsonaf.of_string hosts ] ())
      ()
  in
  H.click h ~selector:".card-where .host";
  let clicked = (H.model h).dialog in
  H.text h ~selector:".modal";
  H.key h "Escape" ~target:Field;
  H.quiet h (fun () -> H.act h (Run "/host"));
  print_s [%sexp (Option.equal Dialog.equal clicked (H.model h).dialog : bool)];
  [%expect
    {|
    (Run /host)
    (Focus picker-input)
    Where tools run
    (Close (Esc))
    []
    desktop /work ✓
    laptop /home/ann/work
    ↑↓ to move, Enter to choose, Esc to close
    Close_dialog
    (Focus editor)
    true
    |}];
  (* Elsewhere than the backend, the header says so too; with no host at
     all, the card says that. *)
  H.key h "Escape" ~target:Field;
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ "hosts", Jsonaf.of_string hosts; "active_host", `String "c7" ]
          ()));
  H.text h ~selector:".card-where";
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.names h ~selector:".session-bar .host";
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:[ "hosts", `Array []; "active_host", `String "" ]
          ()));
  H.text h ~selector:".card-where";
  H.elements h ~selector:".card-where .host";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 2)))))
    (laptop) (main)
    button "laptop" -- Tools run on laptop, in /home/ann/work (/host switches)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \n\
                            \n/work") (purpose (Sidebar 3)))))
    (No host) (main)
    <button class="host link status-item warm">
    |}]
;;

let%expect_test "without the docked sidebar, a slim bar over the chat" =
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "cwd", `String "/home/ada/projects/prigh/tui"
             ; "git_branch", `String "main"
             ]
           ())
      ()
  in
  H.key h "b" ~ctrl:true;
  H.elements h ~selector:".session-card";
  H.elements h ~selector:".chat-tools";
  H.elements h ~selector:".main > *";
  H.show h ~selector:".session-bar";
  [%expect
    {|
    Toggle_sidebar
    <header class="session-bar">
    <Vdom.Node.none-widget class="">
    <div class="chat-frame">
    <div class="toast-anchor">
    <Vdom.Node.none-widget class="">
    <footer class="composer">
    <header class="session-bar">
      <button type="button"
              title="Sessions (Ctrl+B)"
              aria-label="Sessions"
              class="btn ghost icon toggle-sidebar"
              @on_click>
        <icon class="menu"> </icon>
      </button>
      <button type="button" title="Rename (/name)" class="session-name" @on_click>
        <span dir="auto"> New session </span>
        <icon class="pencil"> </icon>
      </button>
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
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
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
      <div class="panel-toggles">
        <button type="button"
                title="Filter the transcript (/filter)"
                aria-label="Filter the transcript"
                id="filter-toggle"
                aria-haspopup="menu"
                aria-expanded="false"
                class="btn filter-toggle ghost icon"
                @on_click>
          <icon class="filter"> </icon>
        </button>
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
      <button type="button"
              title="Settings (Ctrl+, or /settings)"
              aria-label="Settings"
              aria-haspopup="dialog"
              class="btn ghost icon settings-toggle"
              @on_click>
        <icon class="gear"> </icon>
      </button>
    </header>
    |}];
  H.text h ~selector:".session-bar";
  [%expect
    {|
    (Sessions (Ctrl+B)) (New session)
    (0%) ↑0 ↓0 $0.0000 Ready
    (Filter the transcript (/filter)) (Files (Ctrl+Shift+F)) (Terminal (Ctrl+`))
    (Settings (Ctrl+, or /settings))
    |}];
  (* Its toggle brings the sidebar back, and the card with it. *)
  H.click h ~selector:".session-bar .toggle-sidebar";
  H.elements h ~selector:".session-bar";
  H.elements h ~selector:".session-card";
  [%expect
    {|
    Toggle_sidebar
    <section class="session-card">
    |}];
  (* A phone: the sidebar is a drawer, the bar stays (under the drawer). *)
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.elements h ~selector:".session-bar";
  H.elements h ~selector:".session-card";
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.elements h ~selector:".session-bar";
  H.elements h ~selector:".session-card";
  H.elements h ~selector:".main > [inert]";
  [%expect
    {|
    <header class="session-bar">
    <header class="session-bar">
    <header class="session-bar">
    <div class="chat-frame">
    <footer class="composer">
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
  (* The model's chip opens its menu; so does the mode's. *)
  H.click h ~selector:".model-chip";
  H.text h ~selector:"#chip-popover .popover-search";
  H.key h "Escape" ~target:Field;
  H.click h ~selector:".mode-chip";
  H.text h ~selector:"#chip-popover .popover-head";
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    [] Ctrl+L
    Close_dialog
    (Focus editor)
    (Open_chip Mode)
    (Focus popover-list)
    Place_popover
    When tools ask first Alt+M cycles
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

let%expect_test
    "each chip's menu has its command's choices, and owns the keyboard"
  =
  let h = H.create () in
  List.iter
    [ ".model-chip", "/model"
    ; ".thinking-chip", "/thinking"
    ; ".mode-chip", "/mode"
    ]
    ~f:(fun (chip, command) ->
      H.quiet h (fun () -> H.click h ~selector:chip);
      let clicked = (H.model h).dialog in
      H.quiet h (fun () -> H.key h "Escape" ~target:Field);
      H.quiet h (fun () -> H.act h (Run command));
      let items = function
        | Some (Dialog.Picker { picker; _ } | Popover { picker; _ }) ->
          Some (List.map (Picker.visible picker) ~f:(fun i -> i.id))
        | _ -> None
      in
      printf
        "%s: %s, the same choices as %s: %b\n"
        chip
        (match clicked with
         | Some (Popover { chip; _ }) ->
           Sexp.to_string [%sexp (chip : Chip.t)] ^ "'s menu"
         | _ -> "no menu")
        command
        ([%equal: string list option]
           (items clicked)
           (items (H.model h).dialog));
      H.quiet h (fun () -> H.key h "Escape" ~target:Field));
  [%expect
    {|
    (Open_chip Model)
    Close_dialog
    .model-chip: Model's menu, the same choices as /model: true
    Close_dialog
    (Open_chip Thinking)
    Close_dialog
    .thinking-chip: Thinking's menu, the same choices as /thinking: true
    Close_dialog
    (Open_chip Mode)
    Close_dialog
    .mode-chip: Mode's menu, the same choices as /mode: true
    Close_dialog
    |}];
  (* Typing filters the models (fuzzily), Enter takes the choice, Esc leaves
     it as it was. *)
  H.click h ~selector:".model-chip";
  H.input h ~selector:"#popover-filter" "sonet";
  H.text h ~selector:".popover-items";
  H.key h "Enter" ~target:Field;
  H.elements h ~selector:"#chip-popover";
  controls h;
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    (Picker_query sonet)
    Claude Sonnet 5 Anthropic 200k
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_model) (params ((model anthropic/claude-sonnet-5)))
     (tag Model_set))
    (Claude Sonnet 5)
    (Thinking on)
    (Auto: never asks)
    |}];
  H.click h ~selector:".mode-chip";
  H.key h "ArrowDown" ~target:Page;
  H.text h ~selector:".popover-item.selected";
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:"#chip-popover";
  controls h;
  [%expect
    {|
    (Open_chip Mode)
    (Focus popover-list)
    Place_popover
    (Dialog_move 1)
    Plan mode read-only planning: no file edits; bash, job_input and MCP tools ask first
    Close_dialog
    (Focus editor)
    (Claude Sonnet 5)
    (Thinking on)
    (Auto: never asks)
    |}];
  (* Clicking a choice takes it. *)
  H.click h ~selector:".thinking-chip";
  H.click h ~selector:".popover-item" ~nth:3;
  controls h;
  [%expect
    {|
    (Open_chip Thinking)
    (Focus popover-list)
    Place_popover
    (Picker_choose high)
    (Focus editor)
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    (Claude Sonnet 5)
    (Thinking high)
    (Auto: never asks)
    |}]
;;

let%expect_test
    "the model's menu: under each provider, the context window; filtered, best \
     first"
  =
  let h = H.create () in
  H.key h "l" ~ctrl:true;
  H.text h ~selector:"#chip-popover";
  H.show h ~selector:".popover-item.selected";
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    [] Ctrl+L
    Anthropic
    ✓ Claude Opus 5.5 200k
    Claude Sonnet 5 200k
    OpenAI not logged in: /login
    GPT-6 200k
    DeepSeek not logged in: /login
    DeepSeek Chat 200k
    Enter chooses, ← → the next setting, Esc closes
    <div role="option"
         id="popover-option-0"
         aria-selected="true"
         aria-checked="true"
         class="marked menu-item popover-item selected"
         @on_click>
      <span aria-hidden="true" class="mark picked popover-check">
        <icon class="check"> </icon>
      </span>
      <span class="menu-text">
        <span class="menu-label"> Claude Opus 5.5 </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </span>
      <span title="Context window, in tokens" class="popover-aside"> 200k </span>
    </div>
    |}];
  H.input h ~selector:"#popover-filter" "chat";
  H.text h ~selector:".popover-items";
  [%expect
    {|
    (Picker_query chat)
    DeepSeek Chat DeepSeek, not logged in 200k
    ✓ Claude Opus 5.5 Anthropic 200k
    Claude Sonnet 5 Anthropic 200k
    |}];
  H.input h ~selector:"#popover-filter" "zz";
  H.text h ~selector:".popover-empty";
  [%expect
    {|
    (Picker_query zz)
    Nothing matches “zz”: Backspace widens the search.
    |}];
  (* Emptied, the highlight is back on the current model. *)
  H.input h ~selector:"#popover-filter" "";
  H.text h ~selector:".popover-item.selected";
  [%expect
    {|
    (Picker_query "")
    ✓ Claude Opus 5.5 200k
    |}];
  (* Ctrl+L again closes it. *)
  H.key h "l" ~ctrl:true ~target:Field;
  H.elements h ~selector:"#chip-popover";
  [%expect
    {|
    (Open_chip Model)
    (Focus editor)
    |}]
;;

let%expect_test
    "one chip's menu at a time: ← → and Tab go along the chips, a click on \
     another opens its, on the same one or beside the menu closes it"
  =
  let h = H.create () in
  let shown () =
    match (H.model h).dialog with
    | Some (Popover { chip; _ }) -> print_s [%sexp (chip : Chip.t)]
    | Some _ -> print_endline "another dialog"
    | None -> print_endline "closed"
  in
  H.click h ~selector:".model-chip";
  H.key h "ArrowRight" ~target:Field;
  shown ();
  H.key h "ArrowRight" ~target:Page;
  shown ();
  H.key h "ArrowRight" ~target:Page;
  shown ();
  H.key h "ArrowLeft" ~target:Field;
  shown ();
  H.key h "Tab" ~target:Page;
  shown ();
  H.key h "Tab" ~shift:true ~target:Page;
  shown ();
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    (Chip_step 1)
    (Focus popover-list)
    Place_popover
    Thinking
    (Chip_step 1)
    (Focus popover-list)
    Place_popover
    Mode
    (Chip_step 1)
    (Focus popover-filter)
    Place_popover
    Model
    (Chip_step -1)
    (Focus popover-list)
    Place_popover
    Mode
    (Chip_step 1)
    (Focus popover-filter)
    Place_popover
    Model
    (Chip_step -1)
    (Focus popover-list)
    Place_popover
    Mode
    |}];
  (* In the model's filter, once something is typed, ← → move the caret. *)
  H.click h ~selector:".model-chip";
  H.input h ~selector:"#popover-filter" "gp";
  H.key h "ArrowLeft" ~target:Field;
  shown ();
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    (Picker_query gp)
    (browser default)
    Model
    |}];
  (* Only the open chip says so; the other chips stay live, the rest of the
     composer is inert, under the menu. The chips are lifted over the
     menu's backdrop (menu-open) only while it is open: the rest of the
     time, what covers the page covers them too. *)
  H.names h ~selector:".chip-button[aria-expanded=true]";
  H.elements h ~selector:".composer [inert]";
  H.elements h ~selector:".composer-controls";
  H.elements h ~selector:".chip-anchor";
  [%expect
    {|
    button "Claude Opus 5.5" -- Claude Opus 5.5: switch model (Ctrl+L)
    <textarea class="editor">
    <div class="composer-buttons">
    <div class="composer-controls menu-open">
    <div class="chip-anchor model open">
    <div class="chip-anchor thinking">
    <div class="chip-anchor mode">
    |}];
  H.click h ~selector:".mode-chip";
  shown ();
  H.click h ~selector:".mode-chip";
  shown ();
  H.click h ~selector:".thinking-chip";
  H.click h ~selector:".popover-backdrop";
  shown ();
  H.elements h ~selector:".composer-controls";
  [%expect
    {|
    (Open_chip Mode)
    (Focus popover-list)
    Place_popover
    Mode
    (Open_chip Mode)
    (Focus editor)
    closed
    (Open_chip Thinking)
    (Focus popover-list)
    Place_popover
    Close_dialog
    (Focus editor)
    closed
    <div class="composer-controls">
    |}];
  (* A model without thinking levels: ← → pass over its chip. *)
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            [ ( "model"
              , Jsonaf.of_string
                  (H.model_json
                     ~thinking:false
                     ~provider:"deepseek"
                     ~id:"deepseek-chat"
                     ~name:"DeepSeek Chat"
                     ()) )
            ; "thinking", `String "off"
            ]
          ()));
  H.quiet h (fun () -> H.click h ~selector:".model-chip");
  H.quiet h (fun () -> H.key h "ArrowRight" ~target:Field);
  shown ();
  [%expect
    {|
    (Open_chip Model)
    (Chip_step 1)
    Mode
    |}]
;;

(* A phone: the menu is as wide as the composer (style.css); what it shows
   is the same. *)
let%expect_test "narrow: the same menu" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.click h ~selector:".mode-chip";
  H.text h ~selector:"#chip-popover";
  H.names h ~selector:"#chip-popover";
  H.names h ~selector:"#chip-popover [role]";
  [%expect
    {|
    (Open_chip Mode)
    (Focus popover-list)
    Place_popover
    When tools ask first Alt+M cycles
    Asks: edits & commands bash, job_input, edits and MCP tools ask first
    Asks: commands file edits run; bash, job_input and MCP tools ask first
    ✓ Auto: never asks tools run without asking
    Plan mode read-only planning: no file edits; bash, job_input and MCP tools ask first
    Enter chooses, ← → the next setting, Esc closes
    [dialog] div "When tools ask first"
    [listbox] div "When tools ask first"
    [option] div "Asks: edits & commands bash, job_input, edits and MCP tools ask first"
    [option] div "Asks: commands file edits run; bash, job_input and MCP tools ask first"
    [option] div "Auto: never asks tools run without asking"
    [option] div "Plan mode read-only planning: no file edits; bash, job_input and MCP tools ask first"
    |}]
;;

let%expect_test "the status in the card: idle, running, and what applies" =
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
    H.text h ~selector:".session-card .status";
    H.text h ~selector:".session-card .status-extras"
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
  H.elements h ~selector:".session-card .status > .status-item";
  H.elements h ~selector:".session-card .status-extras > .status-item";
  (* Last, in a slot of its own width (style.css), saying it all in its
     title. *)
  H.show h ~selector:".session-card .status > .activity";
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
  H.click h ~selector:".session-card .context";
  [%expect
    {|
    (Run /session)
    (Focus dialog)
    (Rpc (method_ session_stats) (params ()) (tag Session_stats))
    |}];
  (* Waiting for the backend, the card and the bar say so where the status
     goes. *)
  let h = H.connecting () in
  H.text h ~selector:".session-card";
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.text h ~selector:".session-bar";
  [%expect
    {|
    Connecting…
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
            aria-haspopup="dialog"
            aria-expanded="false"
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
