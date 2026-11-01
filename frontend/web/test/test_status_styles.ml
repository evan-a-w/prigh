open! Core
open Prigh_web
module H = Harness
module Key = Pref_sync.Key

(* Where the session's status sits ([web.status-style], /status-style,
   Settings → Appearance → Layout): six styles over one data model and one
   detail card. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let style h = print_s [%sexp ((H.model h).status_style : Status_style.t)]

let at h name =
  H.quiet h (fun () ->
    H.event
      h
      (sprintf
         {|{"event":"prefs_changed","prefs":{"%s":"%s"}}|}
         Key.status_style
         name))
;;

(* A session part-way through its context, with a budget. *)
let fields ?(context = "130000") ?(cost = "0.42") ?budget () =
  [ "context_tokens", `Number context
  ; "cost_usd", `Number cost
  ; "usage", Jsonaf.of_string {|{"input":120000,"output":9000,"cache_read":0}|}
  ]
  @ Option.value_map budget ~default:[] ~f:(fun b ->
    [ "budget_usd", `Number b ])
;;

let state h ?context ?cost ?budget ?(running = false) ?(mode = "auto") () =
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json
          ~fields:
            (("running", if running then `True else `False)
             :: ("approval_mode", `String mode)
             :: fields ?context ?cost ?budget ())
          ()))
;;

let create () = H.create ~state:(H.state_json ~fields:(fields ()) ()) ()

(* Every status element there is, whatever the style: only its own show. *)
let where h =
  List.iter
    [ ".chrome-center .status-island"
    ; ".composer .send-ring"
    ; ".composer .row-chips"
    ; ".composer .status-whisper"
    ; ".composer .border-meter"
    ; ".composer .border-tag"
    ; ".composer .hub-badge"
    ; ".sidebar .status-card"
    ; ".status-details"
    ]
    ~f:(fun selector -> H.elements h ~selector)
;;

let%expect_test
    "the pref: from the backend, live; an unknown name is the default"
  =
  let h = H.connecting () in
  H.quiet h (fun () -> H.act h Start);
  style h;
  H.reply
    h
    "get_prefs"
    {|{"prefs":{"web.migrated":true,"web.status-style":"ring"}}|};
  style h;
  [%expect
    {|
    Island
    (Apply_theme ((name Original) (accent (Preset Orange))))
    (Cache_prefs
     (((key web.theme) (value ())) ((key web.accent) (value ()))
      ((key web.notify) (value ())) ((key web.verbosity) (value ()))
      ((key web.terminal-open) (value ())) ((key web.sidebar-folded) (value ()))
      ((key web.tabs) (value ())) ((key web.status-style) (value (ring)))
      ((key web.open-tabs) (value ())) ((key web.review) (value ()))
      ((key history) (value ())) ((key web.migrated) (value (true)))
      ((key web.sidebar-width) (value ())) ((key web.files-width) (value ()))
      ((key web.files-preview-width) (value ()))
      ((key web.agents-width) (value ())) ((key web.terminal-height) (value ()))
      ((key web.review-chat-width) (value ()))
      ((key web.review-files-width) (value ()))
      ((key web.review-margin-width) (value ()))))
    Ring
    |}];
  H.event h {|{"event":"prefs_changed","prefs":{"web.status-style":"whisper"}}|};
  style h;
  H.event h {|{"event":"prefs_changed","prefs":{"web.status-style":"marquee"}}|};
  style h;
  [%expect
    {|
    (Cache_prefs (((key web.status-style) (value (whisper)))))
    Whisper
    (Cache_prefs (((key web.status-style) (value (marquee)))))
    Island
    |}]
;;

let%expect_test "/status-style: saves the pref, a picker alone, errors" =
  let h = create () in
  run h "/status-style ring";
  style h;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/status-style ring") (max 500)))
     (tag Ignore))
    (Cache_prefs (((key web.status-style) (value (ring)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.status-style ring)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 5300))
    Ring
    Status: Send ring, a ring of context round send; thinking and mode by the model (/status-style)
    |}];
  H.quiet h (fun () -> run h "/status-style");
  H.text h ~selector:".picker-item";
  H.quiet h (fun () -> H.key h "Escape" ~target:Field);
  [%expect
    {|
    Island a pill floating over the top of the chat
    Send ring a ring of context round send; thinking and mode by the model ✓
    Status line one quiet line under the message box
    Border meter the message box's edge fills with context
    Model chip the model chip shows the context; its menu has the rest
    Sidebar a card at the sidebar's foot (a line under the box without it)
    Close_dialog
    |}];
  run h "/status-style marquee";
  style h;
  H.text h ~selector:".toast.error";
  H.text h ~selector:".composer textarea";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/status-style marquee") (max 500)))
     (tag Ignore))
    Ring
    Unknown status style “marquee”: use island, ring, whisper, border, hub, sidebar. (Dismiss)
    [/status-style marquee]
    |}]
;;

let%expect_test "the settings' row: one segment a style" =
  let h = create () in
  H.quiet h (fun () -> H.act h Open_settings);
  H.text h ~selector:".setting.status-style";
  H.elements h ~selector:".setting.status-style .segment";
  H.click h ~selector:".setting.status-style .segment:nth-child(4)";
  H.elements h ~selector:".setting.status-style .segment.on";
  style h;
  [%expect
    {|
    Status
    Where the activity, context, cost, thinking and mode show: a pill floating over the top of the chat.
    (Island) (Send ring) (Status line) (Border meter) (Model chip) (Sidebar)
    <button class="on segment">
    <button class="segment">
    <button class="segment">
    <button class="segment">
    <button class="segment">
    <button class="segment">
    (Run "/status-style border")
    (Cache_prefs (((key web.status-style) (value (border)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.status-style border)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 4660))
    <button class="on segment">
    Border
    |}]
;;

let%expect_test "island: a pill in the chrome's middle" =
  let h = create () in
  where h;
  H.names h ~selector:".status-island";
  H.names h ~selector:".status-island .status-gauge > *";
  H.names h ~selector:".status-island .session-chips button";
  [%expect
    {|
    <div class="status-island">
    <div class="status-details">
    [group] div "Session status"
    [status] span "Ready"
    button "Context 65% full: the session's details"
    span "$0.42" -- Session cost: $0.4200
    [tooltip] div "Status Ready Context 130k / 200k \194\183 65% Tokens 120k in \194\183 9.0k out \194\183 0 cached Cost $0.4200 Thinking on Mode Auto: never asks Model Claude Opus 5.5 Branch main Click for the session\226\128\153s details \194\183 /compact frees context"
    button "Thinking on" -- Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)
    button "Auto: never asks" -- Tools run without asking (Alt+M cycles, /mode picks)
    |}];
  (* At work, it glows. *)
  H.quiet h (fun () -> state h ~running:true ());
  H.elements h ~selector:".status-island";
  H.text h ~selector:".status-island .activity";
  [%expect
    {|
    <div class="busy status-island">
    Working
    |}]
;;

let%expect_test
    "ring: round send; thinking and mode by the model, mode quiet in ask"
  =
  let h = create () in
  at h "ring";
  where h;
  H.show h ~selector:".composer .send-gauge";
  H.names h ~selector:".composer .row-chips button";
  [%expect
    {|
    <span class="ring send-ring warm">
    <div class="row-chips session-chips">
    <div class="status-details">
    <span class="above send-gauge status-gauge">
      <span class="ring send-ring warm" custom-css-vars=((--pct 65.0))>
        <button type="button"
                title="Send (Enter)"
                aria-label="Send"
                disabled=""
                class="btn primary round send"
                @on_click>
          <icon class="send"> </icon>
        </button>
      </span>
      <button type="button"
              aria-label="Context 65% full: the session's details"
              aria-describedby="status-details"
              aria-haspopup="dialog"
              class="context link sr-only status-item"
              @on_click> </button>
      <div class="sr-only">
        <span title="Ready" role="status" aria-atomic="true" class="activity idle status-item">
          <span class="idle-dot"> </span>
          <span class="label"> Ready </span>
        </span>
      </div>
      <div role="tooltip" id="status-details" class="status-details">
        <div class="status-detail">
          <span class="status-detail-key"> Status </span>
          <span class="status-detail-value"> Ready </span>
        </div>
        <div class="context status-detail warm">
          <span class="status-detail-key"> Context </span>
          <span class="status-detail-value"> 130k / 200k · 65% </span>
        </div>
        <div class="status-detail">
          <span class="status-detail-key"> Tokens </span>
          <span class="status-detail-value"> 120k in · 9.0k out · 0 cached </span>
        </div>
        <div class="status-detail">
          <span class="status-detail-key"> Cost </span>
          <span class="status-detail-value"> $0.4200 </span>
        </div>
        <div class="status-detail">
          <span class="status-detail-key"> Thinking </span>
          <span class="status-detail-value"> on </span>
        </div>
        <div class="status-detail">
          <span class="status-detail-key"> Mode </span>
          <span class="status-detail-value"> Auto: never asks </span>
        </div>
        <div class="status-detail">
          <span class="status-detail-key"> Model </span>
          <span class="status-detail-value"> Claude Opus 5.5 </span>
        </div>
        <div class="branch-line status-detail">
          <span class="status-detail-key"> Branch </span>
          <span class="status-detail-value"> main </span>
        </div>
        <div class="status-detail-hint"> Click for the session’s details · /compact frees context </div>
      </div>
    </span>
    button "Thinking on" -- Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)
    button "Auto: never asks" -- Tools run without asking (Alt+M cycles, /mode picks)
    |}];
  (* In ask, the mode chip goes (Alt+M and /mode still change it). *)
  H.quiet h (fun () -> state h ~mode:"ask" ());
  H.names h ~selector:".composer .row-chips button";
  [%expect
    {| button "Thinking on" -- Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking) |}];
  H.quiet h (fun () -> state h ~running:true ());
  H.elements h ~selector:".composer";
  [%expect {| <footer class="busy composer status-style-ring"> |}]
;;

let%expect_test "whisper: a line under the box, each part a button" =
  let h = create () in
  at h "whisper";
  where h;
  H.text h ~selector:".status-whisper";
  H.names h ~selector:".status-whisper button";
  [%expect
    {|
    <div class="status-whisper">
    <div class="status-details">
    Ready · (65% of 200k)
    Status Ready
    Context 130k / 200k · 65%
    Tokens 120k in · 9.0k out · 0 cached
    Cost $0.4200
    Thinking on
    Mode Auto: never asks
    Model Claude Opus 5.5
    Branch main
    Click for the session’s details · /compact frees context
    · $0.42 ·
    (on)
    (auto)
    main
    button "Context 65% full: the session's details"
    button "Thinking on" -- Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)
    button "Auto: never asks" -- Tools run without asking (Alt+M cycles, /mode picks)
    |}]
;;

let%expect_test
    "border: the box's edge is the meter; thinking and mode after the host"
  =
  let h = create () in
  at h "border";
  where h;
  H.show h ~selector:".composer .border-meter";
  H.names h ~selector:".composer .border-tag > *";
  H.names h ~selector:".composer-row > * > *";
  [%expect
    {|
    <div class="row-chips session-chips">
    <span class="border-meter warm">
    <span class="above border-tag status-gauge">
    <div class="status-details">
    <span aria-hidden="true" class="border-meter warm">
      <span class="border-fill" style={ width: 65.00%; }> </span>
    </span>
    [status] span "Ready"
    button "Context 65% full: the session's details"
    span "$0.42" -- Session cost: $0.4200
    [tooltip] div "Status Ready Context 130k / 200k \194\183 65% Tokens 120k in \194\183 9.0k out \194\183 0 cached Cost $0.4200 Thinking on Mode Auto: never asks Model Claude Opus 5.5 Branch main Click for the session\226\128\153s details \194\183 /compact frees context"
    div "(no name)"
    div "Claude Opus 5.5"
    div "backend"
    div "on"
    div "auto"
    button "Dictate" -- Dictate (speak, then click again to stop)
    Vdom.Node.none-widget "(no name)"
    button "Send" -- Send (Enter)
    |}]
;;

let%expect_test "hub: the model chip's ring; its menu has thinking, mode, usage"
  =
  let h = create () in
  at h "hub";
  where h;
  H.show h ~selector:".hub-badge";
  [%expect
    {|
    <span class="hub-badge">
    <span aria-hidden="true" class="hub-badge">
      <span class="mini ring warm" custom-css-vars=((--pct 65.0))> </span>
      <span class="percent"> 65% </span>
      <span title="Auto: never asks" class="hub-mode loud">
        <icon class="shield"> </icon>
      </span>
    </span>
    |}];
  H.click h ~selector:".model-chip";
  H.text h ~selector:"#chip-popover .hub-sections";
  H.names h ~selector:"#chip-popover .hub-choice";
  [%expect
    {|
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    Thinking Alt+T
    (off) (low) (on) (high) (max)
    Mode Alt+M
    (ask) (auto-edit) (auto) (plan)
    Usage
    65% 130k / 200k $0.42 120k in · 9.0k out
    button "off" -- Thinking: off (/thinking off)
    button "low" -- Thinking: low (/thinking low)
    button "on" -- Thinking: on (/thinking on)
    button "high" -- Thinking: high (/thinking high)
    button "max" -- Thinking: max (/thinking max)
    button "ask" -- bash, job_input, edits and MCP tools ask first
    button "auto-edit" -- file edits run; bash, job_input and MCP tools ask first
    button "auto" -- tools run without asking
    button "plan" -- read-only planning, until you leave it (/plan off or Alt+M): no file edits; bash, job_input and MCP tools ask first
    |}];
  (* Choosing a level or a mode keeps the menu open. *)
  H.click h ~selector:"#chip-popover .hub-choice:nth-child(4)";
  H.elements h ~selector:"#chip-popover";
  H.names h ~selector:"#chip-popover .hub-choice.on";
  H.click
    h
    ~selector:"#chip-popover .hub-section:nth-child(2) .hub-choice:nth-child(2)";
  H.elements h ~selector:"#chip-popover";
  [%expect
    {|
    (Set_thinking high)
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    <div class="chip-popover menu-surface">
    button "high" -- Thinking: high (/thinking high)
    button "auto" -- tools run without asking
    (Set_mode ((approval Auto_edit) (plan false)))
    (Rpc (method_ set_mode)
     (params ((approval_mode auto-edit) (plan_mode false)))
     (tag
      (Mode_set
       "Mode: auto-edit (file edits run; bash, job_input and MCP tools ask first)")))
    <div class="chip-popover menu-surface">
    |}];
  (* Alt+T still cycles the thinking, with no chip of its own. *)
  H.quiet h (fun () -> H.key h "Escape" ~target:Field);
  H.key h "t" ~alt:true;
  [%expect
    {|
    Close_dialog
    Cycle_thinking
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    |}]
;;

let%expect_test "sidebar: a card at its foot, a whisper without it" =
  let h = create () in
  at h "sidebar";
  where h;
  H.text h ~selector:".status-card";
  [%expect
    {|
    <section class="status-card">
    <div class="status-details">
    Ready $0.42
    (65%)
    Status Ready
    Context 130k / 200k · 65%
    Tokens 120k in · 9.0k out · 0 cached
    Cost $0.4200
    Thinking on
    Mode Auto: never asks
    Model Claude Opus 5.5
    Branch main
    Click for the session’s details · /compact frees context
    (on)
    (auto)
    |}];
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  where h;
  [%expect
    {|
    <div class="status-whisper">
    <div class="status-details">
    |}];
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.quiet h (fun () -> H.act h (Set_narrow true));
  where h;
  [%expect
    {|
    <div class="status-whisper">
    <div class="status-details">
    |}]
;;

let%expect_test "the context and the budget warm, then hot, in every style" =
  let h = create () in
  let lit () =
    List.iter
      [ ".ring.warm"
      ; ".ring.hot"
      ; ".meter.warm"
      ; ".meter.hot"
      ; ".border-meter.warm"
      ; ".border-meter.hot"
      ; ".budget.warm"
      ; ".budget.hot"
      ; ".status-detail.warm"
      ; ".status-detail.hot"
      ]
      ~f:(fun selector -> H.elements h ~selector)
  in
  List.iter Status_style.all ~f:(fun s ->
    at h (Status_style.name s);
    print_endline ("-- " ^ Status_style.name s);
    H.quiet h (fun () -> state h ~context:"130000" ~cost:"4.2" ~budget:"5" ());
    lit ();
    H.quiet h (fun () -> state h ~context:"180000" ~cost:"5.5" ~budget:"5" ());
    lit ());
  [%expect
    {|
    -- island
    <span class="mini ring warm">
    <button class="budget cost link status-item warm">
    <div class="context status-detail warm">
    <div class="cost status-detail warm">
    <span class="hot mini ring">
    <button class="budget cost hot link status-item">
    <div class="context hot status-detail">
    <div class="cost hot status-detail">
    -- ring
    <span class="ring send-ring warm">
    <div class="context status-detail warm">
    <div class="cost status-detail warm">
    <span class="hot ring send-ring">
    <div class="context hot status-detail">
    <div class="cost hot status-detail">
    -- whisper
    <span class="meter warm">
    <button class="budget cost link status-item warm">
    <div class="context status-detail warm">
    <div class="cost status-detail warm">
    <span class="hot meter">
    <button class="budget cost hot link status-item">
    <div class="context hot status-detail">
    <div class="cost hot status-detail">
    -- border
    <span class="border-meter warm">
    <button class="budget cost link status-item warm">
    <div class="context status-detail warm">
    <div class="cost status-detail warm">
    <span class="border-meter hot">
    <button class="budget cost hot link status-item">
    <div class="context hot status-detail">
    <div class="cost hot status-detail">
    -- hub
    <span class="mini ring warm">
    <span class="hot mini ring">
    -- sidebar
    <span class="meter warm">
    <button class="budget cost link status-item warm">
    <div class="context status-detail warm">
    <div class="cost status-detail warm">
    <span class="hot meter">
    <button class="budget cost hot link status-item">
    <div class="context hot status-detail">
    <div class="cost hot status-detail">
    |}]
;;
