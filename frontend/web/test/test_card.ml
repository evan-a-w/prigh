open! Core
module H = Harness
open Agent_events

(* The session's status in the chat's header, docked sidebar or not: what
   the agent is doing (a dot alone while idle), the context's short meter
   (its details over it) and the cost; what applies before it, when
   anything does; the branch beside the title when there is one. Where tools
   run is the composer's chip. Each [card] prints the lines as the user reads
   them, then what is lit: the lamps, and what is in the accent colour (by
   its class). *)

let plan =
  {|{"steps":[{"step":"Look","status":"completed"},{"step":"Final nix GC","status":"in_progress"}]}|}
;;

let backend_only = {|[{"id":"backend","name":"container","cwd":"/work"}]|}

let fields
      ?(hosts = backend_only)
      ?(active_host = "backend")
      ?branch
      ?plan
      ?(running = false)
      ()
  =
  [ "cwd", `String "/work"
  ; ("running", if running then `True else `False)
  ; "context_tokens", `Number "10000"
  ; "cost_usd", `Number "1.3021"
  ; ( "usage"
    , Jsonaf.of_string {|{"input":1600000,"output":21000,"cache_read":0}|} )
  ; "hosts", Jsonaf.of_string hosts
  ; "active_host", `String active_host
  ]
  @ [ ( "git_branch"
      , Option.value_map branch ~default:`Null ~f:(fun b -> `String b) )
    ]
  @ Option.value_map plan ~default:[] ~f:(fun p ->
    [ "plan", Jsonaf.of_string p ])
;;

let state h ?hosts ?active_host ?branch ?plan ?running () =
  H.event
    h
    (sprintf
       {|{"type":"event","event":"state","state":%s}|}
       (H.state_json
          ~fields:(fields ?hosts ?active_host ?branch ?plan ?running ())
          ()))
;;

let thinking h =
  H.event h {|{"type":"event","event":"agent_start"}|};
  H.event
    h
    {|{"type":"event","event":"message_update","partial":{"role":"assistant","content":[{"type":"thinking","text":""}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"thinking_delta","text":""}}|}
;;

(* The header's status as it stands. *)
let card h =
  print_endline "now:";
  H.text h ~selector:".session-bar .status";
  print_endline "applies:";
  H.text h ~selector:".session-bar .bar-tray > .status-extras";
  print_endline "context:";
  H.text h ~selector:".session-bar .status .context";
  print_endline "where:";
  H.text h ~selector:".session-bar .session-title > .branch";
  print_endline "lit:";
  List.iter
    [ "lamp"
    ; "running"
    ; "queued"
    ; "status-item.mode"
    ; "background.active"
    ; "warm"
    ; "hot"
    ]
    ~f:(fun cls -> H.elements h ~selector:(".session-bar ." ^ cls))
;;

let%expect_test "the card idle, thinking, with a subagent and a mode" =
  let h = H.create ~state:(H.state_json ~fields:(fields ()) ()) () in
  (* Idle, out of git, the backend the only host: two lines, nothing lit. *)
  card h;
  [%expect
    {|
    now:
    Ready (5%)
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    $1.30
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    |}];
  (* The cost to the cent, its title has it exactly; the context's title has
     the tokens. *)
  H.names h ~selector:".session-bar .status > *";
  H.names h ~selector:".session-bar .status .context";
  [%expect
    {|
    [status] span "Ready"
    span "5% Context 10k / 200k \194\183 5% Tokens 1.6M in \194\183 21k out \194\183 0 cached Cost $1.3021 Model Claude Opus 5.5 Click for the session\226\128\153s details \194\183 /compact frees context"
    span "$1.30" -- Session cost: $1.3021 (/session)
    button "Context 5% full: the session's details"
    div "Context 10k / 200k \194\183 5%"
    |}];
  (* Thinking: the activity is the one thing lit. *)
  state h ~running:true ();
  thinking h;
  card h;
  [%expect
    {|
    now:
    Thinking (5%)
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    $1.30
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <span class="lamp">
    <span class="activity running status-item">
    |}];
  (* The agent runs a subagent in the background, and the transcript is
     quiet: across from the activity, both unlit (style.css hides the
     agents' lamp while the activity's glows). *)
  List.iter
    ~f:(H.event h)
    [ call ~id:"s1" ~name:"subagent" {|{"task":"Survey"}|}
    ; tool_start ~id:"s1" ~name:"subagent" {|{"task":"Survey"}|}
    ; start ~call:"s1" ~agent:"a1" "Survey"
    ; inner ~call:"s1" ~agent:"a1" {|{"event":"agent_start"}|}
    ; tool_end ~id:"s1" ~name:"subagent" "started agent a1 (Survey)"
    ];
  H.quiet h (fun () -> H.act h (Run "/verbosity quiet"));
  card h;
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    now:
    Running tools (5%)
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    $1.30
    applies:
    (1 agent running) (quiet)
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <span class="lamp">
    <span class="lamp">
    <span class="activity running status-item">
    <button class="link mode status-item">
    <button class="active background link status-item">
    |}];
  (* The agent done, the subagent still at work: now its lamp is lit. *)
  state h ();
  card h;
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    now:
    Ready (5%)
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    $1.30
    applies:
    (1 agent running) (quiet)
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <span class="lamp">
    <button class="link mode status-item">
    <button class="active background link status-item">
    |}];
  (* Clicking the agents opens them, the context the session's details. *)
  H.click h ~selector:".session-bar .background";
  H.click h ~selector:".session-bar .context";
  [%expect
    {|
    (Open_subagents ())
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Run /session)
    (Focus dialog)
    (Rpc (method_ session_stats) (params ()) (tag Session_stats))
    |}]
;;

let%expect_test "the branch; the host in the composer, the plan in the bar" =
  let others =
    {|[{"id":"backend","name":"container","cwd":"/work"},{"id":"c7","name":"build-runner-eu-west-a-long-hostname","cwd":"/srv/work"}]|}
  in
  let branch = "feature/a-rather-long-branch-for-the-sidebar-card" in
  let h =
    H.create ~state:(H.state_json ~fields:(fields ~branch ~plan ()) ()) ()
  in
  (* A branch: a third line, the branch only, whatever the hosts; the plan
     is not on the card. *)
  H.elements h ~selector:".session-bar > *";
  H.elements h ~selector:".bar-tray > *";
  H.names h ~selector:".session-bar .session-title > .branch";
  H.elements h ~selector:".session-bar .plan";
  state h ~branch ~plan ~hosts:others ~active_host:"c7" ();
  H.names h ~selector:".session-bar .session-title > .branch";
  H.names h ~selector:".composer .host-chip";
  [%expect
    {|
    <Vdom.Node.none-widget class="">
    <div class="session-title">
    <Vdom.Node.none-widget class="">
    <div class="bar-tray">
    <Vdom.Node.none-widget class="">
    <Vdom.Node.none-widget class="">
    <div class="status-extras">
    <div class="session-chips">
    <div class="status">
    <div class="panel-toggles">
    button "feature/a-rather-long-branch-for-the-sidebar-card" -- On branch feature/a-rather-long-branch-for-the-sidebar-card, in /work: pull requests (/pr)
    <button class="link mode plan status-item">
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 2)))))
    button "feature/a-rather-long-branch-for-the-sidebar-card" -- On branch feature/a-rather-long-branch-for-the-sidebar-card, in /work: pull requests (/pr)
    button "build-runner-eu-west-a-long-hostname" -- Tools run on build-runner-eu-west-a-long-hostname, in /srv/work (/host switches)
    |}];
  (* No host at all: the chip says so, lit, and the card has nothing lit. *)
  state h ~hosts:"[]" ~active_host:"" ();
  card h;
  H.elements h ~selector:".composer .warm";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \n\
                            \n/work") (purpose (Sidebar 3)))))
    now:
    Ready (5%)
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    $1.30
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <button class="btn chip-button host-chip warm">
    |}];
  (* Out of git: no third line. *)
  state h ();
  H.elements h ~selector:".session-bar > *";
  H.elements h ~selector:".bar-tray > *";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 4)))))
    <Vdom.Node.none-widget class="">
    <div class="session-title">
    <Vdom.Node.none-widget class="">
    <div class="bar-tray">
    <Vdom.Node.none-widget class="">
    <Vdom.Node.none-widget class="">
    <div class="status-extras">
    <div class="session-chips">
    <div class="status">
    <div class="panel-toggles">
    |}];
  (* Without the sidebar the bar keeps the rest on one line, with the plan
     (opening /todos); the host stays the composer's alone. *)
  state h ~branch ~plan ~hosts:others ~active_host:"c7" ();
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.text h ~selector:".session-bar";
  H.elements h ~selector:".session-bar .host";
  H.elements h ~selector:".session-bar .host-chip";
  H.click h ~selector:".session-bar .plan";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 5)))))
    (Sessions (Ctrl+B))
    (New session) (feature/a-rather-long-branch-for-the-sidebar-card)
    (plan 1/2: Final nix GC)
    (on)
    (auto)
    Ready (5%)
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Model Claude Opus 5.5
    Branch feature/a-rather-long-branch-for-the-sidebar-card
    Click for the session’s details · /compact frees context
    $1.30
    (Filter the transcript (/filter)) (Files (Ctrl+Shift+F)) (Terminal (Ctrl+`))
    (Settings (Ctrl+, or /settings))
    (Run /todos)
    (Focus dialog)
    |}]
;;

(* A phone: the header keeps its title, the rest folds into a tray that a
   glance at the status (a dot, the percent) opens; Esc or the scrim close
   it. Wider, there is no glance and the tray is laid out in the header. *)
let%expect_test "a phone's header: the glance opens the tray" =
  let h = H.create () in
  H.elements h ~selector:".bar-tray-toggle";
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.show h ~selector:".bar-tray-toggle";
  H.click h ~selector:".bar-tray-toggle";
  H.elements h ~selector:".bar-tray.open";
  H.names h ~selector:".bar-tray-toggle";
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:".bar-tray.open";
  H.click h ~selector:".bar-tray-toggle";
  H.click h ~selector:".bar-tray-backdrop";
  H.elements h ~selector:".bar-tray-backdrop";
  [%expect {|
    <button type="button"
            title="Status, thinking, mode and panels"
            aria-label="Status and controls"
            aria-controls="bar-tray"
            aria-expanded="false"
            class="bar-tray-toggle btn ghost"
            @on_click>
      <span aria-hidden="true" class="status-glance">
        <span class="idle-dot"> </span>
        <span class="glance-percent"> 0% </span>
      </span>
      <icon class="chevron"> </icon>
    </button>
    Toggle_status_tray
    <div class="bar-tray open">
    button "Status and controls" -- Status, thinking, mode and panels
    Toggle_status_tray
    Toggle_status_tray
    Toggle_status_tray
    |}]
;;
