open! Core
module H = Harness
open Agent_events

(* The session's status at the composer's right (activity, context, cost):
   what the agent is doing (a dot alone while idle), the context's ring
   (its details under it) and the cost; what applies at the right of the
   chrome, when anything does; the branch in the details. Where tools run
   is the composer's chip. Each [card] prints the lines as the user reads
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

(* The status as it stands. *)
let card h =
  print_endline "now:";
  H.text h ~selector:".composer-status";
  print_endline "applies:";
  H.text h ~selector:".chrome-tools .status-notes";
  print_endline "context:";
  H.text h ~selector:".composer-status .context";
  print_endline "where:";
  H.text h ~selector:".composer-status .status-details .branch-line";
  print_endline "lit:";
  List.iter
    [ "lamp"; "running"; "mode-chip.never-asks"; "warm"; "hot" ]
    ~f:(fun cls -> H.elements h ~selector:(".composer ." ^ cls))
;;

let%expect_test
    "the status idle, thinking, with a subagent and a quiet transcript"
  =
  let h = H.create ~state:(H.state_json ~fields:(fields ()) ()) () in
  (* Idle, out of git, the backend the only host: two lines, nothing lit. *)
  card h;
  [%expect
    {|
    now:
    Ready (5%) $1.30
    Status Ready
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Thinking provider's default
    Mode Auto: never asks
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <button class="btn chip-button mode-chip never-asks">
    |}];
  (* The cost to the cent, its title has it exactly; the context's title has
     the tokens. *)
  H.names h ~selector:".composer-status .status-gauge > *";
  H.names h ~selector:".composer-status .context";
  [%expect
    {|
    [status] span "Ready"
    button "Context 5% full: the session's details"
    span "$1.30" -- Session cost: $1.3021
    [tooltip] div "Status Ready Context 10k / 200k \194\183 5% Tokens 1.6M in \194\183 21k out \194\183 0 cached Cost $1.3021 Thinking provider's default Mode Auto: never asks Model Claude Opus 5.5 Click for the session\226\128\153s details \194\183 /compact frees context"
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
    Thinking (5%) $1.30
    Status Thinking
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Thinking provider's default
    Mode Auto: never asks
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <span class="lamp">
    <span class="activity running status-item">
    <button class="btn chip-button mode-chip never-asks">
    |}];
  (* The agent runs a subagent in the background (the agents' toggle
     counts it; the status does not), and the transcript is quiet: a note. *)
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
    Running tools (5%) $1.30
    Status Running tools
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Thinking provider's default
    Mode Auto: never asks
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <span class="lamp">
    <span class="activity running status-item">
    <button class="btn chip-button mode-chip never-asks">
    |}];
  (* The agent done: nothing is lit but the note. *)
  state h ();
  card h;
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    now:
    Ready (5%) $1.30
    Status Ready
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Thinking provider's default
    Mode Auto: never asks
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <button class="btn chip-button mode-chip never-asks">
    |}];
  (* Clicking the context opens the session's details. *)
  H.click h ~selector:".composer-status .context";
  [%expect
    {|
    (Run /session)
    (Focus dialog)
    (Rpc (method_ session_stats) (params ()) (tag Session_stats))
    |}]
;;

let%expect_test
    "the branch in the details; the host in the composer, the plan a note"
  =
  let others =
    {|[{"id":"backend","name":"container","cwd":"/work"},{"id":"c7","name":"build-runner-eu-west-a-long-hostname","cwd":"/srv/work"}]|}
  in
  let branch = "feature/a-rather-long-branch-for-the-sidebar-card" in
  let h =
    H.create ~state:(H.state_json ~fields:(fields ~branch ~plan ()) ()) ()
  in
  (* A branch: a line of the details, whatever the hosts; the plan is a
     note at the right of the chrome. *)
  H.elements h ~selector:".chat-chrome > *";
  H.elements h ~selector:".chrome-tools > *";
  H.text h ~selector:".status-details .branch-line";
  H.elements h ~selector:".status-notes .plan";
  state h ~branch ~plan ~hosts:others ~active_host:"c7" ();
  H.text h ~selector:".status-details .branch-line";
  H.names h ~selector:".composer .host-chip";
  [%expect
    {|
    <div class="chrome-left">
    <div class="chrome-center">
    <div class="chrome-right">
    <div class="status-notes">
    <div class="panel-toggles">
    <Vdom.Node.none-widget class="">
    Branch feature/a-rather-long-branch-for-the-sidebar-card
    <button class="link mode plan status-item">
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 2)))))
    Branch feature/a-rather-long-branch-for-the-sidebar-card
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
    Ready (5%) $1.30
    Status Ready
    Context 10k / 200k · 5%
    Tokens 1.6M in · 21k out · 0 cached
    Cost $1.3021
    Thinking provider's default
    Mode Auto: never asks
    Model Claude Opus 5.5
    Click for the session’s details · /compact frees context
    applies:
    context:
    (5%)
    Context 10k / 200k · 5%
    where:
    lit:
    <button class="btn chip-button mode-chip never-asks">
    <button class="btn chip-button host-chip warm">
    <button class="btn chip-button host-chip warm">
    |}];
  (* Out of git: no branch line. *)
  state h ();
  H.elements h ~selector:".status-details .branch-line";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 4)))))
    |}];
  (* Without the sidebar the chrome has its toggle and the settings, the
     plan opens /todos; the host stays the composer's alone. *)
  state h ~branch ~plan ~hosts:others ~active_host:"c7" ();
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.text h ~selector:".chat-chrome";
  H.elements h ~selector:".chat-chrome .host-chip";
  H.click h ~selector:".status-notes .plan";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 5)))))
    (Sessions (Ctrl+B))
    (plan 1/2: Final nix GC)
    (Filter the transcript (/filter)) (Files (Ctrl+Shift+F)) (Terminal (Ctrl+`))
    (Settings (Ctrl+, or /settings))
    (Run /todos)
    (Focus dialog)
    |}]
;;

(* A phone: the ☰ at the left, the session's name in the middle, and one ⋯
   for the rest (the thinking level, the mode, the transcript's verbosity and
   the usage, what applies, the panels, the settings), which opens a menu;
   Esc or the backdrop close it. Wider, the rest is laid out at the right. *)
let%expect_test "a phone's chrome: the ⋯ opens the menu" =
  let h = H.create () in
  H.elements h ~selector:".chrome-more";
  H.elements h ~selector:".chrome-tools";
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.elements h ~selector:".chat-chrome > * > *";
  H.show h ~selector:".chrome-more .more-toggle";
  H.click h ~selector:".chrome-more .more-toggle";
  H.elements h ~selector:"#chrome-menu > *";
  H.names h ~selector:".chrome-more .more-toggle";
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:"#chrome-menu";
  H.click h ~selector:".chrome-more .more-toggle";
  H.click h ~selector:".chrome-backdrop";
  H.elements h ~selector:".chrome-backdrop";
  [%expect
    {|
    <div class="chrome-tools">
    <button class="btn chrome-button ghost icon round toggle-sidebar">
    <h1 class="chrome-title">
    <div class="chrome-more">
    <button type="button"
            title="Panels and settings"
            aria-label="Panels and settings"
            aria-controls="chrome-menu"
            aria-expanded="false"
            class="btn chrome-button ghost icon more-toggle round"
            @on_click>
      <icon class="more"> </icon>
    </button>
    Toggle_status_tray
    <div class="sheet-grip">
    <div class="session-sections">
    <div class="status-notes">
    <button class="jump-row sheet-row">
    <button class="filter-row sheet-row">
    <button class="files-sheet-row files-toggle sheet-row">
    <button class="sheet-row terminal-row terminal-toggle">
    <button class="settings-row sheet-row">
    button "Panels and settings"
    Toggle_status_tray
    Toggle_status_tray
    Toggle_status_tray
    |}]
;;
