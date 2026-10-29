open! Core
module H = Harness
open Agent_events

(* The session's card at the foot of the docked sidebar: what the agent is
   doing with the cost across from it, what applies when anything does, the
   context's meter (the tokens in its title), and the branch when there is
   one. Where tools run is the composer's chip, and the plan the bar's. Each
   [card] prints the lines as the user reads them, then what is lit: the
   lamps, and what is in the accent colour (by its class; style.css mutes
   the background agents and the modes in the card while the agent
   works). *)

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

(* The card's lines as they stand. *)
let card h =
  print_endline "now:";
  H.text h ~selector:".session-card .card-head";
  print_endline "applies:";
  H.text h ~selector:".session-card .card-status > .status-extras";
  print_endline "context:";
  H.text h ~selector:".session-card .card-status > .context";
  print_endline "where:";
  H.text h ~selector:".session-card .card-where";
  print_endline "lit:";
  List.iter
    [ "lamp"; "running"; "queued"; "mode"; "background.active"; "warm"; "hot" ]
    ~f:(fun cls -> H.elements h ~selector:(".session-card ." ^ cls))
;;

let%expect_test "the card idle, thinking, with a subagent and a mode" =
  let h = H.create ~state:(H.state_json ~fields:(fields ()) ()) () in
  (* Idle, out of git, the backend the only host: two lines, nothing lit. *)
  card h;
  [%expect
    {|
    now:
    Ready $1.30
    applies:
    context:
    (5%)
    where:
    lit:
    |}];
  (* The cost to the cent, its title has it exactly; the context's title has
     the tokens. *)
  H.names h ~selector:".session-card .card-head > *";
  H.names h ~selector:".session-card .card-status > .context";
  [%expect
    {|
    [status] span "Ready"
    span "$1.30" -- Session cost: $1.3021 (/session)
    button "Context 5% full" -- 1.6M tokens in, 21k out, 0 cache read; 5% of a 200k context (/compact frees some). Click for the session's details (/session)
    |}];
  (* Thinking: the activity is the one thing lit. *)
  state h ~running:true ();
  thinking h;
  card h;
  [%expect
    {|
    now:
    Thinking $1.30
    applies:
    context:
    (5%)
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
    Running tools $1.30
    applies:
    (1 agent running) (quiet)
    context:
    (5%)
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
    Ready $1.30
    applies:
    (1 agent running) (quiet)
    context:
    (5%)
    where:
    lit:
    <span class="lamp">
    <button class="link mode status-item">
    <button class="active background link status-item">
    |}];
  (* Clicking the agents opens them, the context the session's details. *)
  H.click h ~selector:".session-card .background";
  H.click h ~selector:".session-card .context";
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
  H.elements h ~selector:".session-card .card-status > *";
  H.names h ~selector:".session-card .card-where > *";
  H.elements h ~selector:".session-card .plan";
  state h ~branch ~plan ~hosts:others ~active_host:"c7" ();
  H.names h ~selector:".session-card .card-where > *";
  H.names h ~selector:".composer .host-chip";
  [%expect
    {|
    <div class="card-head">
    <div class="status-extras">
    <button class="context link status-item">
    <div class="card-where">
    button "feature/a-rather-long-branch-for-the-sidebar-card" -- On branch feature/a-rather-long-branch-for-the-sidebar-card, in /work: pull requests (/pr)
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
    Ready $1.30
    applies:
    context:
    (5%)
    where:
    lit:
    <button class="btn chip-button host-chip warm">
    |}];
  (* Out of git: no third line. *)
  state h ();
  H.elements h ~selector:".session-card .card-status > *";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 4)))))
    <div class="card-head">
    <div class="status-extras">
    <button class="context link status-item">
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
    (Sessions (Ctrl+B)) (New session)
    (plan 1/2: Final nix GC)
    (5%) ↑1.6M ↓21k $1.30 Ready
    (Filter the transcript (/filter)) (Files (Ctrl+Shift+F)) (Terminal (Ctrl+`))
    (Settings (Ctrl+, or /settings))
    (Run /todos)
    (Focus dialog)
    |}]
;;
