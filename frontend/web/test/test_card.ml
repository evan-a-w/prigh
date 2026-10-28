open! Core
module H = Harness
open Agent_events

(* The session's card at the foot of the docked sidebar: two quiet lines,
   what the agent is doing with what applies across from it, then the
   counters; and where (the branch, the tool host when there is a choice)
   when there is something to say. Each [card] prints the lines as the user
   reads them, then what is lit: the lamps, and what is in the accent colour
   (by its class; style.css mutes the background agents and the modes in
   the card while the agent works). *)

let backend_only = {|[{"id":"backend","name":"container","cwd":"/work"}]|}

let fields
      ?(hosts = backend_only)
      ?(active_host = "backend")
      ?branch
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
;;

let state h ?hosts ?active_host ?branch ?running () =
  H.event
    h
    (sprintf
       {|{"type":"event","event":"state","state":%s}|}
       (H.state_json
          ~fields:(fields ?hosts ?active_host ?branch ?running ())
          ()))
;;

let thinking h =
  H.event h {|{"type":"event","event":"agent_start"}|};
  H.event
    h
    {|{"type":"event","event":"message_update","partial":{"role":"assistant","content":[{"type":"thinking","text":""}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"thinking_delta","text":""}}|}
;;

(* The card's lines as they stand (the grid puts the activity first). *)
let card h =
  print_endline "now:";
  H.text h ~selector:".session-card .activity";
  H.text h ~selector:".session-card .status-extras";
  print_endline "counters:";
  H.text h ~selector:".session-card .status > :not(.activity)";
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
    Ready
    counters:
    (5%)
    ↑1.6M ↓21k
    $1.30
    where:
    lit:
    |}];
  (* The cost to the cent; its title has it exactly, the tokens theirs. *)
  H.names h ~selector:".session-card .status > *";
  [%expect
    {|
    button "Context 5% full" -- Context: 10k of 200k tokens (/compact frees some). Click for the session's details (/session)
    span "\226\134\1451.6M \226\134\14721k" -- Tokens: 1600000 in, 21000 out, 0 cache read
    span "$1.30" -- Session cost: $1.3021 (/session)
    [status] span "Ready"
    |}];
  (* Thinking: the activity is the one thing lit. *)
  state h ~running:true ();
  thinking h;
  card h;
  [%expect
    {|
    now:
    Thinking
    counters:
    (5%)
    ↑1.6M ↓21k
    $1.30
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
    Running tools
    (1 agent running) (quiet)
    counters:
    (5%)
    ↑1.6M ↓21k
    $1.30
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
    Ready
    (1 agent running) (quiet)
    counters:
    (5%)
    ↑1.6M ↓21k
    $1.30
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

let%expect_test "where: the branch, and the host when there is a choice" =
  let others =
    {|[{"id":"backend","name":"container","cwd":"/work"},{"id":"c7","name":"build-runner-eu-west-a-long-hostname","cwd":"/srv/work"}]|}
  in
  let branch = "feature/a-rather-long-branch-for-the-sidebar-card" in
  let h = H.create ~state:(H.state_json ~fields:(fields ~branch ()) ()) () in
  (* A branch, the backend alone: the branch only. *)
  H.text h ~selector:".session-card .card-where";
  [%expect {| (feature/a-rather-long-branch-for-the-sidebar-card) |}];
  (* Another host to choose: it says which, the backend. *)
  state h ~branch ~hosts:others ();
  H.text h ~selector:".session-card .card-where";
  [%expect
    {| (feature/a-rather-long-branch-for-the-sidebar-card) (container) |}];
  (* On the other host, with a long name. *)
  state h ~branch ~hosts:others ~active_host:"c7" ();
  H.names h ~selector:".session-card .card-where > *";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 2)))))
    button "feature/a-rather-long-branch-for-the-sidebar-card" -- On branch feature/a-rather-long-branch-for-the-sidebar-card, in /work: pull requests (/pr)
    button "build-runner-eu-west-a-long-hostname" -- Tools run on build-runner-eu-west-a-long-hostname, in /srv/work (/host switches)
    |}];
  H.click h ~selector:".card-where .host";
  [%expect
    {|
    (Run /host)
    (Focus picker-input)
    |}];
  H.quiet h (fun () -> H.key h "Escape" ~target:Field);
  (* No host at all: the card says so, lit. *)
  state h ~hosts:"[]" ~active_host:"" ();
  card h;
  [%expect
    {|
    Close_dialog
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \n\
                            \n/work") (purpose (Sidebar 3)))))
    now:
    Ready
    counters:
    (5%)
    ↑1.6M ↓21k
    $1.30
    where:
    (No host)
    lit:
    <button class="host link status-item warm">
    |}];
  (* Out of git on the backend alone: no third line. *)
  state h ();
  H.elements h ~selector:".session-card .card-status > *";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 4)))))
    <div class="status">
    <div class="status-extras">
    |}];
  (* Without the sidebar the bar keeps it all on one line, the host when it
     is not the backend. *)
  state h ~branch ~hosts:others ~active_host:"c7" ();
  H.quiet h (fun () -> H.act h Toggle_sidebar);
  H.text h ~selector:".session-bar";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nc7\
                            \n/work") (purpose (Sidebar 5)))))
    (Sessions (Ctrl+B)) (New session)
    (build-runner-eu-west-a-long-hostname)
    (5%) ↑1.6M ↓21k $1.30 Ready
    (Filter the transcript (/filter)) (Files (Ctrl+Shift+F)) (Terminal (Ctrl+`))
    (Settings (Ctrl+, or /settings))
    |}]
;;
