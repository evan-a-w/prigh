open! Core
open Agent_events
module H = Harness

(* A subagent's chat: opened from the session card, its line in the
   transcript, Alt+N or [/agents N]; its lineage in the breadcrumb and the
   card; Back to its parent; the composer writing to it ([steer_agent]),
   while it runs and after it finished; stopping it. *)

let in_a1 = inner ~call:"s1" ~agent:"a1"
let in_n1 json = in_a1 (inner ~call:"n1" ~agent:"a1/n1" json)
let events h = List.iter ~f:(H.event h)

(* A background subagent a1 that started a synchronous one, a1/n1. *)
let survey h =
  H.quiet h (fun () ->
    events
      h
      [ {|{"event":"agent_start"}|}
      ; call ~id:"s1" ~name:"subagent" {|{"task":"Survey the repository"}|}
      ; tool_start
          ~id:"s1"
          ~name:"subagent"
          {|{"task":"Survey the repository"}|}
      ; start ~call:"s1" ~agent:"a1" "Survey the repository"
      ; in_a1 {|{"event":"agent_start"}|}
      ; in_a1
          {|{"event":"message_start","message":{"role":"user","text":"Survey the repository"}}|}
      ; in_a1 (call ~id:"n1" ~name:"subagent" {|{"task":"Count the modules"}|})
      ; in_a1
          (tool_start
             ~id:"n1"
             ~name:"subagent"
             {|{"task":"Count the modules"}|})
      ; in_a1 (start ~call:"n1" ~agent:"a1/n1" "Count the modules")
      ; in_n1 {|{"event":"agent_start"}|}
      ; in_n1
          {|{"event":"message_start","message":{"role":"user","text":"Count the modules"}}|}
      ; tool_end
          ~id:"s1"
          ~name:"subagent"
          "started agent a1 (Survey the repository); its result will be \
           delivered to you when it finishes"
      ; text_reply "Started a survey."
      ; {|{"event":"agent_end","messages":[]}|}
      ])
;;

let where h =
  print_endline "crumbs:";
  H.text h ~selector:".chrome-center";
  print_endline "card:";
  H.text h ~selector:".session-card";
  print_endline "shown:";
  H.elements h ~selector:"[data-subagent]";
  H.names h ~selector:".composer .editor"
;;

let%expect_test
    "a subagent's chat: its lineage in the breadcrumb and the card, Back to \
     its parent, Esc to the session"
  =
  let h = H.create () in
  survey h;
  where h;
  [%expect
    {|
    crumbs:
    New session
    card:
    (Subagents 1)
    (Survey the repository working…)
    shown:
    textarea "Message"
    |}];
  H.click h ~selector:".session-card-agent";
  where h;
  [%expect
    {|
    (Open_subagent_chat a1)
    (Focus editor)
    crumbs:
    (New session)
    /
    Survey the repository
    card:
    (Parents 1)
    (Root)
    (Subagents 1)
    (Count the modules thinking…)
    shown:
    <div class="subagent-chat">
    textarea "Message"
    |}];
  (* Its own subagent: the chain up to the session, and back up it. *)
  H.click h ~selector:".session-card-agent";
  where h;
  [%expect
    {|
    (Open_subagent_chat a1/n1)
    (Focus editor)
    crumbs:
    (New session)
    /
    (Survey the repository)
    /
    Count the modules
    card:
    (Parents 2)
    (Root) (Survey the repository)
    shown:
    <div class="subagent-chat">
    textarea "Message"
    |}];
  H.act h Subagent_back;
  H.text h ~selector:".chrome-center";
  H.text h ~selector:".session-card";
  [%expect
    {|
    (Focus editor)
    (New session)
    /
    Survey the repository
    (Parents 1)
    (Root)
    (Subagents 1)
    (Count the modules thinking…)
    |}];
  H.key h "Escape";
  where h;
  [%expect
    {|
    Subagent_back
    (Focus editor)
    crumbs:
    New session
    card:
    (Subagents 1)
    (Survey the repository working…)
    shown:
    textarea "Message"
    |}]
;;

let%expect_test
    "writing to a subagent: Enter steers it while it runs, Alt+Enter queues a \
     follow-up; once it finished, Enter carries it on"
  =
  let h = H.create () in
  survey h;
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  H.type_ h "use the tests";
  H.key h "Enter";
  H.type_ h "then summarise";
  H.key h ~alt:true "Enter";
  [%expect
    {|
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "use the tests") (max 500))) (tag Ignore))
    (Rpc (method_ steer_agent) (params ((agent_id a1) (text "use the tests")))
     (tag (Steered (agent a1) (draft "use the tests"))))
    Send_follow_up
    (Rpc (method_ push_pref)
     (params ((key history) (value "then summarise") (max 500))) (tag Ignore))
    (Rpc (method_ steer_agent)
     (params ((agent_id a1) (text "then summarise") (mode follow_up)))
     (tag (Steered (agent a1) (draft "then summarise"))))
    |}];
  H.names h ~selector:".composer .send";
  H.names h ~selector:".composer .stop";
  [%expect {| button "Sending" -- Sending… |}];
  H.quiet h (fun () ->
    events
      h
      [ in_a1 (text_reply "Done.")
      ; end_ ~call:"s1" ~agent:"a1" ~turns:2 "Done."
      ]);
  H.names h ~selector:".composer .send";
  H.names h ~selector:".composer .stop";
  H.type_ h "now the docs";
  H.key h "Enter";
  [%expect
    {|
    button "Sending" -- Sending…
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "now the docs") (max 500))) (tag Ignore))
    (Rpc (method_ steer_agent) (params ((agent_id a1) (text "now the docs")))
     (tag (Steered (agent a1) (draft "now the docs"))))
    |}];
  (* A nested one takes no messages: it says whom to write to. *)
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1/n1"));
  H.type_ h "hello";
  H.key h "Enter";
  H.text h ~selector:".toast";
  print_endline ("draft: " ^ (H.model h).draft);
  [%expect
    {|
    Send
    a1/n1 runs inside a1 (a subagent's own subagent) and takes no messages: write to a1 instead. (Dismiss)
    draft: hello
    |}]
;;

let%expect_test
    "the composer's stop stops the subagent on show, not the session"
  =
  let h = H.create () in
  survey h;
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  H.click h ~selector:".composer .stop";
  [%expect
    {|
    (Cancel_subagent a1)
    (Rpc (method_ cancel_subagent) (params ((agent_id a1))) (tag Show_error))
    |}]
;;

let%expect_test "its line in the transcript, Alt+N and /agents N open it" =
  let h = H.create () in
  survey h;
  H.act h (Open_subagent_n 1);
  H.text h ~selector:".chrome-center";
  H.key h "Escape";
  H.key h ~alt:true "1";
  H.text h ~selector:".chrome-center";
  H.key h "Escape";
  H.type_ h "/agents 1";
  H.key h "Enter";
  H.text h ~selector:".chrome-center";
  H.act h (Open_subagent_n 9);
  [%expect
    {|
    (Focus editor)
    (New session)
    /
    Survey the repository
    Subagent_back
    (Focus editor)
    (Open_subagent_n 1)
    (Focus editor)
    (New session)
    /
    Survey the repository
    Subagent_back
    (Focus editor)
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents 1") (max 500))) (tag Ignore))
    (Focus editor)
    (New session)
    /
    Survey the repository
    |}]
;;

let%expect_test "switching sessions leaves the subagent's chat" =
  let h = H.create () in
  survey h;
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  H.quiet h (fun () -> H.act h New_session);
  print_s [%sexp ((H.model h).subagent_chat : string option)];
  [%expect {| (a1) |}]
;;

(* Everything around a subagent's chat is the subagent's: the host and model
   chips, the folder and branch under the composer, the context ring; none
   of them changes it (the backend keeps what it started with), and saying
   so leaves the session alone. Back in the session, the session's. *)
let%expect_test
    "a subagent's chat shows its own host, model, folder, branch and context"
  =
  let hosts =
    {|[{"id":"backend","name":"server-laptop","cwd":"/home/lloyd","home":"/home/lloyd"},
       {"id":"c7","name":"desktop","cwd":"/home/me","home":"/home/me"}]|}
  in
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:
             [ "hosts", Jsonaf.of_string hosts
             ; "cwd", `String "/home/lloyd/projects/prigh"
             ; "thinking", `String "high"
             ; "message_count", `Number "2"
             ; "context_tokens", `Number "50000"
             ; "cost_usd", `Number "1.5"
             ]
           ())
      ()
  in
  let worktree =
    {|{"agent":"a1","task":"Fix the ACP login status","path":"/home/me/.prigh/worktrees/prigh/fix-acp-login-status","branch":"fix/acp-login-status","root":"/home/me/projects/prigh","host":"desktop"}|}
  in
  H.quiet h (fun () ->
    events
      h
      [ {|{"event":"agent_start"}|}
      ; call ~id:"s1" ~name:"subagent" {|{"task":"Fix the ACP login status"}|}
      ; sprintf
          {|{"event":"subagent_start","call_id":"s1","agent_id":"a1","task":"Fix the ACP login status","name":"ACP login status","model":"claude-sonnet-5","tools":["bash"],"host":"desktop","worktree":%s}|}
          worktree
      ; in_a1
          {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Looking."}],"stop_reason":{"type":"tool_use"},"usage":{"input":20000,"output":1000,"cache_read":0},"model":"claude-sonnet-5"}}|}
      ];
    H.reply
      h
      "list_subagents"
      (sprintf
         {|[{"id":"a1","call_id":"s1","parent":null,"task":"Fix the ACP login status","name":"ACP login status","model":"claude-sonnet-5","host":"desktop","thinking":"low","cwd":"/home/me/.prigh/worktrees/prigh/fix-acp-login-status","worktree":%s,"state":"running","started_at_ms":1791187201000,"updated_at_ms":0,"ended_at_ms":null,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":1,"stale":false,"result":null}]|}
         worktree));
  let chrome () =
    H.names h ~selector:".composer-controls button";
    H.names h ~selector:".composer-where button";
    H.names h ~selector:".context-ring";
    H.names h ~selector:".composer .editor"
  in
  chrome ();
  [%expect
    {|
    button "server-laptop" -- Tools run on server-laptop, in /home/lloyd (/host switches)
    button "Model: Claude Opus 5.5, High" -- Claude Opus 5.5, reasoning High: models, reasoning and access (Ctrl+L)
    button "Folder: ~/projects/prigh. Change it" -- ~/projects/prigh. Start in another folder (/cd)
    button "Branch: main. Change it" -- Work in another worktree (/worktree)
    button "Context 25% full, $1.50: details"
    textarea "Message"
    |}];
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  chrome ();
  [%expect
    {|
    button "The subagent's tool host: desktop" -- The subagent's tools run on desktop, set when it started
    button "The subagent's model: Claude Sonnet 5, Low" -- Claude Sonnet 5, reasoning Low: the subagent's own, set when it started
    button "The subagent's folder: ~/.prigh/worktrees/prigh/fix-acp-login-status" -- ~/.prigh/worktrees/prigh/fix-acp-login-status: the subagent's folder
    button "The subagent's branch: fix/acp-login-status" -- fix/acp-login-status: the subagent's branch
    button "Context 11% full, $0.07: details"
    textarea "Message"
    |}];
  (* Its chips and folder say why they do not change; nor do the commands. *)
  H.click h ~selector:".model-chip";
  H.click h ~selector:".where-branch";
  H.type_ h "/model gpt-6";
  H.key h "Enter";
  H.text h ~selector:".toast";
  print_endline (H.model h).draft;
  [%expect
    {|
    (Open_chip Model)
    (Expire_toast (id 0) (after_ms 6000))
    (Subagent_fixed Branch)
    (Expire_toast (id 1) (after_ms 6000))
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/model openai/gpt-6") (max 500)))
     (tag Ignore))
    ACP login status keeps the worktree and branch it started with (a subagent's cannot change): back to the main agent (Esc) to change the session's.
    ACP login status keeps the model and reasoning it started with (a subagent's cannot change): back to the main agent (Esc) to change the session's. (Dismiss)
    /model openai/gpt-6
    |}];
  H.key h "Escape";
  chrome ();
  [%expect
    {|
    Subagent_back
    (Focus editor)
    button "server-laptop" -- Tools run on server-laptop, in /home/lloyd (/host switches)
    button "Model: Claude Opus 5.5, High" -- Claude Opus 5.5, reasoning High: models, reasoning and access (Ctrl+L)
    button "Folder: ~/projects/prigh. Change it" -- ~/projects/prigh. Start in another folder (/cd)
    button "Branch: main. Change it" -- Work in another worktree (/worktree)
    button "Context 25% full, $1.50: details"
    textarea "Message"
    |}]
;;

let%expect_test
    "a subagent on another engine and host shows both on its card and in its \
     chat"
  =
  let h = H.create () in
  H.quiet h (fun () ->
    events
      h
      [ {|{"event":"agent_start"}|}
      ; call
          ~id:"s1"
          ~name:"subagent"
          {|{"task":"Audit parser","engine":"codex","host":"laptop","model":"gpt-5"}|}
      ; tool_start
          ~id:"s1"
          ~name:"subagent"
          {|{"task":"Audit parser","engine":"codex","host":"laptop","model":"gpt-5"}|}
      ; {|{"event":"subagent_start","call_id":"s1","agent_id":"a1","task":"Audit parser","name":"Audit parser","model":"codex/gpt-5","tools":[],"host":"laptop","engine":"codex","cwd":"/home/me","forked":false,"continued":false}|}
      ; tool_end
          ~id:"s1"
          ~name:"subagent"
          "started agent a1 on Codex, laptop (Audit parser); its result will \
           be delivered to you when it finishes"
      ; {|{"event":"agent_end","messages":[]}|}
      ]);
  H.text h ~selector:".tool-subagent";
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  H.names h ~selector:".composer-controls button";
  [%expect
    {|
    subagent Audit parser (Open its chat)
    button "The subagent's tool host: laptop, disconnected" -- Tools wait for laptop to reconnect: run prigh tool-host -connect (or a TUI) there, or /host picks another
    button "The subagent's model: Codex \194\183 gpt-5, Default" -- Codex · gpt-5, reasoning Default: the subagent's own, set when it started
    |}]
;;
