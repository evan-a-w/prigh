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
  [%expect {|
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
  [%expect {|
    (Open_subagent_chat a1)
    (Focus editor)
    crumbs:
    (New session)
    /
    Survey the repository
    card:
    (Parents 1)
    (The session)
    (Subagents 1)
    (Count the modules thinking…)
    shown:
    <div class="subagent-chat">
    textarea "Message"
    |}];
  (* Its own subagent: the chain up to the session, and back up it. *)
  H.click h ~selector:".session-card-agent";
  where h;
  [%expect {|
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
    (The session) (Survey the repository)
    shown:
    <div class="subagent-chat">
    textarea "Message"
    |}];
  H.act h Subagent_back;
  H.text h ~selector:".chrome-center";
  H.text h ~selector:".session-card";
  [%expect {|
    (Focus editor)
    (New session)
    /
    Survey the repository
    (Parents 1)
    (The session)
    (Subagents 1)
    (Count the modules thinking…)
    |}];
  H.key h "Escape";
  where h;
  [%expect {|
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
  [%expect {|
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
  [%expect {|
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
  [%expect {|
    Send
    a1/n1 runs inside a1 (a subagent's own subagent) and takes no messages: write to a1 instead. (Dismiss)
    draft: hello
    |}]
;;

let%expect_test "the composer's stop stops the subagent on show, not the session"
  =
  let h = H.create () in
  survey h;
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  H.click h ~selector:".composer .stop";
  [%expect {|
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
  [%expect {|
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
  print_s
    [%sexp ((H.model h).subagent_chat : string option)];
  [%expect {| (a1) |}]
;;
