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
      ; {|{"event":"agent_end"}|}
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
  [%expect {| |}];
  H.click h ~selector:".session-card [data-agent=a1]";
  where h;
  [%expect {| |}];
  (* Its own subagent: the chain up to the session, and back up it. *)
  H.click h ~selector:".session-card [data-agent=a1/n1]";
  where h;
  [%expect {| |}];
  H.act h Subagent_back;
  H.text h ~selector:".chrome-center";
  [%expect {| |}];
  H.key h "Escape";
  where h;
  [%expect {| |}]
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
  [%expect {| |}];
  H.names h ~selector:".composer .send, .composer .stop";
  [%expect {| |}];
  H.quiet h (fun () ->
    events
      h
      [ in_a1 (text_reply "Done.")
      ; end_ ~call:"s1" ~agent:"a1" ~turns:2 "Done."
      ]);
  H.names h ~selector:".composer .send, .composer .stop";
  H.type_ h "now the docs";
  H.key h "Enter";
  [%expect {| |}];
  (* A nested one takes no messages: it says whom to write to. *)
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1/n1"));
  H.type_ h "hello";
  H.key h "Enter";
  [%expect {| |}]
;;

let%expect_test "the composer's stop stops the subagent on show, not the session"
  =
  let h = H.create () in
  survey h;
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  H.click h ~selector:".composer .stop";
  [%expect {| |}]
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
  [%expect {| |}]
;;

let%expect_test "switching sessions leaves the subagent's chat" =
  let h = H.create () in
  survey h;
  H.quiet h (fun () -> H.act h (Open_subagent_chat "a1"));
  H.quiet h (fun () -> H.act h New_session);
  print_s
    [%sexp ((H.model h).subagent_chat : string option)];
  [%expect {| |}]
;;
