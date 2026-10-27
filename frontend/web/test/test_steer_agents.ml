open! Core
open Prigh_web
open Agent_events
module H = Harness

(* Steering a running background subagent from the composer: retargeting it
   (its Steer button, not merely showing it; Esc and the chip's cross back), sending
   ([steer_agent]), its queue ([agent_queues]), errors, a subagent that
   finishes, [/agents steer] and switching sessions. *)

let in_a1 = inner ~call:"s1" ~agent:"a1"
let events h = List.iter ~f:(H.event h)

let running =
  sprintf
    {|{"event":"state","state":%s}|}
    (H.state_json ~fields:[ "running", `True ] ())
;;

(* The main agent runs; it started a background subagent a1, which started
   a synchronous one, a1/n1, and runs bash. *)
let started h =
  H.quiet h (fun () ->
    events
      h
      [ running
      ; {|{"event":"agent_start"}|}
      ; call ~id:"s1" ~name:"subagent" {|{"task":"Survey the repository"}|}
      ; tool_start
          ~id:"s1"
          ~name:"subagent"
          {|{"task":"Survey the repository"}|}
      ; start ~call:"s1" ~agent:"a1" "Survey the repository"
      ; in_a1 {|{"event":"agent_start"}|}
      ; in_a1 {|{"event":"turn_start"}|}
      ; in_a1
          {|{"event":"message_start","message":{"role":"user","text":"Survey the repository"}}|}
      ; in_a1 (call ~id:"n1" ~name:"subagent" {|{"task":"Count the modules"}|})
      ; in_a1
          (tool_start
             ~id:"n1"
             ~name:"subagent"
             {|{"task":"Count the modules"}|})
      ; in_a1 (start ~call:"n1" ~agent:"a1/n1" "Count the modules")
      ; tool_end
          ~id:"s1"
          ~name:"subagent"
          "started agent a1 (Survey the repository); its result will be \
           delivered to you when it finishes"
      ])
;;

let agent_queues queues =
  let texts l = `Array (List.map l ~f:(fun t -> `String t)) in
  Jsonaf.to_string
    (`Object
        [ "event", `String "agent_queues"
        ; ( "agents"
          , `Array
              (List.map queues ~f:(fun (id, steer, follow_up) ->
                 `Object
                   [ "agent_id", `String id
                   ; "steer_texts", texts steer
                   ; "follow_up_texts", texts follow_up
                   ])) )
        ])
;;

let recipient h = print_s [%sexp ((H.model h).recipient : Recipient.t)]

(* The composer: the chip, what the editor holds and says, the send
   button. *)
let composer h =
  recipient h;
  H.text h ~selector:".steer-chip";
  H.names h ~selector:".composer .editor";
  H.names h ~selector:".composer .send";
  print_endline ("draft: " ^ (H.model h).draft)
;;

let%expect_test
    "showing a subagent leaves the composer on the main agent; its Steer \
     button retargets it; Enter steers it, Alt+Enter queues a follow-up, Esc \
     goes back"
  =
  let h = H.create () in
  started h;
  composer h;
  [%expect
    {|
    Main
    textarea "Message"
    button "Steer" -- Steer (Enter)
    icon "(no name)"
    draft:
    |}];
  (* Shown in the panel, a nested one takes no messages. *)
  H.act h (Select_item (Agent "a1/n1"));
  recipient h;
  H.names h ~selector:".agents-steer";
  [%expect
    {|
    Main
    button "Steer" -- a1/n1 runs inside a1 (a subagent's own subagent) and takes no messages: steer a1 instead.
    |}];
  (* Showing a1 does not steer it: the composer stays the main agent's, and
     its Steer button offers to. *)
  H.act h (Select_item (Agent "a1"));
  composer h;
  H.show h ~selector:".agents-steer";
  [%expect
    {|
    Main
    textarea "Message"
    button "Steer" -- Steer (Enter)
    icon "(no name)"
    draft:
    <button type="button"
            title="Write to a1 from the composer (/agents steer)"
            aria-pressed="false"
            class="agents-steer btn small"
            @on_click>
      <icon class="target"> </icon>
      Steer
    </button>
    |}];
  H.click h ~selector:".agents-steer";
  composer h;
  H.show h ~selector:".steer-chip";
  H.names h ~selector:".agents-steer";
  [%expect
    {|
    (Set_recipient (Agent a1))
    (Focus editor)
    (Agent a1)
    Steering a1 Survey the repository (Write to the main agent again (Esc))
    textarea "Message"
    button "Steer" -- Steer a1 (Enter)
    icon "(no name)"
    draft:
    <div title="Your messages go to subagent a1: Enter reaches it at its next step, Alt+Enter when it ends its turn"
         class="steer-chip">
      <icon class="target"> </icon>
      <span class="steer-chip-label">
        Steering
        <span class="steer-chip-id"> a1 </span>
      </span>
      <span class="steer-chip-task"> Survey the repository </span>
      <button type="button"
              title="Write to the main agent again (Esc)"
              aria-label="Stop steering"
              class="btn ghost icon steer-chip-close"
              @on_click>
        <icon class="close"> </icon>
      </button>
    </div>
    button "Steering" -- Your messages go to it (Esc in the editor: to the main agent)
    |}];
  H.show h ~selector:".editor";
  [%expect
    {|
    <textarea id="editor"
              placeholder="Steer a1 (Alt+Enter: after its turn)…"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}];
  H.type_ h "use the tests";
  H.key h "Enter";
  H.type_ h "then summarise";
  H.key h ~alt:true "Enter";
  composer h;
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
    (Agent a1)
    Steering a1 Survey the repository (Write to the main agent again (Esc))
    textarea "Message"
    button "Sending" -- Sending…
    draft:
    |}];
  H.reply h "steer_agent" "{}";
  H.reply h "steer_agent" "{}";
  (* Esc goes back to the main agent: it does not stop the run. *)
  H.key h "Escape";
  composer h;
  H.names h ~selector:".agents-steer";
  [%expect
    {|
    (Set_recipient Main)
    (Focus editor)
    Main
    textarea "Message"
    button "Steer" -- Steer (Enter)
    icon "(no name)"
    draft:
    button "Steer" -- Write to a1 from the composer (/agents steer)
    |}];
  (* The Steer button, then the chip's cross. *)
  H.click h ~selector:".agents-steer";
  recipient h;
  H.click h ~selector:".steer-chip-close";
  recipient h;
  [%expect
    {|
    (Set_recipient (Agent a1))
    (Focus editor)
    (Agent a1)
    (Set_recipient Main)
    (Focus editor)
    Main
    |}];
  (* Steering again: it outlasts going back to the list, showing another
     one and closing the panel, until Esc. *)
  H.click h ~selector:".agents-steer";
  H.act h Agents_back;
  H.act h (Select_item (Agent "a1/n1"));
  H.act h Toggle_subagents;
  composer h;
  H.key h "Escape";
  recipient h;
  [%expect
    {|
    (Set_recipient (Agent a1))
    (Focus editor)
    (Focus editor)
    (Agent a1)
    Steering a1 Survey the repository (Write to the main agent again (Esc))
    textarea "Message"
    button "Steer" -- Steer a1 (Enter)
    icon "(no name)"
    draft:
    (Set_recipient Main)
    (Focus editor)
    Main
    |}]
;;

let%expect_test
    "a subagent's queue is listed with the main agent's: edit, remove, take \
     back"
  =
  let h = H.create () in
  started h;
  H.quiet h (fun () ->
    H.act h (Select_item (Agent "a1"));
    H.act h (Set_recipient (Agent "a1"));
    H.event
      h
      {|{"event":"queue_update","steer":1,"follow_up":0,"steer_texts":["main steer"],"follow_up_texts":[]}|};
    H.event
      h
      (agent_queues
         [ "a1", [ "use the tests"; "and the docs" ], [ "then summarise" ] ]));
  H.text h ~selector:".queue";
  H.text h ~selector:".status-extras .queued";
  H.show h ~selector:".queue li:nth-child(2)";
  [%expect
    {|
    Steers this turn main steer
    (Edit) (Remove from the queue)
    To a1, next step use the tests
    (Edit) (Remove from the queue)
    To a1, next step and the docs
    (Edit) (Remove from the queue)
    To a1, after its turn then summarise
    (Edit) (Remove from the queue)
    (3 queued for a1)
    <li class="queued steer to-agent">
      <span title="Reaches subagent a1 after its current tool calls" class="queued-tag"> To a1, next step </span>
      <span dir="auto" class="queued-text"> use the tests </span>
      <div class="queued-actions">
        <button type="button"
                title="Edit: take it back into the editor"
                class="btn ghost queued-edit"
                @on_click>
          <icon class="pencil"> </icon>
          <span class="queued-action-label"> Edit </span>
        </button>
        <button type="button"
                title="Remove from the queue"
                class="btn ghost icon queued-remove"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
    </li>
    |}];
  (* The panel's list says how many wait. *)
  H.quiet h (fun () -> H.act h Agents_back);
  H.text h ~selector:".agents-row.running .row-meta";
  [%expect
    {|
    a1  3 queued  deepseek-flash  1 turn  1 tool call
    a1/n1  deepseek-flash
    |}];
  H.quiet h (fun () -> H.act h (Select_item (Agent "a1")));
  (* Remove: gone at once. *)
  H.click h ~nth:2 ~selector:".queued-remove";
  H.text h ~selector:".queue";
  [%expect
    {|
    (Remove_queued (Agent a1) ((kind Steer) (index 1) (text "and the docs")))
    (Rpc (method_ remove_agent_queued)
     (params ((agent_id a1) (queue steer) (index 1) (text "and the docs")))
     (tag (Unqueued "and the docs")))
    Steers this turn main steer
    (Edit) (Remove from the queue)
    To a1, next step use the tests
    (Edit) (Remove from the queue)
    To a1, after its turn then summarise
    (Edit) (Remove from the queue)
    |}];
  H.reply h "remove_agent_queued" {|{"text":"and the docs"}|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2940))
    Removed from the queue: and the docs
    |}];
  (* Edit: back into the editor. *)
  H.click h ~nth:1 ~selector:".queued-edit";
  H.reply h "remove_agent_queued" {|{"text":"use the tests"}|};
  composer h;
  [%expect
    {|
    (Edit_queued (Agent a1) ((kind Steer) (index 0) (text "use the tests")))
    (Rpc (method_ remove_agent_queued)
     (params ((agent_id a1) (queue steer) (index 0) (text "use the tests")))
     (tag Dequeued))
    (Focus editor)
    (Agent a1)
    Steering a1 Survey the repository (Write to the main agent again (Esc))
    textarea "Message"
    button "Steer" -- Steer a1 (Enter)
    icon "(no name)"
    draft: use the tests
    |}];
  (* Alt+↑ takes back the recipient's last queued message. *)
  H.key h ~alt:true "ArrowUp";
  H.reply h "dequeue_agent" {|{"text":"then summarise"}|};
  print_endline (H.model h).draft;
  H.key h ~alt:true "ArrowUp";
  H.reply h "dequeue_agent" "null";
  H.text h ~selector:".toast";
  [%expect
    {|
    Dequeue
    (Rpc (method_ dequeue_agent) (params ((agent_id a1))) (tag Dequeued))
    (Focus editor)
    then summarise

    use the tests
    Dequeue
    (Rpc (method_ dequeue_agent) (params ((agent_id a1))) (tag Dequeued))
    (Expire_toast (id 1) (after_ms 2500))
    Removed from the queue: and the docs
    Nothing is queued
    |}];
  (* A stale removal: the backend's queues again. *)
  H.quiet h (fun () -> H.event h (agent_queues [ "a1", [ "x" ], [] ]));
  H.click h ~selector:".queued.to-agent .queued-remove";
  H.fail
    h
    "remove_agent_queued"
    "that message is no longer queued: it was delivered or removed already";
  H.reply
    h
    "get_pending"
    {|{"steer_texts":[],"follow_up_texts":[],"agents":[{"agent_id":"a1","steer_texts":["y"],"follow_up_texts":[]}],"confirms":[],"questions":[]}|};
  H.text h ~selector:".queue";
  [%expect
    {|
    (Remove_queued (Agent a1) ((kind Steer) (index 0) (text x)))
    (Rpc (method_ remove_agent_queued)
     (params ((agent_id a1) (queue steer) (index 0) (text x)))
     (tag (Unqueued x)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    To a1, next step y
    (Edit) (Remove from the queue)
    |}]
;;

let%expect_test
    "a refused message stays; a subagent that finishes gives the composer back"
  =
  let h = H.create () in
  started h;
  H.quiet h (fun () ->
    H.act h (Select_item (Agent "a1"));
    H.click h ~selector:".agents-steer");
  H.type_ h "hurry up";
  H.quiet h (fun () -> H.key h "Enter");
  H.fail
    h
    "steer_agent"
    "a1 has finished and its report was delivered to the main agent: tell the \
     main agent instead, or have it start a new subagent";
  H.text h ~selector:".toast";
  composer h;
  [%expect
    {|
    (Set_recipient (Agent a1))
    Send
    Couldn't send it to a1: a1 has finished and its report was delivered to the main agent: tell the main agent instead, or have it start a new subagent. Your message is back in the editor. (Dismiss)
    (Agent a1)
    Steering a1 Survey the repository (Write to the main agent again (Esc))
    textarea "Message"
    button "Steer" -- Steer a1 (Enter)
    icon "(no name)"
    draft: hurry up
    |}];
  (* Images do not go to a subagent. *)
  H.quiet h (fun () ->
    H.act
      h
      (Add_image
         { Prigh_protocol.Image.mime_type = "image/png"
         ; data = "iVBORw0KGgo="
         ; bytes = 8
         }));
  H.key h "Enter";
  H.quiet h (fun () -> H.act h (Remove_image 0));
  [%expect {| Send |}];
  H.text h ~selector:".toast";
  [%expect
    {| a1 takes text only: remove the images and files, or Esc to send them to the main agent. (Dismiss) |}];
  (* It finishes: the composer is the main agent's, with a note; no Steer
     button, and it cannot be steered any more. *)
  events
    h
    [ in_a1 (text_reply "Surveyed.")
    ; end_ ~call:"s1" ~agent:"a1" ~turns:1 "Surveyed."
    ];
  H.text h ~selector:".toast";
  composer h;
  H.elements h ~selector:".agents-steer";
  [%expect
    {|
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Expire_toast (id 2) (after_ms 3740))
    a1 takes text only: remove the images and files, or Esc to send them to the main agent. (Dismiss)
    a1 has finished: you are writing to the main agent again
    Main
    textarea "Message"
    button "Steer" -- Steer (Enter)
    icon "(no name)"
    draft: hurry up
    |}];
  H.act h (Set_recipient (Agent "a1"));
  H.text h ~selector:".toast:last-child";
  [%expect
    {| a1 has finished and takes no more messages: tell the main agent instead. (Dismiss) |}]
;;

let%expect_test "a delivered steer: in the subagent's transcript, and said" =
  let h = H.create () in
  started h;
  H.quiet h (fun () -> H.act h (Select_item (Agent "a1")));
  H.event
    h
    (in_a1
       {|{"event":"message_start","message":{"role":"user","text":"[The user sends you this while you work: take it into account and carry on.]\n\nuse the tests","origin":{"body":"use the tests","notes":[{"kind":"steer","title":"You (steer)","text":"[The user sends you this while you work: take it into account and carry on.]"}]}}}|});
  (* The main agent's own messages to it are not the user's. *)
  H.event
    h
    (in_a1
       {|{"event":"message_start","message":{"role":"user","text":"[Your caller sends you this while you work.]\n\nalso the docs","origin":{"body":"also the docs","notes":[{"kind":"steer","title":"Main agent (steer)","text":"[Your caller sends you this while you work.]"}]}}}|});
  H.text h ~selector:".toast";
  H.show h ~selector:".agents-transcript .msg.noted";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2940))
    Steer delivered to a1: use the tests
    <div data-entry="2" class="msg noted">
      <details data-kind="steer" class="note">
        <summary>
          <span aria-hidden="true" class="glyph"> → </span>
          <span class="title"> You (steer) </span>
        </summary>
        <pre class="note-text">
          [The user sends you this while you work: take it into account and carry on.]
        </pre>
      </details>
      <div class="msg user">
        <div dir="auto" class="bubble"> use the tests </div>
      </div>
    </div>
    <div data-entry="3" class="msg noted">
      <details data-kind="steer" class="note">
        <summary>
          <span aria-hidden="true" class="glyph"> → </span>
          <span class="title"> Main agent (steer) </span>
        </summary>
        <pre class="note-text"> [Your caller sends you this while you work.] </pre>
      </details>
      <div class="msg user">
        <div dir="auto" class="bubble"> also the docs </div>
      </div>
    </div>
    |}];
  H.event
    h
    (in_a1
       {|{"event":"message_start","message":{"role":"user","text":"[The user sends you this as you finish: deal with it before you report.]\n\nthen summarise","origin":{"body":"then summarise","notes":[{"kind":"steer","title":"You (follow-up)","text":"[The user sends you this as you finish: deal with it before you report.]"}]}}}|});
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 3140))
    Follow-up delivered to a1: then summarise
    |}]
;;

let%expect_test "/agents steer and /agents follow-up" =
  let h = H.create () in
  started h;
  let run text =
    H.type_ h text;
    H.act h Send
  in
  run "/agents steer 1 hurry up";
  run "/agents follow-up a1 then report in a table";
  H.reply h "steer_agent" "{}";
  H.reply h "steer_agent" "{}";
  recipient h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/agents steer 1 hurry up") (max 500)))
     (tag Ignore))
    (Rpc (method_ steer_agent) (params ((agent_id a1) (text "hurry up")))
     (tag (Steered (agent a1) (draft "/agents steer 1 hurry up"))))
    (Rpc (method_ push_pref)
     (params
      ((key history) (value "/agents follow-up a1 then report in a table")
       (max 500)))
     (tag Ignore))
    (Rpc (method_ steer_agent)
     (params ((agent_id a1) (text "then report in a table") (mode follow_up)))
     (tag
      (Steered (agent a1) (draft "/agents follow-up a1 then report in a table"))))
    Main
    |}];
  (* Refused here: what to do instead, the command stays. *)
  H.quiet h (fun () -> run "/agents steer 1");
  H.text h ~selector:".toast:last-child";
  print_endline (H.model h).draft;
  H.quiet h (fun () ->
    H.act h (Set_draft "");
    run "/agents steer 9 hurry");
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    Usage: /agents steer <n|id> <message> (/agents lists them) (Dismiss)
    /agents steer 1
    No subagent 9: give its number (1-2) or id; /agents lists them. (Dismiss)
    |}];
  (* Refused by the backend: back in the editor. *)
  H.quiet h (fun () ->
    H.act h (Set_draft "");
    run "/agents steer a9 hurry");
  H.fail h "steer_agent" {|unknown subagent "a9"; known: a1|};
  H.text h ~selector:".toast:last-child";
  print_endline (H.model h).draft;
  [%expect
    {|
    Couldn't send it to a9: unknown subagent "a9"; known: a1. Your message is back in the editor. (Dismiss)
    /agents steer a9 hurry
    |}]
;;

let%expect_test "switching sessions forgets the steering and the queues" =
  let h = H.create () in
  started h;
  H.quiet h (fun () ->
    H.act h (Set_recipient (Agent "a1"));
    H.event h (agent_queues [ "a1", [ "use the tests" ], [] ]));
  recipient h;
  H.elements h ~selector:".queue";
  H.elements h ~selector:".steer-chip";
  [%expect
    {|
    (Agent a1)
    <ul class="queue">
    <div class="steer-chip">
    |}];
  H.quiet h (fun () ->
    H.event
      h
      (sprintf
         {|{"event":"state","state":%s}|}
         (H.state_json ~fields:[ "session_id", `String "s2" ] ())));
  recipient h;
  H.elements h ~selector:".queue";
  H.elements h ~selector:".steer-chip";
  print_s [%sexp ((H.model h).agent_queues : Prigh_protocol.Agent_queue.t list)];
  [%expect
    {|
    Main
    ()
    |}]
;;

let steer_message =
  {|{"role":"user","text":"[The user sends you this while you work: take it into account and carry on.]\n\nuse the tests","origin":{"body":"use the tests","notes":[{"kind":"steer","title":"You (steer)","text":"[The user sends you this while you work: take it into account and carry on.]"}]}}|}
;;

(* What a1's transcript shows, one line per entry. *)
let a1_transcript h =
  match Chat.find_subagent (H.model h).chat "a1" with
  | None -> print_endline "a1: no transcript"
  | Some s ->
    List.iter (Chat.entries s.chat) ~f:(fun (e : Chat.Entry.t) ->
      print_endline
        (match e with
         | User u -> "user: " ^ String.prefix (Option.value_map u.origin ~default:u.text ~f:(fun o -> Option.value o.body ~default:u.text)) 40
         | Assistant { message; _ } ->
           "assistant: "
           ^ String.concat
               (List.filter_map message.content ~f:(function
                  | Prigh_protocol.Content.Text t -> Some t
                  | _ -> None))
         | Shell _ -> "shell"
         | Compaction _ -> "compaction"))
;;

let a1_list =
  {|[{"id":"a1","call_id":"s1","parent":null,"task":"Survey the repository","model":"deepseek-flash","started_at_ms":0,"turns":1,"tool_calls":1,"stale":false},
     {"id":"a1/n1","call_id":"n1","parent":"a1","task":"Count the modules","model":"deepseek-flash","started_at_ms":0,"turns":0,"tool_calls":0,"stale":false}]|}
;;

let%expect_test
    "steering, then a reconnect while it runs: the steering stays, its \
     transcript is fetched once and goes on"
  =
  let h = H.create () in
  started h;
  H.quiet h (fun () ->
    H.act h (Select_item (Agent "a1"));
    H.act h (Set_recipient (Agent "a1"));
    H.type_ h "use the tests";
    H.key h "Enter";
    H.reply h "steer_agent" "{}";
    events
      h
      [ agent_queues [ "a1", [ "use the tests" ], [] ]
      ; in_a1 (sprintf {|{"event":"message_start","message":%s}|} steer_message)
      ; in_a1 (sprintf {|{"event":"message_end","message":%s}|} steer_message)
      ; agent_queues []
      ]);
  a1_transcript h;
  [%expect {|
    Send
    user: Survey the repository
    assistant:
    user: use the tests
    |}];
  H.act h Backend_closed;
  H.quiet h (fun () ->
    H.act h (Reply (Reconnect 1, Ok (Jsonaf.of_string "{}")));
    H.reply h "get_state" running);
  H.reply
    h
    "get_messages_page"
    (H.page
       {|[{"role":"user","text":"survey"},
          {"role":"assistant","content":[{"type":"tool_call","id":"s1","name":"subagent","arguments":"{\"task\":\"Survey the repository\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},
          {"role":"tool_result","tool_call_id":"s1","tool_name":"subagent","text":"started agent a1 (Survey the repository); its result will be delivered to you when it finishes","is_error":false}]|});
  [%expect {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    (Rpc (method_ get_subagent) (params ((id s1) (elide true)))
     (tag (Subagent s1)))
    (Expire_toast (id 2) (after_ms 2500))
    |}];
  (* Its events go on while the snapshot is on its way. *)
  H.event h (in_a1 (text_reply "noted"));
  H.reply
    h
    "get_subagent"
    (sprintf
       {|{"subagent":{"id":"a1","call_id":"s1","parent":null,"task":"Survey the repository","model":"deepseek-flash","state":"running","turns":2},
          "messages":[{"role":"user","text":"Survey the repository"},%s,{"role":"assistant","content":[{"type":"text","text":"noted"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}]}|}
       steer_message);
  H.reply h "list_subagents" a1_list;
  H.event h (in_a1 (text_reply "carrying on"));
  a1_transcript h;
  recipient h;
  print_s
    [%sexp
      (List.map (H.model h).agents.agents ~f:(fun a -> a.id) : string list)];
  [%expect {|
    user: Survey the repository
    user: use the tests
    assistant: noted
    assistant: carrying on
    (Agent a1)
    (a1 a1/n1)
    |}]
;;
