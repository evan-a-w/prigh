open! Core
module H = Harness

(* Queued steers and follow-ups, listed above the composer while they wait. *)

let running =
  sprintf
    {|{"event":"state","state":%s}|}
    (H.state_json ~fields:[ "running", `True ] ())
;;

let queue_update ~steer ~follow_up =
  let texts l =
    Jsonaf.to_string (`Array (List.map l ~f:(fun t -> `String t)))
  in
  sprintf
    {|{"event":"queue_update","steer":%d,"follow_up":%d,"steer_texts":%s,"follow_up_texts":%s}|}
    (List.length steer)
    (List.length follow_up)
    (texts steer)
    (texts follow_up)
;;

let queue h = H.text h ~selector:".queue"

let%expect_test
    "queued messages are listed with their kind, and can be edited or removed"
  =
  let h = H.create () in
  H.quiet h (fun () -> H.event h running);
  H.elements h ~selector:".queue";
  [%expect {| |}];
  H.type_ h "also check the docs";
  H.act h Send;
  H.type_ h "then write the changelog";
  H.act h Send_follow_up;
  H.quiet h (fun () ->
    H.event
      h
      (queue_update
         ~steer:[ "also check the docs" ]
         ~follow_up:[ "then write the changelog"; "and a\nsecond line" ]);
    H.reply h "steer" "{}";
    H.reply h "follow_up" "{}");
  queue h;
  H.show h ~selector:".queued:first-child";
  [%expect
    {|
    (Save_history ("also check the docs"))
    Scroll_to_bottom
    (Rpc (method_ steer) (params ((text "also check the docs")))
     (tag (Sent (text "also check the docs") (images ()))))
    (Save_history ("then write the changelog" "also check the docs"))
    Scroll_to_bottom
    (Rpc (method_ follow_up) (params ((text "then write the changelog")))
     (tag (Sent (text "then write the changelog") (images ()))))
    Steers this turn also check the docs
    (Edit) (Remove from the queue)
    After this turn then write the changelog
    (Edit) (Remove from the queue)
    After this turn and a
    second line
    (Edit) (Remove from the queue)
    <li class="queued steer">
      <span title="Reaches the agent after its current tool calls, in this turn" class="queued-tag"> Steers this turn </span>
      <span class="queued-text"> also check the docs </span>
      <div class="queued-actions">
        <button type="button"
                title="Edit: take it back into the editor"
                aria-label="Edit: take it back into the editor"
                class="btn ghost queued-edit"
                @on_click>
          <icon class="pencil"> </icon>
          <span class="queued-action-label"> Edit </span>
        </button>
        <button type="button"
                title="Remove from the queue"
                aria-label="Remove from the queue"
                class="btn ghost icon queued-remove"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
    </li>
    |}];
  (* Remove: gone at once, acknowledged once the backend took it out. *)
  H.click h ~nth:1 ~selector:".queued-remove";
  queue h;
  [%expect
    {|
    (Remove_queued
     ((kind Follow_up) (index 0) (text "then write the changelog")))
    (Rpc (method_ remove_queued)
     (params ((queue follow_up) (index 0) (text "then write the changelog")))
     (tag (Unqueued "then write the changelog")))
    Steers this turn also check the docs
    (Edit) (Remove from the queue)
    After this turn and a
    second line
    (Edit) (Remove from the queue)
    |}];
  H.event
    h
    (queue_update
       ~steer:[ "also check the docs" ]
       ~follow_up:[ "and a\nsecond line" ]);
  H.reply
    h
    "remove_queued"
    {|{"text":"then write the changelog","attachments":[]}|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4000))
    Removed from the queue: then write the changelog
    |}];
  (* Edit: back into the editor, before what is being written. *)
  H.type_ h "draft";
  H.click h ~selector:".queued-edit";
  H.event h (queue_update ~steer:[] ~follow_up:[ "and a\nsecond line" ]);
  H.reply h "remove_queued" {|{"text":"also check the docs","attachments":[]}|};
  print_endline (H.model h).draft;
  queue h;
  [%expect
    {|
    (Edit_queued ((kind Steer) (index 0) (text "also check the docs")))
    (Rpc (method_ remove_queued)
     (params ((queue steer) (index 0) (text "also check the docs")))
     (tag Dequeued))
    (Focus editor)
    also check the docs

    draft
    After this turn and a
    second line
    (Edit) (Remove from the queue)
    |}]
;;

let%expect_test
    "a message delivered meanwhile: said, and the list is the backend's again"
  =
  let h = H.create () in
  H.quiet h (fun () ->
    H.event h running;
    H.event h (queue_update ~steer:[ "a"; "b" ] ~follow_up:[]));
  H.click h ~nth:1 ~selector:".queued-remove";
  queue h;
  [%expect
    {|
    (Remove_queued ((kind Steer) (index 1) (text b)))
    (Rpc (method_ remove_queued) (params ((queue steer) (index 1) (text b)))
     (tag (Unqueued b)))
    Steers this turn a
    (Edit) (Remove from the queue)
    |}];
  H.fail
    h
    "remove_queued"
    "that message is no longer queued: it was delivered or removed already";
  H.reply
    h
    "get_pending"
    {|{"steer_texts":["b"],"follow_up_texts":[],"confirms":[],"questions":[]}|};
  H.text h ~selector:".toast";
  queue h;
  [%expect
    {|
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    That message is no longer queued: it was delivered or removed already
    Steers this turn b
    (Edit) (Remove from the queue)
    |}]
;;

let%expect_test "delivery is acknowledged when the transcript has it" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.event h running;
    H.event h (queue_update ~steer:[ "steer me" ] ~follow_up:[ "after that" ]));
  (* The steer leaves the queue, then reaches the transcript. *)
  H.event h (queue_update ~steer:[] ~follow_up:[ "after that" ]);
  H.elements h ~selector:".toast";
  [%expect {| |}];
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"steer me"}}|};
  H.text h ~selector:".toast";
  queue h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4000))
    Steer delivered: steer me
    After this turn after that
    (Edit) (Remove from the queue)
    |}];
  (* The run ends; the follow-up starts the next one. *)
  H.event h {|{"event":"agent_end","messages":[]}|};
  H.event h (queue_update ~steer:[] ~follow_up:[]);
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"after that"}}|};
  H.text h ~selector:".toast:last-child";
  H.elements h ~selector:".queue";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 4000))
    Follow-up delivered: after that
    |}]
;;

let%expect_test "stopping the run: what was queued is not called delivered" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.event h running;
    H.event h (queue_update ~steer:[ "steer me" ] ~follow_up:[]));
  H.act h Abort;
  H.event h (queue_update ~steer:[] ~follow_up:[]);
  H.event h {|{"event":"agent_end","messages":[]}|};
  H.reply h "abort" {|{"restored":["steer me"]}|};
  (* Sent again later as a prompt of its own: not a delivery. *)
  H.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"steer me"}}|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Rpc (method_ abort) (params ()) (tag Restored))
    (Expire_toast (id 0) (after_ms 4000))
    (Focus editor)
    Stopped; 1 queued message back in the editor
    |}]
;;
