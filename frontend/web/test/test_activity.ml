open! Core
module H = Harness

(* What the agent is doing, in the status line and the reply it streams: a
   slow model, a thinking one and a failing one look different. *)

let assistant content =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"claude-opus-5-5"}|}
    (String.concat ~sep:"," content)
;;

let event fields = sprintf {|{"type":"event",%s}|} fields

let update delta content =
  event
    (sprintf
       {|"event":"message_update","partial":%s,"delta":%s|}
       (assistant content)
       delta)
;;

let show h =
  H.text h ~selector:".status .running";
  H.show h ~selector:".msg.assistant.streaming";
  print_endline "---"
;;

let%expect_test
    "waiting, thinking without text, a retry and its attempt, then writing"
  =
  let h = H.create () in
  H.event
    h
    (event
       (sprintf
          {|"event":"state","state":%s|}
          (H.state_json ~fields:[ "running", `True ] ())));
  H.event h (event {|"event":"agent_start"|});
  H.event h (event {|"event":"turn_start"|});
  H.event
    h
    (event (sprintf {|"event":"message_start","message":%s|} (assistant [])));
  show h;
  (* Newer models' thinking has no text to show: a block starts, nothing
     more comes until it ends. *)
  H.event
    h
    (update
       {|{"type":"thinking_delta","text":""}|}
       [ {|{"type":"thinking","text":""}|} ]);
  show h;
  H.event
    h
    (event
       {|"event":"retry","attempt":2,"max_attempts":6,"error":"overloaded_error: Overloaded","delay_ms":2000|});
  show h;
  H.event
    h
    (event (sprintf {|"event":"message_start","message":%s|} (assistant [])));
  show h;
  H.event
    h
    (update
       {|{"type":"text_delta","text":"Hello"}|}
       [ {|{"type":"text","text":"Hello"}|} ]);
  H.text h ~selector:".status .running";
  [%expect
    {|
    Waiting for the model
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="pending">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="label"> Waiting for the model… </span>
      </div>
    </div>
    ---
    Thinking
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="live thinking">
        <div class="thinking-head">
          <span aria-hidden="true" class="lamp"> </span>
          <span class="label"> Thinking… </span>
        </div>
      </div>
    </div>
    ---
    Retrying in 2s (attempt 2/6): overloaded_error: Overloaded
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="pending">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="label"> Retrying in 2s (attempt 2 of 6)… </span>
        <div class="detail"> after: overloaded_error: Overloaded </div>
      </div>
    </div>
    ---
    Waiting for the model (attempt 2/6, after: overloaded_error: Overloaded)
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="pending">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="label"> Waiting for the model (attempt 2 of 6)… </span>
        <div class="detail"> after: overloaded_error: Overloaded </div>
      </div>
    </div>
    ---
    Writing
    |}]
;;
