open! Core

let%expect_test "startup, a prompt and its streamed reply" =
  let h = Harness.create ~verbose:true () in
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Set_url_session s1)
    Scroll_to_bottom
    (Rpc (method_ get_messages) (params ()) (tag Messages))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    |}];
  Harness.act h (Set_draft "hello");
  Harness.act h Send;
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  Harness.event
    h
    {|{"event":"message_update","partial":{"role":"assistant","content":[{"type":"text","text":"Hi **there**"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"text_delta","text":"Hi"}}|};
  Harness.show h ~selector:".entries";
  [%expect
    {|
    (Save_history (hello))
    (Rpc (method_ prompt) (params ((text hello))) (tag Show_error))
    <div class="entries">
      <div class="msg user">
        <div class="bubble"> hello </div>
      </div>
      <div class="assistant msg streaming">
        <div class="markdown">
          <p>
            Hi
            <strong> there </strong>
          </p>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "messages' times: in the browser's zone, relative to its day" =
  let h = Harness.create () in
  (* 2026-10-05 10:00 UTC; Paris is two hours ahead. *)
  Harness.act h (Set_utc_offset (Time_ns.Span.of_hr 2.));
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello","at":1791194280000}}|};
  Harness.event
    h
    {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Hi"}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"m","at":1791194350000}}|};
  let show () = Harness.text h ~selector:".entries" in
  show ();
  [%expect
    {|
    hello
    11:58
    Hi
    m · 10 in · 5 out · 11:59
    |}];
  (* Ten hours behind UTC, it has just turned midnight: they were yesterday. *)
  Harness.act h (Set_utc_offset (Time_ns.Span.of_hr (-10.)));
  show ();
  [%expect
    {|
    hello
    Yesterday 23:58
    Hi
    m · 10 in · 5 out · Yesterday 23:59
    |}];
  (* A day later in Paris. *)
  Harness.act h (Set_utc_offset (Time_ns.Span.of_hr 2.));
  Harness.act h (Tick (Time_ns.add Harness.now (Time_ns.Span.of_day 1.)));
  show ();
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    hello
    Yesterday 11:58
    Hi
    m · 10 in · 5 out · Yesterday 11:59
    |}];
  Harness.show h ~selector:".msg.user time";
  [%expect
    {| <time title="Monday 5 October 2026, 11:58:00" datetime="2026-10-05T09:58:00Z" class="time"> Yesterday 11:58 </time> |}]
;;
