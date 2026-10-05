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
