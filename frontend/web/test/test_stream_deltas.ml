open! Core
open! Expect_test_helpers_core

let partial content =
  sprintf
    {|{"role":"assistant","content":%s,"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    content
;;

let start = sprintf {|{"event":"message_start","message":%s}|} (partial "[]")

let update ?partial ?from ~seq ~block delta =
  sprintf
    {|{"event":"message_update","stream":3%s,"seq":%d%s,"delta":%s}|}
    (Option.value_map from ~default:"" ~f:(sprintf {|,"from":%d|}))
    seq
    (Option.value_map partial ~default:"" ~f:(sprintf {|,"partial":%s|}))
    (String.chop_suffix_exn delta ~suffix:"}" ^ sprintf {|,"block":%d}|} block)
;;

let text s = sprintf {|{"type":"text_delta","text":%S}|} s
let thinking s = sprintf {|{"type":"thinking_delta","text":%S}|} s

let%expect_test "deltas build the reply the whole-message updates showed" =
  let deltas =
    [ update ~seq:1 ~block:0 (thinking "Let me ")
    ; update ~seq:2 ~block:0 (thinking "think.")
    ; update ~seq:3 ~block:1 (text "Hi ")
    ; update ~from:4 ~seq:5 ~block:1 (text "**there**,\n\nsecond")
    ; update ~seq:6 ~block:1 (text " paragraph")
    ; {|{"event":"message_update","stream":3,"seq":7,"delta":{"type":"tool_call_start","index":0,"id":"c1","name":"bash","block":2}}|}
    ; {|{"event":"message_update","stream":3,"seq":8,"delta":{"type":"tool_call_delta","index":0,"arguments":"{\"command\":\"ls\"}","block":2}}|}
    ]
  in
  let whole =
    [ update
        ~seq:0
        ~block:2
        ~partial:
          (partial
             {|[{"type":"thinking","text":"Let me think."},{"type":"text","text":"Hi **there**,\n\nsecond paragraph"},{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}]|})
        (text "")
    ]
  in
  let html events =
    Chat_harness.show (Chat_harness.chat ~running:true (start :: events));
    expect_test_output ()
  in
  let from_deltas = html deltas in
  let from_whole = html whole in
  print_s [%message (String.equal from_deltas from_whole : bool)];
  print_endline from_deltas;
  [%expect {|
    ("String.equal from_deltas from_whole" true)
    <div class="entries">
      <div class="assistant msg streaming">
        <details class="thinking">
          <summary>
            <span class="label"> Thought </span>
            <span class="preview"> Let me think. </span>
          </summary>
          <div class="thinking-text">
            <div class="markdown">
              <div class="md-part">
                <p dir="auto"> Let me think. </p>
              </div>
            </div>
          </div>
        </details>
        <div class="markdown">
          <div class="md-part">
            <p dir="auto">
              Hi
              <strong> there </strong>
              ,
            </p>
          </div>
          <div class="md-part">
            <p dir="auto"> second paragraph </p>
          </div>
        </div>
        <div class="running tool tool-bash">
          <div class="tool-head">
            <span aria-hidden="true" class="lamp"> </span>
            <span class="name"> bash </span>
            <span class="arg command"> ls </span>
          </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "a gap asks the backend to resync, once" =
  let h = Harness.create () in
  Harness.event h start;
  Harness.event h (update ~seq:1 ~block:0 (text "Hello "));
  print_endline "-- seq 3 after 1";
  Harness.event h (update ~seq:3 ~block:0 (text "lost "));
  print_endline "-- seq 4: still lost, not asked again";
  Harness.event h (update ~seq:4 ~block:0 (text "more "));
  print_endline "-- the resync";
  Harness.event
    h
    (update
       ~seq:5
       ~block:0
       ~partial:(partial {|[{"type":"text","text":"Hello world again"}]|})
       (text "again"));
  Harness.event h (update ~seq:6 ~block:0 (text ", and on"));
  Harness.event h (update ~seq:6 ~block:0 (text ", and on"));
  Harness.show h ~selector:".assistant";
  [%expect {|
    -- seq 3 after 1
    (Rpc (method_ resync) (params ()) (tag Ignore))
    -- seq 4: still lost, not asked again
    -- the resync
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> Hello world again, and on </p>
        </div>
      </div>
    </div>
    |}]
;;
