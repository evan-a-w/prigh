open! Core
open Prigh_web

let assistant
      ?(stop = {|{"type":"end_turn"}|})
      ?(usage = {|{"input":0,"output":0,"cache_read":0}|})
      content
  =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":%s,"usage":%s,"model":"claude-opus-5-5"}|}
    (String.concat ~sep:"," content)
    stop
    usage
;;

let update ?stop content =
  sprintf
    {|{"event":"message_update","partial":%s,"delta":{"type":"text_delta","text":""}}|}
    (assistant ?stop content)
;;

let message_end ?stop ?usage content =
  sprintf
    {|{"event":"message_end","message":%s}|}
    (assistant ?stop ?usage content)
;;

let text s = sprintf {|{"type":"text","text":%S}|} s
let thinking s = sprintf {|{"type":"thinking","text":%S}|} s

let%expect_test "user messages: text, pasted images" =
  Chat_harness.show
    (Chat_harness.chat
       [ {|{"type":"event","event":"message_start","message":{"role":"user","text":"Give me a tour of the project.","images":[{"mime_type":"image/png","data":"iVBORw0KGgo="}]}}|}
       ; {|{"event":"message_start","message":{"role":"user","text":"line one\nline two"}}|}
       ]);
  [%expect
    {|
    <div class="entries">
      <div class="msg user">
        <div class="images">
          <details class="image">
            <summary title="[image: image/png, 8 B]">
              <img src="data:image/png;base64,iVBORw0KGgo="
                   alt="[image: image/png, 8 B]"
                   class="thumb"/>
              <span class="lightbox">
                <img src="data:image/png;base64,iVBORw0KGgo=" alt="[image: image/png, 8 B]"/>
              </span>
            </summary>
          </details>
        </div>
        <div class="bubble"> Give me a tour of the project. </div>
      </div>
      <div class="msg user">
        <div class="bubble"> line one
    line two </div>
      </div>
    </div>
    |}]
;;

let%expect_test
    "a reply as it streams: waiting, thinking live, text with open spans"
  =
  let chat = Chat_harness.chat ~running:true [ update [] ] in
  Chat_harness.show chat;
  [%expect
    {|
    <div class="entries">
      <div class="assistant msg streaming">
        <div class="pending">
          <span> </span>
          <span> </span>
          <span> </span>
        </div>
      </div>
    </div>
    |}];
  let chat =
    Chat_harness.apply
      chat
      (update [ thinking "Let me look.\nFirst the **files**." ])
  in
  Chat_harness.show chat;
  [%expect
    {|
    <div class="entries">
      <div class="assistant msg streaming">
        <div class="live thinking">
          <div class="thinking-head">
            <span class="spinner"> </span>
            <span class="label"> Thinking… </span>
          </div>
          <div class="thinking-text"> Let me look.
    First the **files**. </div>
        </div>
      </div>
    </div>
    |}];
  let chat =
    Chat_harness.apply
      chat
      (update
         [ thinking "Let me look.\nFirst the **files**."
         ; text "Here **is the `pla"
         ])
  in
  Chat_harness.show ~selector:".msg.assistant" chat;
  [%expect
    {|
    <div class="assistant msg streaming">
      <details class="thinking">
        <summary>
          <span class="label"> Thought </span>
          <span class="preview"> Let me look. </span>
        </summary>
        <div class="thinking-text">
          <div class="markdown">
            <p>
              Let me look.
              <br/>
              First the
              <strong> files </strong>
              .
            </p>
          </div>
        </div>
      </details>
      <div class="markdown">
        <p>
          Here
          <strong>
            is the
            <code> pla </code>
          </strong>
        </p>
      </div>
    </div>
    |}];
  Chat_harness.show
    ~selector:".msg.assistant"
    (Chat_harness.apply
       chat
       (message_end
          ~usage:{|{"input":1200,"output":345,"cache_read":56000}|}
          [ thinking "Let me look.\nFirst the **files**."
          ; text "Here **is the `plan`**."
          ]));
  [%expect
    {|
    <div class="assistant msg">
      <details class="thinking">
        <summary>
          <span class="label"> Thought </span>
          <span class="preview"> Let me look. </span>
        </summary>
        <div class="thinking-text">
          <div class="markdown">
            <p>
              Let me look.
              <br/>
              First the
              <strong> files </strong>
              .
            </p>
          </div>
        </div>
      </details>
      <div class="markdown">
        <p>
          Here
          <strong>
            is the
            <code> plan </code>
          </strong>
          .
        </p>
      </div>
      <div class="meta"> claude-opus-5-5 · 57.2k in · 56.0k cached · 345 out </div>
    </div>
    |}]
;;

let%expect_test "how a reply stopped: error, interrupted, length" =
  Chat_harness.show
    (Chat_harness.chat
       [ message_end
           ~stop:
             {|{"type":"error","message":"429 rate limited: try again in 20s"}|}
           []
       ; message_end ~stop:{|{"type":"aborted"}|} [ text "I was saying" ]
       ; message_end ~stop:{|{"type":"length"}|} [ text "A long" ]
       ]);
  [%expect
    {|
    <div class="entries">
      <div class="assistant msg">
        <div class="error stop">
          <span class="title"> The model returned an error </span>
          <div class="detail"> 429 rate limited: try again in 20s </div>
          <span class="hint"> Send a message to retry, or switch model. </span>
        </div>
      </div>
      <div class="assistant msg">
        <div class="markdown">
          <p> I was saying </p>
        </div>
        <div class="aborted stop"> Interrupted </div>
      </div>
      <div class="assistant msg">
        <div class="markdown">
          <p> A long </p>
        </div>
        <div class="length stop"> Stopped at the output token limit — ask it to continue. </div>
      </div>
    </div>
    |}]
;;

let%expect_test
    "times: under user messages and in replies' footers; days marked where \
     they change"
  =
  let user ?at text =
    sprintf
      {|{"event":"message_start","message":{"role":"user","text":%S%s}}|}
      text
      (Option.value_map at ~default:"" ~f:(sprintf {|,"at":%s|}))
  in
  let reply ?(stop = {|{"type":"end_turn"}|}) ~at content =
    sprintf
      {|{"event":"message_end","message":{"role":"assistant","content":[%s],"stop_reason":%s,"usage":{"input":1200,"output":40,"cache_read":0},"model":"m","at":%s}}|}
      (String.concat ~sep:"," content)
      stop
      at
  in
  (* Read on Monday 5 October 2026 at 15:00, two hours ahead of UTC. Times
     are strings: under js_of_ocaml an [int] has 32 bits. *)
  let chat =
    Chat_harness.chat
      [ user ~at:"1767218340000" "New year's eve" (* 2025-12-31 21:59Z *)
      ; reply ~at:"1767225660000" [ text "Happy new year" ]
        (* 2026-01-01 00:01Z *)
      ; user "Written before sessions kept times"
      ; user ~at:"1791021600000" "Saturday" (* 2026-10-03 10:00Z *)
      ; reply
          ~stop:{|{"type":"tool_use"}|}
          ~at:"1791147600000" (* 2026-10-04 21:00Z: 23:00 local *)
          [ text "Working late" ]
      ; reply ~at:"1791153000000" [ text "Past midnight" ] (* 00:30 local *)
      ; reply
          ~stop:{|{"type":"aborted"}|}
          ~at:"1791205170000" (* 14:59:30 local *)
          [ text "Stopped" ]
      ]
  in
  Chat_harness.text chat;
  [%expect
    {| New year's eve 31 Dec 2025 23:59 Thu 1 Jan Happy new year m · 1.2k in · 40 out · 1 Jan 02:01 Written before sessions kept times Sat 3 Oct Saturday 3 Oct 12:00 Yesterday Working late Today Past midnight m · 1.2k in · 40 out · 00:30 Stopped Interrupted 14:59 |}];
  Chat_harness.show ~selector:".msg.user" chat;
  [%expect
    {|
    <div class="msg user">
      <div class="bubble"> New year's eve </div>
      <time title="Wednesday 31 December 2025, 23:59:00" datetime="2025-12-31T21:59:00Z" class="time"> 31 Dec 2025 23:59 </time>
    </div>
    <div class="msg user">
      <div class="bubble"> Written before sessions kept times </div>
    </div>
    <div class="msg user">
      <div class="bubble"> Saturday </div>
      <time title="Saturday 3 October 2026, 12:00:00" datetime="2026-10-03T10:00:00Z" class="time"> 3 Oct 12:00 </time>
    </div>
    |}];
  Chat_harness.show ~selector:".meta" chat;
  [%expect
    {|
    <div class="meta">
      m · 1.2k in · 40 out
       ·
      <time title="Thursday 1 January 2026, 02:01:00" datetime="2026-01-01T00:01:00Z" class="time"> 1 Jan 02:01 </time>
    </div>
    <div class="meta">
      m · 1.2k in · 40 out
       ·
      <time title="Monday 5 October 2026, 00:30:00" datetime="2026-10-04T22:30:00Z" class="time"> 00:30 </time>
    </div>
    <div class="meta">
      <time title="Monday 5 October 2026, 14:59:30" datetime="2026-10-05T12:59:30Z" class="time"> 14:59 </time>
    </div>
    |}];
  Chat_harness.show ~selector:".day-sep" chat;
  [%expect
    {|
    <div role="separator" class="day-sep">
      <span> Thu 1 Jan </span>
    </div>
    <div role="separator" class="day-sep">
      <span> Sat 3 Oct </span>
    </div>
    <div role="separator" class="day-sep">
      <span> Yesterday </span>
    </div>
    <div role="separator" class="day-sep">
      <span> Today </span>
    </div>
    |}]
;;

let%expect_test
    "live messages without a time get the one the page received them at"
  =
  let at = Time_ns.of_string_with_utc_offset "2026-10-05 12:59:00Z" in
  List.iter
    [ {|{"event":"message_start","message":{"role":"user","text":"no time"}}|}
    ; {|{"event":"message_end","message":{"role":"user","text":"sent","at":1791194280000}}|}
    ; {|{"event":"subagent","call_id":"c1","agent_id":"a1","inner":{"event":"message_end","message":{"role":"tool_result","tool_call_id":"c2","tool_name":"ls","text":"","is_error":false}}}|}
    ; {|{"event":"tool_output","call_id":"c1","chunk":"x"}|}
    ]
    ~f:(fun json ->
      let event =
        Prigh_protocol.Event.of_json (Jsonaf.of_string json) |> Or_error.ok_exn
      in
      print_s [%sexp (Chat.received event ~at : Prigh_protocol.Event.t)]);
  [%expect
    {|
    (Message_start (User ((text "no time") (at "2026-10-05 12:59:00Z"))))
    (Message_end (User ((text sent) (at "2026-10-05 09:58:00Z"))))
    (Subagent (call_id c1) (agent_id a1)
     (event
      (Message_end
       (Tool_result
        ((tool_call_id c2) (tool_name ls) (text "") (is_error false)
         (at "2026-10-05 12:59:00Z"))))))
    (Tool_output (call_id c1) (chunk x))
    |}]
;;

let%expect_test "deliveries of background work: compact cards" =
  Chat_harness.show
    (Chat_harness.chat
       [ {|{"type":"event","event":"message_start","message":{"role":"user","text":"[subagent a1 finished] Count the lines in src/app.ml and report back.\nsrc/app.ml has **5 lines**.\n[subagent: 2 turns, 0 in / 0 out tokens, $0.0000]"}}|}
       ; {|{"type":"event","event":"message_start","message":{"role":"user","text":"[job j2 exited 0] sleep 0.5; echo second\nsecond\n\n[job j3 exited 2] make\nError: no rule\n\n[subagent a2 failed] Fix the tests\nsubagent failed: 529 overloaded\n[subagent: 1 turns, 10 in / 0 out tokens, $0.0001]"}}|}
       ]);
  [%expect
    {|
    <div class="entries">
      <div class="msg">
        <div class="delivery">
          <div class="delivery-section ok">
            <div class="delivery-head">
              <span class="icon"> ↩ </span>
              <span class="kind"> subagent a1 </span>
              <span class="chip ok"> finished </span>
              <span class="task"> Count the lines in src/app.ml and report back. </span>
              <span class="stats"> 2 turns, 0 in / 0 out tokens, $0.0000 </span>
            </div>
            <details class="delivery-body">
              <summary>
                <span class="label"> Report </span>
                <span class="preview"> src/app.ml has 5 lines. </span>
              </summary>
              <div class="markdown">
                <p>
                  src/app.ml has
                  <strong> 5 lines </strong>
                  .
                </p>
              </div>
            </details>
          </div>
        </div>
      </div>
      <div class="msg">
        <div class="delivery">
          <div class="delivery-section ok">
            <div class="delivery-head">
              <span class="icon"> ↩ </span>
              <span class="kind"> job j2 </span>
              <span class="chip ok"> exited 0 </span>
              <span class="task"> sleep 0.5; echo second </span>
            </div>
            <details class="delivery-body">
              <summary>
                <span class="label"> Output </span>
                <span class="preview"> second </span>
              </summary>
              <div class="output">
                <pre> second </pre>
              </div>
            </details>
          </div>
          <div class="bad delivery-section">
            <div class="delivery-head">
              <span class="icon"> ↩ </span>
              <span class="kind"> job j3 </span>
              <span class="bad chip"> exited 2 </span>
              <span class="task"> make </span>
            </div>
            <details class="delivery-body">
              <summary>
                <span class="label"> Output </span>
                <span class="preview"> Error: no rule </span>
              </summary>
              <div class="error output">
                <pre> Error: no rule </pre>
              </div>
            </details>
          </div>
          <div class="bad delivery-section">
            <div class="delivery-head">
              <span class="icon"> ↩ </span>
              <span class="kind"> subagent a2 </span>
              <span class="bad chip"> failed </span>
              <span class="task"> Fix the tests </span>
              <span class="stats"> 1 turns, 10 in / 0 out tokens, $0.0001 </span>
            </div>
            <details class="delivery-body">
              <summary>
                <span class="label"> Report </span>
                <span class="preview"> subagent failed: 529 overloaded </span>
              </summary>
              <div class="markdown">
                <p> subagent failed: 529 overloaded </p>
              </div>
            </details>
          </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "notices and compaction" =
  let chat =
    Chat_harness.chat
      [ {|{"event":"compacted","summary":"## Goal\nShip the **web** UI.\n\nDone so far: chat view."}|}
      ]
  in
  Chat_harness.show (Chat.add_notice chat "Switched to Claude Opus 5.5");
  [%expect
    {|
    <div class="entries">
      <details class="compaction msg">
        <summary>
          <span class="label"> Context compacted </span>
          <span class="preview"> Goal </span>
        </summary>
        <div class="markdown">
          <h2> Goal </h2>
          <p>
            Ship the
            <strong> web </strong>
             UI.
          </p>
          <p> Done so far: chat view. </p>
        </div>
      </details>
      <div class="msg notice"> Switched to Claude Opus 5.5 </div>
    </div>
    |}]
;;

let%expect_test "nothing but whitespace renders nothing" =
  Chat_harness.show
    (Chat_harness.chat [ message_end [ text "  \n"; thinking "" ] ]);
  [%expect {| <div class="entries"> </div> |}]
;;
