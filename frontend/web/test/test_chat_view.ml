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
      <div data-entry="0" class="msg user">
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
        <div dir="auto" class="bubble"> Give me a tour of the project. </div>
      </div>
      <div data-entry="1" class="msg user">
        <div dir="auto" class="bubble"> line one
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
          <span aria-hidden="true" class="lamp"> </span>
          <span class="label"> Waiting for the model… </span>
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
            <span aria-hidden="true" class="lamp"> </span>
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
            <div class="md-part">
              <p dir="auto">
                Let me look.
                <br/>
                First the
                <strong> files </strong>
                .
              </p>
            </div>
          </div>
        </div>
      </details>
      <div class="markdown">
        <div class="md-part">
          <p dir="auto">
            Here
            <strong>
              is the
              <code> pla </code>
            </strong>
          </p>
        </div>
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
            <div class="md-part">
              <p dir="auto">
                Let me look.
                <br/>
                First the
                <strong> files </strong>
                .
              </p>
            </div>
          </div>
        </div>
      </details>
      <div class="markdown">
        <div class="md-part">
          <p dir="auto">
            Here
            <strong>
              is the
              <code> plan </code>
            </strong>
            .
          </p>
        </div>
      </div>
      <div class="meta">
        <span> claude-opus-5-5 </span>
        <span> 57.2k in </span>
        <span> 56.0k cached </span>
        <span> 345 out </span>
      </div>
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
      <div data-entry="0" class="assistant msg">
        <div class="error stop">
          <span class="title"> The model returned an error </span>
          <div class="detail"> 429 rate limited: try again in 20s </div>
          <span class="hint"> Send a message to retry, or switch model. </span>
        </div>
      </div>
      <div class="assistant msg">
        <div class="markdown">
          <div class="md-part">
            <p dir="auto"> I was saying </p>
          </div>
        </div>
        <div class="aborted stop">
          <span aria-hidden="true" class="mark stopped">
            <icon class="square"> </icon>
          </span>
          Interrupted
        </div>
      </div>
      <div class="assistant msg">
        <div class="markdown">
          <div class="md-part">
            <p dir="auto"> A long </p>
          </div>
        </div>
        <div class="length stop">
          <span aria-hidden="true" class="mark warn">
            <icon class="warning"> </icon>
          </span>
          Stopped at the output token limit: ask it to continue.
        </div>
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
    {| New year's eve 31 Dec 2025 23:59 Thu 1 Jan Happy new year m 1.2k in 40 out 1 Jan 02:01 Written before sessions kept times Sat 3 Oct Saturday 3 Oct 12:00 Yesterday Working late Today Past midnight m 1.2k in 40 out 00:30 Stopped ■ Interrupted 14:59 |}];
  Chat_harness.show ~selector:".msg.user" chat;
  [%expect
    {|
    <div data-entry="0" class="msg user">
      <div dir="auto" class="bubble"> New year's eve </div>
      <time title="Wednesday 31 December 2025, 23:59:00" datetime="2025-12-31T21:59:00Z" class="time"> 31 Dec 2025 23:59 </time>
    </div>
    <div data-entry="2" class="msg user">
      <div dir="auto" class="bubble"> Written before sessions kept times </div>
    </div>
    <div data-entry="3" class="msg user">
      <div dir="auto" class="bubble"> Saturday </div>
      <time title="Saturday 3 October 2026, 12:00:00" datetime="2026-10-03T10:00:00Z" class="time"> 3 Oct 12:00 </time>
    </div>
    |}];
  Chat_harness.show ~selector:".meta" chat;
  [%expect
    {|
    <div class="meta">
      <span> m </span>
      <span> 1.2k in </span>
      <span> 40 out </span>
      <time title="Thursday 1 January 2026, 02:01:00" datetime="2026-01-01T00:01:00Z" class="time"> 1 Jan 02:01 </time>
    </div>
    <div class="meta">
      <span> m </span>
      <span> 1.2k in </span>
      <span> 40 out </span>
      <time title="Monday 5 October 2026, 00:30:00" datetime="2026-10-04T22:30:00Z" class="time"> 00:30 </time>
    </div>
    <div class="meta">
      <time title="Monday 5 October 2026, 14:59:30" datetime="2026-10-05T12:59:30Z" class="time"> 14:59 </time>
    </div>
    |}];
  Chat_harness.show ~selector:".day-sep" chat;
  [%expect
    {|
    <div @key=day1 role="separator" class="day-sep">
      <span> Thu 1 Jan </span>
    </div>
    <div @key=day3 role="separator" class="day-sep">
      <span> Sat 3 Oct </span>
    </div>
    <div @key=day4 role="separator" class="day-sep">
      <span> Yesterday </span>
    </div>
    <div @key=day5 role="separator" class="day-sep">
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
       [ {|{"type":"event","event":"message_start","message":{"role":"user","text":"[subagent a1 finished] Count the lines in src/app.ml and report back.\nsrc/app.ml has **5 lines**.\n[subagent: 2 turns, 0 in / 0 out tokens, $0.0000]","origin":{"source":{"kind":"delivery","title":"Reports"}}}}|}
       ; {|{"type":"event","event":"message_start","message":{"role":"user","text":"[job j2 exited 0] sleep 0.5; echo second\nsecond\n\n[job j3 exited 2] make\nError: no rule\n\n[subagent a2 failed] Fix the tests\nsubagent failed: 529 overloaded\n[subagent: 1 turns, 10 in / 0 out tokens, $0.0001]","origin":{"source":{"kind":"delivery","title":"Reports"}}}}|}
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
                <div class="md-part">
                  <p dir="auto">
                    src/app.ml has
                    <strong> 5 lines </strong>
                    .
                  </p>
                </div>
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
            <div class="output">
              <pre> second </pre>
            </div>
          </div>
          <div class="bad delivery-section">
            <div class="delivery-head">
              <span class="icon"> ↩ </span>
              <span class="kind"> job j3 </span>
              <span class="bad chip"> exited 2 </span>
              <span class="task"> make </span>
            </div>
            <div class="error output">
              <pre> Error: no rule </pre>
            </div>
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
                <div class="md-part">
                  <p dir="auto"> subagent failed: 529 overloaded </p>
                </div>
              </div>
            </details>
          </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "compaction" =
  let chat =
    Chat_harness.chat
      [ {|{"event":"compacted","summary":"## Goal\nShip the **web** UI.\n\nDone so far: chat view."}|}
      ]
  in
  Chat_harness.show chat;
  [%expect
    {|
    <div class="entries">
      <details data-entry="0" class="compaction msg">
        <summary>
          <span class="label"> Context compacted </span>
          <span class="preview"> Goal </span>
        </summary>
        <div class="markdown">
          <div class="md-part">
            <h2 dir="auto"> Goal </h2>
            <p dir="auto">
              Ship the
              <strong> web </strong>
               UI.
            </p>
          </div>
          <div class="md-part">
            <p dir="auto"> Done so far: chat view. </p>
          </div>
        </div>
      </details>
    </div>
    |}]
;;

let%expect_test "nothing but whitespace renders nothing" =
  Chat_harness.show
    (Chat_harness.chat [ message_end [ text "  \n"; thinking "" ] ]);
  [%expect
    {|
    <div class="entries">
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </div>
    |}]
;;

(* What virtual_dom compares to skip an entry: the lazy value its thunk
   holds, the same object while the entry is unchanged. *)
let thunks (node : Virtual_dom.Vdom.Node.t) =
  let node =
    match node with
    | Lazy { t; _ } -> Lazy.force t
    | node -> node
  in
  let raw = Js_of_ocaml.Js.Unsafe.inject (Virtual_dom.Vdom.Node.to_raw node) in
  Js_of_ocaml.Js.to_array (Js_of_ocaml.Js.Unsafe.get raw "children")
  |> Array.map ~f:(fun child -> Js_of_ocaml.Js.Unsafe.get child "args")
;;

let%expect_test "re-rendering reuses the entries that did not change" =
  let bash =
    {|{"type":"event","event":"tool_start","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}}|}
  in
  let chat =
    Chat_harness.chat
      ~running:true
      [ {|{"event":"message_start","message":{"role":"user","text":"one"}}|}
      ; message_end [ text "first" ]
      ; bash
      ; {|{"event":"message_start","message":{"role":"user","text":"two"}}|}
      ; update [ text "str" ]
      ]
  in
  let compare before after =
    let before = thunks (Chat_view.view Chat_harness.times before)
    and after = thunks (Chat_view.view Chat_harness.times after) in
    print_s
      [%sexp
        (Array.map2_exn before after ~f:(fun a b ->
           if phys_equal a b then "same" else "rendered")
         : string array)]
  in
  print_s
    [%sexp
      (phys_equal
         (Chat_view.view Chat_harness.times chat)
         (Chat_view.view Chat_harness.times chat)
       : bool)];
  [%expect {| true |}];
  (* A streamed delta: only the streaming message. *)
  let chat' = Chat_harness.apply chat (update [ text "streaming" ]) in
  compare chat chat';
  [%expect {| (same same same same rendered) |}];
  (* A tool's output: only its card. *)
  let chat'' =
    Chat_harness.apply
      chat'
      {|{"event":"tool_output","call_id":"c1","chunk":"a.ml\n"}|}
  in
  compare chat' chat'';
  [%expect {| (same same rendered same same) |}];
  (* The run ends: the card of a call left without a result changes (it
     will never get one), the others stay. *)
  let ended =
    Chat_harness.apply chat'' {|{"event":"agent_end","messages":[]}|}
  in
  compare chat'' ended;
  [%expect {| (same same rendered same same) |}]
;;

let%expect_test "delivery of a subagent that ran out of turns" =
  Chat_harness.text
    (Chat_harness.chat
       [ {|{"type":"event","event":"message_start","message":{"role":"user","text":"[subagent a1 out of turns] Fix the parser\nsubagent hit the 1-turn limit without finishing\n[subagent: 1 turns, 20 in / 8 out tokens, $0.0000]\n[out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it]","origin":{"source":{"kind":"delivery","title":"Reports"}}}}|}
       ]);
  [%expect
    {| ↩ subagent a1 out of turns Fix the parser Report subagent hit the 1-turn limit without finishing subagent hit the 1-turn limit without finishing [subagent: 1 turns, 20 in / 8 out tokens, $0.0000] [out of turns: call subagent_continue with id a1 (optionally max_turns, default 1, and a message) to give it more turns: it carries on from where it stopped; or subagent_cancel it, or leave it] |}]
;;
