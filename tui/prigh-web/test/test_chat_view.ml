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
    let before = thunks (Chat_view.view before)
    and after = thunks (Chat_view.view after) in
    print_s
      [%sexp
        (Array.map2_exn before after ~f:(fun a b ->
           if phys_equal a b then "same" else "rendered")
         : string array)]
  in
  print_s
    [%sexp (phys_equal (Chat_view.view chat) (Chat_view.view chat) : bool)];
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
