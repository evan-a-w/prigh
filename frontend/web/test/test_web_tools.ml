open! Core
open Prigh_web
module Json = Prigh_protocol.Json

let results =
  "1. Eio docs — https://ocaml.org/p/eio\n\
   2. Eio on GitHub — https://github.com/ocaml-multicore/eio\n\
   3. Effects tutorial — https://example.com/effects\n"
;;

let server_tool
      ?(name = "web_search")
      ?(input = {|{"query":"ocaml eio"}|})
      ?output
      ?(is_error = false)
      id
  =
  Json.obj
    ([ "type", Json.str "server_tool"
     ; "id", Json.str id
     ; "name", Json.str name
     ; "input", Json.str input
     ]
     @
     match output with
     | None -> []
     | Some output ->
       [ "output", Json.str output; "is_error", Json.bool is_error ])
;;

let text t = Json.obj [ "type", Json.str "text"; "text", Json.str t ]

let assistant ?(stop = "end_turn") content =
  Json.obj
    [ "role", Json.str "assistant"
    ; "content", `Array content
    ; "stop_reason", Json.obj [ "type", Json.str stop ]
    ; ( "usage"
      , Json.obj
          [ "input", Json.int 0
          ; "output", Json.int 0
          ; "cache_read", Json.int 0
          ] )
    ; "model", Json.str "m"
    ]
;;

let update content delta =
  Json.to_string
    (Json.obj
       [ "event", Json.str "message_update"
       ; "partial", assistant content
       ; "delta", delta
       ])
;;

let message_end ?stop content =
  Json.to_string
    (Json.obj
       [ "event", Json.str "message_end"; "message", assistant ?stop content ])
;;

let text_delta t =
  Json.obj [ "type", Json.str "text_delta"; "text", Json.str t ]
;;

let show_message chat = Chat_harness.show ~selector:".msg.assistant" chat

let%expect_test "web search streamed between text: running, then its results" =
  let use =
    Json.obj
      [ "type", Json.str "server_tool_use"
      ; "id", Json.str "s1"
      ; "name", Json.str "web_search"
      ; "input", Json.str {|{"query":"ocaml eio"}|}
      ]
  in
  let result =
    Json.obj
      [ "type", Json.str "server_tool_result"
      ; "id", Json.str "s1"
      ; "output", Json.str results
      ; "is_error", `False
      ]
  in
  let chat =
    Chat_harness.chat
      ~running:true
      [ update
          [ text "Let me look that up." ]
          (text_delta "Let me look that up.")
      ; update [ text "Let me look that up."; server_tool "s1" ] use
      ]
  in
  show_message chat;
  [%expect
    {|
    <div class="assistant msg streaming">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> Let me look that up. </p>
        </div>
      </div>
      <div class="running tool tool-web_search">
        <div class="tool-head">
          <span aria-hidden="true" class="lamp"> </span>
          <span class="name"> web_search </span>
          <span class="arg query"> "ocaml eio" </span>
        </div>
      </div>
    </div>
    |}];
  let done_ =
    [ text "Let me look that up."; server_tool ~output:results "s1" ]
  in
  let chat =
    List.fold
      ~init:chat
      ~f:Chat_harness.apply
      [ update done_ result
      ; update (done_ @ [ text "Eio is" ]) (text_delta "Eio is")
      ; message_end (done_ @ [ text "Eio is an effects-based IO library." ])
      ]
  in
  show_message chat;
  [%expect
    {|
    <div class="assistant msg">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> Let me look that up. </p>
        </div>
      </div>
      <div class="ok tool tool-web_search">
        <div class="tool-head">
          <span title="Done" role="img" aria-label="Done" class="done mark">
            <icon class="check"> </icon>
          </span>
          <span class="name"> web_search </span>
          <span class="arg query"> "ocaml eio" </span>
          <span class="chip"> 3 results </span>
        </div>
        <div class="tool-body">
          <details class="results">
            <summary>
              <span class="label"> 3 results </span>
              <span class="preview"> Eio docs   Eio on GitHub   Effects tutorial </span>
            </summary>
            <ol class="results">
              <li>
                <a href="https://ocaml.org/p/eio"
                   target="_blank"
                   rel="noopener noreferrer"
                   class="title"> Eio docs </a>
                <span class="url"> https://ocaml.org/p/eio </span>
              </li>
              <li>
                <a href="https://github.com/ocaml-multicore/eio"
                   target="_blank"
                   rel="noopener noreferrer"
                   class="title"> Eio on GitHub </a>
                <span class="url"> https://github.com/ocaml-multicore/eio </span>
              </li>
              <li>
                <a href="https://example.com/effects"
                   target="_blank"
                   rel="noopener noreferrer"
                   class="title"> Effects tutorial </a>
                <span class="url"> https://example.com/effects </span>
              </li>
            </ol>
          </details>
        </div>
      </div>
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> Eio is an effects-based IO library. </p>
        </div>
      </div>
    </div>
    |}]
;;

let loaded content =
  Chat.of_messages
    [ Or_error.ok_exn (Prigh_protocol.Message.of_json (assistant content)) ]
;;

let%expect_test
    "loaded searches: no sources, failed, interrupted, unknown tools"
  =
  let chat =
    loaded
      [ server_tool ~output:"searched" "s1"
      ; server_tool
          ~input:{|{"query":"rate me"}|}
          ~output:"web search failed: too many requests"
          ~is_error:true
          "s2"
      ; server_tool ~input:{|{"query":"never answered"}|} "s3"
      ; server_tool
          ~name:"code_execution"
          ~input:{|{"code":"print(1)"}|}
          ~output:"1"
          "s4"
      ]
  in
  Chat_harness.show ~selector:".tool" chat;
  [%expect
    {|
    <div class="ok tool tool-web_search">
      <div class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> web_search </span>
        <span class="arg query"> "ocaml eio" </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> searched </pre>
        </div>
      </div>
    </div>
    <div class="error tool tool-web_search">
      <div class="tool-head">
        <span title="Failed" role="img" aria-label="Failed" class="failed mark">
          <icon class="cross"> </icon>
        </span>
        <span class="name"> web_search </span>
        <span class="arg query"> "rate me" </span>
      </div>
      <div class="tool-body">
        <div class="error output">
          <pre> web search failed: too many requests </pre>
        </div>
      </div>
    </div>
    <div class="interrupted tool tool-web_search">
      <div class="tool-head">
        <span title="Interrupted" role="img" aria-label="Interrupted" class="mark stopped">
          <icon class="square"> </icon>
        </span>
        <span class="name"> web_search </span>
        <span class="arg query"> "never answered" </span>
        <span class="chip"> no result </span>
      </div>
    </div>
    <div class="ok tool tool-code_execution">
      <div class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> code_execution </span>
        <span class="arg"> {"code":"print(1)"} </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> 1 </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "/fork and /tree labels and a subagent's latest step name it" =
  let message =
    Or_error.ok_exn
      (Prigh_protocol.Message.of_json (assistant [ server_tool "s1" ]))
  in
  print_endline (Session_tree.first_line message);
  [%expect {| web_search "ocaml eio" |}];
  print_s
    [%sexp
      (Tool_view.last_activity (loaded [ server_tool "s1" ]) : string option)];
  [%expect {| ("web_search \"ocaml eio\"") |}]
;;

let tool_end ?(id = "c1") ~name ~arguments ?(is_error = false) text =
  Json.to_string
    (Json.obj
       [ "event", Json.str "tool_end"
       ; ( "call"
         , Json.obj
             [ "id", Json.str id
             ; "name", Json.str name
             ; "arguments", Json.str arguments
             ] )
       ; ( "result"
         , Json.obj
             [ "role", Json.str "tool_result"
             ; "tool_call_id", Json.str id
             ; "tool_name", Json.str name
             ; "text", Json.str text
             ; "is_error", Json.bool is_error
             ] )
       ])
;;

let tool_start ~name ~arguments =
  Json.to_string
    (Json.obj
       [ "event", Json.str "tool_start"
       ; ( "call"
         , Json.obj
             [ "id", Json.str "c1"
             ; "name", Json.str name
             ; "arguments", Json.str arguments
             ] )
       ])
;;

let%expect_test "web_fetch: the URL as a link, the page folded" =
  let arguments = {|{"url":"https://ocaml.org/p/eio"}|} in
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.chat
       ~running:true
       [ tool_start ~name:"web_fetch" ~arguments ]);
  [%expect
    {|
    <div class="running tool tool-web_fetch">
      <div class="tool-head">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="name"> web_fetch </span>
        <a href="https://ocaml.org/p/eio" target="_blank" rel="noopener noreferrer" class="arg url"> https://ocaml.org/p/eio </a>
      </div>
    </div>
    |}];
  let page =
    "https://ocaml.org/p/eio/latest (HTTP 200, text/html, 12.3 KB)\n\n\
     # Eio\n\n\
     Effects-based direct-style IO.\n\
     line 4\n\
     line 5\n\
     line 6\n\
     line 7\n\
     line 8\n\
     line 9\n\
     line 10\n"
  in
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.chat [ tool_end ~name:"web_fetch" ~arguments page ]);
  [%expect
    {|
    <div class="ok tool tool-web_fetch">
      <div class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> web_fetch </span>
        <a href="https://ocaml.org/p/eio" target="_blank" rel="noopener noreferrer" class="arg url"> https://ocaml.org/p/eio </a>
        <span class="chip"> → https://ocaml.org/p/eio/latest </span>
        <span class="chip"> HTTP 200, text/html, 12.3 KB </span>
      </div>
      <div class="tool-body">
        <div class="clipped from-start output">
          <pre> # Eio

    Effects-based direct-style IO.
    line 4
    line 5
    line 6 </pre>
          <button type="button" data-fold="c1" aria-expanded="false" class="fold">
            <span class="chev"> </span>
            <span>
              Show all
              <span class="count"> 10 </span>
               lines
            </span>
          </button>
        </div>
      </div>
    </div>
    |}];
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.chat
       [ tool_end
           ~name:"web_fetch"
           ~arguments
           ~is_error:true
           "fetch failed: HTTP 404"
       ]);
  [%expect
    {|
    <div class="error tool tool-web_fetch">
      <div class="tool-head">
        <span title="Failed" role="img" aria-label="Failed" class="failed mark">
          <icon class="cross"> </icon>
        </span>
        <span class="name"> web_fetch </span>
        <a href="https://ocaml.org/p/eio" target="_blank" rel="noopener noreferrer" class="arg url"> https://ocaml.org/p/eio </a>
      </div>
      <div class="tool-body">
        <div class="error output">
          <pre> fetch failed: HTTP 404 </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "job_input: the job and what it was sent, escaped" =
  let arguments = {|{"id":"j1","input":"print(1)\n","close":true}|} in
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.chat
       ~running:true
       [ tool_start ~name:"job_input" ~arguments ]);
  [%expect
    {|
    <div class="running tool tool-job_input">
      <div class="tool-head">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="name"> job_input </span>
        <span class="arg input"> j1 "print(1)\n" </span>
        <span class="chip"> close stdin </span>
      </div>
    </div>
    |}];
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.chat
       [ tool_end ~name:"job_input" ~arguments "sent 9 bytes to job j1\n1" ]);
  [%expect
    {|
    <div class="ok tool tool-job_input">
      <div class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> job_input </span>
        <span class="arg input"> j1 "print(1)\n" </span>
        <span class="chip"> close stdin </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> sent 9 bytes to job j1
    1 </pre>
        </div>
      </div>
    </div>
    |}];
  (* Long input is cut; closing alone shows only the job. *)
  Chat_harness.text
    ~selector:".tool-head"
    (Chat_harness.chat
       [ tool_end
           ~name:"job_input"
           ~arguments:
             {|{"id":"j2","input":"let x = \"a long line of text\" in\n\tprint x; more and more"}|}
           "sent"
       ; tool_end
           ~id:"c2"
           ~name:"job_input"
           ~arguments:{|{"id":"j3","input":"","close":true}|}
           "closed"
       ]);
  [%expect
    {|
    ✓ job_input j2 "let x = \"a long line of text\" in\n\tp…"
    ✓ job_input j3 close stdin
    |}]
;;

let%expect_test "worktree: the action and its branch or path" =
  Chat_harness.text
    ~selector:".tool-head"
    (Chat_harness.chat
       [ tool_end
           ~name:"worktree"
           ~arguments:{|{"action":"create","branch":"feat/x","base":"main"}|}
           "made worktree /w/repo/feat-x"
       ; tool_end
           ~id:"c2"
           ~name:"worktree"
           ~arguments:
             {|{"action":"remove","path":"/w/repo/feat-x","force":true}|}
           "removed worktree /w/repo/feat-x"
       ; tool_end
           ~id:"c3"
           ~name:"worktree"
           ~arguments:{|{"action":"list"}|}
           "worktrees of /repo"
       ]);
  [%expect
    {|
    ✓ worktree create feat/x from main
    ✓ worktree remove /w/repo/feat-x (force)
    ✓ worktree list
    |}]
;;
