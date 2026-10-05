open! Core
open Prigh_web

let show ?running events =
  Chat_harness.show ~selector:".tool" (Chat_harness.chat ?running events)
;;

let%expect_test "bash: arguments streaming, then running with live output" =
  let chat =
    Chat_harness.chat
      ~running:true
      [ {|{"event":"message_update","partial":{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls && ec"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"tool_call_delta","index":0,"arguments":"ls && ec"}}|}
      ]
  in
  Chat_harness.show ~selector:".tool" chat;
  [%expect
    {|
    <div class="running tool tool-bash">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> bash </span>
        <span class="arg command"> ls && ec </span>
      </div>
    </div>
    |}];
  let chat =
    List.fold
      ~init:chat
      ~f:Chat_harness.apply
      [ {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls && echo done\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}}|}
      ; {|{"type":"event","event":"tool_start","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"ls && echo done\"}"}}|}
      ; {|{"type":"event","event":"tool_output","call_id":"c1","chunk":"app.ml.orig\nhome\nnotes.md\npic.png\npw\nscript.json\nscript-stream.json\nserver.log\nserve.sh\nshoot.sh\nshots\nsrc\n"}|}
      ]
  in
  Chat_harness.show ~selector:".tool" chat;
  [%expect
    {|
    <div class="running tool tool-bash">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> bash </span>
        <span class="arg command"> ls && echo done </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <details class="more">
            <summary> 4 more lines </summary>
            <pre> app.ml.orig
    home
    notes.md
    pic.png </pre>
          </details>
          <pre> pw
    script.json
    script-stream.json
    server.log
    serve.sh
    shoot.sh
    shots
    src </pre>
        </div>
      </div>
    </div>
    |}];
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.apply
       chat
       {|{"type":"event","event":"tool_end","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"ls && echo done\"}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"app.ml.orig\nhome\nnotes.md\npic.png\npw\nscript.json\nscript-stream.json\nserver.log\nserve.sh\nshoot.sh\nshots\nsrc\ndone\n","is_error":false}}|});
  [%expect
    {|
    <div class="ok tool tool-bash">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> bash </span>
        <span class="arg command"> ls && echo done </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre>
            app.ml.orig
    home
    notes.md
    pic.png
    pw
    script.json
    script-stream.json
    server.log
    serve.sh
    shoot.sh
    shots
    src
    done
          </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test
    "bash: a failure shows how it ended; multi-line commands in full"
  =
  show
    [ {|{"type":"event","event":"tool_end","call":{"id":"c9","name":"bash","arguments":"{\"command\":\"cat missing.txt\"}"},"result":{"role":"tool_result","tool_call_id":"c9","tool_name":"bash","text":"cat: missing.txt: No such file or directory\n[exit code 1]","is_error":true}}|}
    ; {|{"type":"event","event":"tool_end","call":{"id":"c2","name":"bash","arguments":"{\"command\":\"cd src\nmake\",\"timeout\":5}"},"result":{"role":"tool_result","tool_call_id":"c2","tool_name":"bash","text":"building\n[timed out after 5s]","is_error":true}}|}
    ];
  [%expect
    {|
    <div class="error tool tool-bash">
      <div class="tool-head">
        <span class="icon"> ✕ </span>
        <span class="name"> bash </span>
        <span class="arg command"> cat missing.txt </span>
        <span class="bad chip"> exit code 1 </span>
      </div>
      <div class="tool-body">
        <div class="error output">
          <pre> cat: missing.txt: No such file or directory </pre>
        </div>
      </div>
    </div>
    <div class="error tool tool-bash">
      <div class="tool-head">
        <span class="icon"> ✕ </span>
        <span class="name"> bash </span>
        <span class="arg command"> cd src … </span>
        <span class="bad chip"> timed out after 5s </span>
      </div>
      <div class="tool-body">
        <pre class="command"> cd src
    make </pre>
        <div class="error output">
          <pre> building </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "bash in the background: the job it started" =
  show
    ~running:true
    [ {|{"type":"event","event":"tool_start","call":{"id":"c10","name":"bash","arguments":"{\"command\":\"sleep 1; echo bg finished\",\"background\":true}"}}|}
    ];
  [%expect
    {|
    <div class="running tool tool-bash">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> bash </span>
        <span class="arg command"> sleep 1; echo bg finished </span>
        <span class="chip"> background </span>
      </div>
    </div>
    |}];
  show
    [ {|{"type":"event","event":"tool_end","call":{"id":"c10","name":"bash","arguments":"{\"command\":\"sleep 1; echo bg finished\",\"background\":true}"},"result":{"role":"tool_result","tool_call_id":"c10","tool_name":"bash","text":"started job j1: sleep 1; echo bg finished; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_wait to block, job_kill to stop it.","is_error":false}}|}
    ];
  [%expect
    {|
    <div class="ok tool tool-bash">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> bash </span>
        <span class="arg command"> sleep 1; echo bg finished </span>
        <span class="chip job"> job j1 </span>
      </div>
    </div>
    |}]
;;

let%expect_test "long output: the middle folds behind \"N more lines\"" =
  let text = List.init 30 ~f:(sprintf "line %d") |> String.concat ~sep:"\\n" in
  show
    [ sprintf
        {|{"event":"tool_end","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"seq\"}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"%s","is_error":false}}|}
        text
    ];
  [%expect
    {|
    <div class="ok tool tool-bash">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> bash </span>
        <span class="arg command"> seq </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> line 0
    line 1
    line 2
    line 3 </pre>
          <details class="more">
            <summary> 18 more lines </summary>
            <pre>
              line 4
    line 5
    line 6
    line 7
    line 8
    line 9
    line 10
    line 11
    line 12
    line 13
    line 14
    line 15
    line 16
    line 17
    line 18
    line 19
    line 20
    line 21
            </pre>
          </details>
          <pre> line 22
    line 23
    line 24
    line 25
    line 26
    line 27
    line 28
    line 29 </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "read: text folded, images as thumbnails, ranges, errors" =
  show
    [ {|{"type":"event","event":"tool_end","call":{"id":"c2","name":"read","arguments":"{\"path\":\"notes.md\"}"},"result":{"role":"tool_result","tool_call_id":"c2","tool_name":"read","text":"# Notes\nSome notes about the project.\n","is_error":false}}|}
    ; {|{"type":"event","event":"tool_end","call":{"id":"c3","name":"read","arguments":"{\"path\":\"pic.png\"}"},"result":{"role":"tool_result","tool_call_id":"c3","tool_name":"read","text":"Read image file [image/png, 320x200]","is_error":false,"images":[{"mime_type":"image/png","data":"iVBORw0KGgo="}]}}|}
    ; {|{"event":"tool_end","call":{"id":"c4","name":"read","arguments":"{\"path\":\"big.ml\",\"offset\":10,\"limit\":20}"},"result":{"role":"tool_result","tool_call_id":"c4","tool_name":"read","text":"x","is_error":false}}|}
    ; {|{"event":"tool_end","call":{"id":"c5","name":"read","arguments":"{\"path\":\"nope.txt\"}"},"result":{"role":"tool_result","tool_call_id":"c5","tool_name":"read","text":"file not found: nope.txt","is_error":true}}|}
    ];
  [%expect
    {|
    <div class="ok tool tool-read">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> read </span>
        <span class="arg path"> notes.md </span>
      </div>
      <div class="tool-body">
        <details class="file">
          <summary> 2 lines </summary>
          <pre> # Notes
    Some notes about the project.
     </pre>
        </details>
      </div>
    </div>
    <div class="ok tool tool-read">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> read </span>
        <span class="arg path"> pic.png </span>
      </div>
      <div class="tool-body">
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
        <span class="caption"> Read image file [image/png, 320x200] </span>
      </div>
    </div>
    <div class="ok tool tool-read">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> read </span>
        <span class="arg path"> big.ml </span>
        <span class="chip"> lines 10–29 </span>
      </div>
      <div class="tool-body">
        <details class="file">
          <summary> 1 line </summary>
          <pre> x </pre>
        </details>
      </div>
    </div>
    <div class="error tool tool-read">
      <div class="tool-head">
        <span class="icon"> ✕ </span>
        <span class="name"> read </span>
        <span class="arg path"> nope.txt </span>
      </div>
      <div class="tool-body">
        <div class="error output">
          <pre> file not found: nope.txt </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "write: the path, a preview, whether it replaced a file" =
  show
    [ {|{"type":"event","event":"tool_end","call":{"id":"c4","name":"write","arguments":"{\"path\":\"hello.py\",\"content\":\"import sys\\n\\nprint('hello', sys.argv[1:])\\n\"}"},"result":{"role":"tool_result","tool_call_id":"c4","tool_name":"write","text":"wrote 3 lines to /tmp/chatT/hello.py","is_error":false}}|}
    ; {|{"event":"tool_end","call":{"id":"c5","name":"write","arguments":"{\"path\":\"a.txt\",\"content\":\"x\"}"},"result":{"role":"tool_result","tool_call_id":"c5","tool_name":"write","text":"overwrote 1 line to /w/a.txt","is_error":false}}|}
    ];
  [%expect
    {|
    <div class="ok tool tool-write">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> write </span>
        <span class="arg path"> hello.py </span>
        <span class="chip"> 3 lines </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> import sys

    print('hello', sys.argv[1:]) </pre>
        </div>
      </div>
    </div>
    <div class="ok tool tool-write">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> write </span>
        <span class="arg path"> a.txt </span>
        <span class="chip"> 1 line </span>
        <span class="chip"> overwrote </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> x </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "write: content shown as it streams" =
  show
    ~running:true
    [ {|{"event":"message_update","partial":{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"write","arguments":"{\"path\":\"notes.txt\",\"content\":\"one\\ntwo\\nthr"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},"delta":{"type":"tool_call_delta","index":0,"arguments":"thr"}}|}
    ];
  [%expect
    {|
    <div class="running tool tool-write">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> write </span>
        <span class="arg path"> notes.txt </span>
        <span class="chip"> 3 lines </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> one
    two
    thr </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "edit: the diff it returned, or its edits before it has run" =
  let call =
    {|{"id":"c5","name":"edit","arguments":"{\"path\":\"src/app.ml\",\"edits\":[{\"old_text\":\"let greet name = \\\"Hello, \\\" ^ name\",\"new_text\":\"let salute name = \\\"Hi, \\\" ^ name\"},{\"old_text\":\"  print_endline (greet \\\"world\\\");\",\"new_text\":\"  print_endline (salute \\\"world\\\");\"}]}"}|}
  in
  show ~running:true [ sprintf {|{"event":"tool_start","call":%s}|} call ];
  [%expect
    {|
    <div class="running tool tool-edit">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> edit </span>
        <span class="arg path"> src/app.ml </span>
        <span class="add chip"> +2 </span>
        <span class="chip del"> −2 </span>
      </div>
      <div class="tool-body">
        <div class="diff-view">
          <table class="diff">
            <tbody>
              <tr class="hunk">
                <td colspan="2"> edit 1 </td>
              </tr>
              <tr class="del">
                <td class="sign"> - </td>
                <td class="text"> let greet name = "Hello, " ^ name </td>
              </tr>
              <tr class="add">
                <td class="sign"> + </td>
                <td class="text"> let salute name = "Hi, " ^ name </td>
              </tr>
              <tr class="hunk">
                <td colspan="2"> edit 2 </td>
              </tr>
              <tr class="del">
                <td class="sign"> - </td>
                <td class="text">   print_endline (greet "world"); </td>
              </tr>
              <tr class="add">
                <td class="sign"> + </td>
                <td class="text">   print_endline (salute "world"); </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    |}];
  show
    [ sprintf
        {|{"type":"event","event":"tool_end","call":%s,"result":{"role":"tool_result","tool_call_id":"c5","tool_name":"edit","text":"--- a/src/app.ml\n+++ b/src/app.ml\n@@ -1,5 +1,5 @@\n-let greet name = \"Hello, \" ^ name\n+let salute name = \"Hi, \" ^ name\n \n let main () =\n-  print_endline (greet \"world\");\n+  print_endline (salute \"world\");\n   exit 0\n","is_error":false}}|}
        call
    ];
  [%expect
    {|
    <div class="ok tool tool-edit">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> edit </span>
        <span class="arg path"> src/app.ml </span>
        <span class="add chip"> +2 </span>
        <span class="chip del"> −2 </span>
      </div>
      <div class="tool-body">
        <div class="diff-view">
          <table class="diff">
            <tbody>
              <tr class="hunk">
                <td colspan="4"> @@ -1,5 +1,5 @@ </td>
              </tr>
              <tr class="del">
                <td class="ln"> 1 </td>
                <td class="ln">  </td>
                <td class="sign"> - </td>
                <td class="text"> let greet name = "Hello, " ^ name </td>
              </tr>
              <tr class="add">
                <td class="ln">  </td>
                <td class="ln"> 1 </td>
                <td class="sign"> + </td>
                <td class="text"> let salute name = "Hi, " ^ name </td>
              </tr>
              <tr class="same">
                <td class="ln"> 2 </td>
                <td class="ln"> 2 </td>
                <td class="sign">   </td>
                <td class="text">  </td>
              </tr>
              <tr class="same">
                <td class="ln"> 3 </td>
                <td class="ln"> 3 </td>
                <td class="sign">   </td>
                <td class="text"> let main () = </td>
              </tr>
              <tr class="del">
                <td class="ln"> 4 </td>
                <td class="ln">  </td>
                <td class="sign"> - </td>
                <td class="text">   print_endline (greet "world"); </td>
              </tr>
              <tr class="add">
                <td class="ln">  </td>
                <td class="ln"> 4 </td>
                <td class="sign"> + </td>
                <td class="text">   print_endline (salute "world"); </td>
              </tr>
              <tr class="same">
                <td class="ln"> 5 </td>
                <td class="ln"> 5 </td>
                <td class="sign">   </td>
                <td class="text">   exit 0 </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    |}];
  show
    [ sprintf
        {|{"event":"tool_end","call":%s,"result":{"role":"tool_result","tool_call_id":"c5","tool_name":"edit","text":"edit 2: old_text not found in file","is_error":true}}|}
        call
    ];
  [%expect
    {|
    <div class="error tool tool-edit">
      <div class="tool-head">
        <span class="icon"> ✕ </span>
        <span class="name"> edit </span>
        <span class="arg path"> src/app.ml </span>
        <span class="add chip"> +2 </span>
        <span class="chip del"> −2 </span>
      </div>
      <div class="tool-body">
        <div class="error output">
          <pre> edit 2: old_text not found in file </pre>
        </div>
        <div class="diff-view">
          <table class="diff">
            <tbody>
              <tr class="hunk">
                <td colspan="2"> edit 1 </td>
              </tr>
              <tr class="del">
                <td class="sign"> - </td>
                <td class="text"> let greet name = "Hello, " ^ name </td>
              </tr>
              <tr class="add">
                <td class="sign"> + </td>
                <td class="text"> let salute name = "Hi, " ^ name </td>
              </tr>
              <tr class="hunk">
                <td colspan="2"> edit 2 </td>
              </tr>
              <tr class="del">
                <td class="sign"> - </td>
                <td class="text">   print_endline (greet "world"); </td>
              </tr>
              <tr class="add">
                <td class="sign"> + </td>
                <td class="text">   print_endline (salute "world"); </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "ls, grep, find" =
  show
    [ {|{"type":"event","event":"tool_end","call":{"id":"c6","name":"ls","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"c6","tool_name":"ls","text":"hello.py\nsrc/\n","is_error":false}}|}
    ; {|{"type":"event","event":"tool_end","call":{"id":"c7","name":"grep","arguments":"{\"pattern\":\"salute\",\"path\":\"src\",\"glob\":\"*.ml\",\"ignore_case\":true}"},"result":{"role":"tool_result","tool_call_id":"c7","tool_name":"grep","text":"src/app.ml:1:let salute name = \"Hi, \" ^ name\nsrc/app.ml:4:  print_endline (salute \"world\");\n","is_error":false}}|}
    ; {|{"type":"event","event":"tool_end","call":{"id":"c8","name":"find","arguments":"{\"pattern\":\"*.ml\"}"},"result":{"role":"tool_result","tool_call_id":"c8","tool_name":"find","text":"src/app.ml\n","is_error":false}}|}
    ];
  [%expect
    {|
    <div class="ok tool tool-ls">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> ls </span>
        <span class="arg path"> . </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> hello.py
    src/ </pre>
        </div>
      </div>
    </div>
    <div class="ok tool tool-grep">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> grep </span>
        <span class="arg pattern"> salute </span>
        <span class="chip"> in src </span>
        <span class="chip"> *.ml </span>
        <span class="chip"> ignore case </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre>
            src/app.ml:1:let salute name = "Hi, " ^ name
    src/app.ml:4:  print_endline (salute "world");
          </pre>
        </div>
      </div>
    </div>
    <div class="ok tool tool-find">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> find </span>
        <span class="arg pattern"> *.ml </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> src/app.ml </pre>
        </div>
      </div>
    </div>
    |}]
;;

let subagent_start =
  {|{"type":"event","event":"subagent_start","call_id":"c12","agent_id":"a1","task":"Count the lines in src/app.ml and report back.\nBe brief.","model":"deepseek-flash","tools":["bash","read"]}|}
;;

let subagent_call =
  {|{"id":"c12","name":"subagent","arguments":"{\"task\":\"Count the lines in src/app.ml and report back.\\nBe brief.\"}"}|}
;;

let inner json =
  sprintf
    {|{"event":"subagent","call_id":"c12","agent_id":"a1","inner":%s}|}
    json
;;

let subagent_running =
  [ sprintf {|{"event":"tool_start","call":%s}|} subagent_call
  ; subagent_start
  ; inner {|{"type":"event","event":"agent_start"}|}
  ; inner {|{"type":"event","event":"turn_start"}|}
  ; inner
      {|{"type":"event","event":"message_start","message":{"role":"user","text":"Count the lines in src/app.ml and report back.\nBe brief."}}|}
  ; inner
      {|{"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"Counting."},{"type":"tool_call","id":"s1","name":"bash","arguments":"{\"command\":\"wc -l src/app.ml\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}}|}
  ; inner
      {|{"type":"event","event":"tool_start","call":{"id":"s1","name":"bash","arguments":"{\"command\":\"wc -l src/app.ml\"}"}}|}
  ]
;;

let subagent_done =
  subagent_running
  @ [ inner
        {|{"type":"event","event":"tool_end","call":{"id":"s1","name":"bash","arguments":"{\"command\":\"wc -l src/app.ml\"}"},"result":{"role":"tool_result","tool_call_id":"s1","tool_name":"bash","text":"5 src/app.ml\n","is_error":false}}|}
    ; inner {|{"type":"event","event":"turn_start"}|}
    ; inner
        {|{"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"src/app.ml has **5 lines**."}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}}|}
    ; inner {|{"type":"event","event":"agent_end","messages":[]}|}
    ; {|{"type":"event","event":"subagent_end","call_id":"c12","agent_id":"a1","usage":{"input":0,"output":0,"cache_read":0},"turns":2,"cost_usd":0.0123,"result":{"text":"src/app.ml has **5 lines**.\n[subagent: 2 turns, 0 in / 0 out tokens, $0.0123]","is_error":false}}|}
    ]
;;

let show_subagent ?running events =
  Chat_harness.show
    ~selector:".tool-subagent"
    (Chat_harness.chat ?running events)
;;

let%expect_test "subagent: running shows its latest step" =
  show_subagent ~running:true subagent_running;
  [%expect
    {|
    <div class="running tool tool-subagent">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> subagent </span>
        <span class="arg task"> Count the lines in src/app.ml and report back. … </span>
        <span class="chip model"> deepseek-flash </span>
        <span class="chip"> 1 turn </span>
      </div>
      <div class="tool-body">
        <details class="task">
          <summary>
            <span class="label"> Task </span>
            <span class="preview"> Count the lines in src/app.ml and report back. … </span>
          </summary>
          <div class="task-text"> Count the lines in src/app.ml and report back.
    Be brief. </div>
        </details>
        <div class="activity">
          <span class="arrow"> ↳ </span>
          <span class="text"> bash wc -l src/app.ml </span>
        </div>
        <details class="transcript">
          <summary>
            <span class="label"> Transcript </span>
            <span class="preview"> 2 messages </span>
          </summary>
          <div class="entries">
            <div class="msg user">
              <div class="bubble"> Count the lines in src/app.ml and report back.
    Be brief. </div>
            </div>
            <div class="assistant msg">
              <div class="markdown">
                <p> Counting. </p>
              </div>
              <div class="running tool tool-bash">
                <div class="tool-head">
                  <span class="spinner"> </span>
                  <span class="name"> bash </span>
                  <span class="arg command"> wc -l src/app.ml </span>
                </div>
              </div>
            </div>
          </div>
        </details>
      </div>
    </div>
    |}]
;;

let%expect_test "subagent: done, with its report and its transcript folded" =
  let chat = Chat_harness.chat subagent_done in
  Chat_harness.show ~selector:".tool-subagent > .tool-head" chat;
  Chat_harness.show ~selector:".tool-subagent .report" chat;
  Chat_harness.text ~selector:".tool-subagent .transcript" chat;
  [%expect
    {|
    <div class="tool-head">
      <span class="icon"> ✓ </span>
      <span class="name"> subagent </span>
      <span class="arg task"> Count the lines in src/app.ml and report back. … </span>
      <span class="chip model"> deepseek-flash </span>
      <span class="chip"> 2 turns </span>
      <span class="chip"> $0.01 </span>
    </div>
    <details class="report">
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
    Transcript 3 messages Count the lines in src/app.ml and report back.
    Be brief. Counting. ✓ bash wc -l src/app.ml 5 src/app.ml src/app.ml has 5 lines .
    |}]
;;

let%expect_test
    "subagent in the background: the agent it started, still running after the \
     turn"
  =
  show_subagent
    (subagent_running
     @ [ sprintf
           {|{"type":"event","event":"tool_end","call":%s,"result":{"role":"tool_result","tool_call_id":"c12","tool_name":"subagent","text":"started agent a1 (Count the lines in src/app.ml and report back.); its result will be delivered to you when it finishes; use subagent_wait to block on it","is_error":false}}|}
           subagent_call
       ]);
  [%expect
    {|
    <div class="running tool tool-subagent">
      <div class="tool-head">
        <span class="spinner"> </span>
        <span class="name"> subagent </span>
        <span class="arg task"> Count the lines in src/app.ml and report back. … </span>
        <span class="chip job"> agent a1 </span>
        <span class="chip model"> deepseek-flash </span>
        <span class="chip"> 1 turn </span>
      </div>
      <div class="tool-body">
        <details class="task">
          <summary>
            <span class="label"> Task </span>
            <span class="preview"> Count the lines in src/app.ml and report back. … </span>
          </summary>
          <div class="task-text"> Count the lines in src/app.ml and report back.
    Be brief. </div>
        </details>
        <div class="activity">
          <span class="arrow"> ↳ </span>
          <span class="text"> bash wc -l src/app.ml </span>
        </div>
        <details class="transcript">
          <summary>
            <span class="label"> Transcript </span>
            <span class="preview"> 2 messages </span>
          </summary>
          <div class="entries">
            <div class="msg user">
              <div class="bubble"> Count the lines in src/app.ml and report back.
    Be brief. </div>
            </div>
            <div class="assistant msg">
              <div class="markdown">
                <p> Counting. </p>
              </div>
              <div class="running tool tool-bash">
                <div class="tool-head">
                  <span class="spinner"> </span>
                  <span class="name"> bash </span>
                  <span class="arg command"> wc -l src/app.ml </span>
                </div>
              </div>
            </div>
          </div>
        </details>
      </div>
    </div>
    |}]
;;

let%expect_test "subagent: failed" =
  show_subagent
    [ sprintf {|{"event":"tool_start","call":%s}|} subagent_call
    ; subagent_start
    ; {|{"event":"subagent_end","call_id":"c12","agent_id":"a1","usage":{"input":0,"output":0,"cache_read":0},"turns":1,"cost_usd":0,"result":{"text":"subagent failed: 529 overloaded\n[subagent: 1 turns, 0 in / 0 out tokens, $0.0000]","is_error":true}}|}
    ];
  [%expect
    {|
    <div class="error tool tool-subagent">
      <div class="tool-head">
        <span class="icon"> ✕ </span>
        <span class="name"> subagent </span>
        <span class="arg task"> Count the lines in src/app.ml and report back. … </span>
        <span class="chip model"> deepseek-flash </span>
        <span class="chip"> 1 turn </span>
        <span class="chip"> $0.00 </span>
      </div>
      <div class="tool-body">
        <details class="task">
          <summary>
            <span class="label"> Task </span>
            <span class="preview"> Count the lines in src/app.ml and report back. … </span>
          </summary>
          <div class="task-text"> Count the lines in src/app.ml and report back.
    Be brief. </div>
        </details>
        <div class="error output">
          <pre> subagent failed: 529 overloaded </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "subagent from a reloaded session: no events, only its result" =
  let chat =
    Chat.of_messages
      [ Assistant
          { content =
              [ Tool_call
                  (Prigh_protocol.Tool_call.of_json
                     (Jsonaf.of_string subagent_call)
                   |> Or_error.ok_exn)
              ]
          ; stop_reason = Tool_use
          ; usage = Prigh_protocol.Usage.zero
          ; model = "m"
          }
      ; Tool_result
          { tool_call_id = "c12"
          ; tool_name = "subagent"
          ; text =
              "There are 5 lines.\n\
               [subagent: 2 turns, 10 in / 5 out tokens, $0.0010]"
          ; is_error = false
          ; images = []
          }
      ]
  in
  Chat_harness.show ~selector:".tool" chat;
  [%expect
    {|
    <div class="ok tool tool-subagent">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> subagent </span>
        <span class="arg task"> Count the lines in src/app.ml and report back. … </span>
      </div>
      <div class="tool-body">
        <details class="task">
          <summary>
            <span class="label"> Task </span>
            <span class="preview"> Count the lines in src/app.ml and report back. … </span>
          </summary>
          <div class="task-text"> Count the lines in src/app.ml and report back.
    Be brief. </div>
        </details>
        <details class="report">
          <summary>
            <span class="label"> Report </span>
            <span class="preview"> There are 5 lines. </span>
          </summary>
          <div class="markdown">
            <p> There are 5 lines. </p>
          </div>
        </details>
      </div>
    </div>
    |}]
;;

let%expect_test
    "jobs: status, waiting (reports as cards), a note of what still runs"
  =
  show
    [ {|{"type":"event","event":"tool_end","call":{"id":"c11","name":"job_status","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"c11","tool_name":"job_status","text":"j1  running  0s  sleep 1; echo bg finished  (last: 0 B, no output)","is_error":false}}|}
    ; {|{"type":"event","event":"tool_end","call":{"id":"c13","name":"job_wait","arguments":"{\"ids\":[\"j1\",\"j2\"]}"},"result":{"role":"tool_result","tool_call_id":"c13","tool_name":"job_wait","text":"[job j1 exited 0] sleep 1; echo bg finished\nbg finished\n\ntimed out; still running: j2 (sleep 100)","is_error":false}}|}
    ; {|{"event":"tool_end","call":{"id":"c14","name":"job_kill","arguments":"{\"id\":\"j2\"}"},"result":{"role":"tool_result","tool_call_id":"c14","tool_name":"job_kill","text":"[job j2 killed] sleep 100","is_error":false}}|}
    ; {|{"event":"tool_end","call":{"id":"c15","name":"job_output","arguments":"{\"id\":\"j3\"}"},"result":{"role":"tool_result","tool_call_id":"c15","tool_name":"job_output","text":"[job j3 running after 3s, 12 bytes; lines 1-2 of 2] make\nstep 1\nstep 2","is_error":false}}|}
    ; {|{"event":"tool_end","call":{"id":"c16","name":"job_wait","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"c16","tool_name":"job_wait","text":"no jobs to wait for","is_error":false}}|}
    ];
  [%expect
    {|
    <div class="ok tool tool-job_status">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> job_status </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> j1  running  0s  sleep 1; echo bg finished  (last: 0 B, no output) </pre>
        </div>
      </div>
    </div>
    <div class="ok tool tool-job_wait">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> job_wait </span>
        <span class="arg"> j1 j2 </span>
      </div>
      <div class="tool-body">
        <div class="delivery">
          <div class="delivery-section ok">
            <div class="delivery-head">
              <span class="icon"> ↩ </span>
              <span class="kind"> job j1 </span>
              <span class="chip ok"> exited 0 </span>
              <span class="task"> sleep 1; echo bg finished </span>
            </div>
            <details class="delivery-body">
              <summary>
                <span class="label"> Output </span>
                <span class="preview"> bg finished </span>
              </summary>
              <div class="output">
                <pre> bg finished </pre>
              </div>
            </details>
          </div>
        </div>
        <div class="output">
          <pre> timed out; still running: j2 (sleep 100) </pre>
        </div>
      </div>
    </div>
    <div class="ok tool tool-job_kill">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> job_kill </span>
        <span class="arg"> j2 </span>
      </div>
      <div class="tool-body">
        <div class="delivery">
          <div class="bad delivery-section">
            <div class="delivery-head">
              <span class="icon"> ↩ </span>
              <span class="kind"> job j2 </span>
              <span class="bad chip"> killed </span>
              <span class="task"> sleep 100 </span>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="ok tool tool-job_output">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> job_output </span>
        <span class="arg"> j3 </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> [job j3 running after 3s, 12 bytes; lines 1-2 of 2] make
    step 1
    step 2 </pre>
        </div>
      </div>
    </div>
    <div class="ok tool tool-job_wait">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> job_wait </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> no jobs to wait for </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "a call that never got a result: the agent stopped" =
  show
    [ {|{"event":"tool_start","call":{"id":"c1","name":"bash","arguments":"{\"command\":\"sleep 100\"}"}}|}
    ];
  [%expect
    {|
    <div class="interrupted tool tool-bash">
      <div class="tool-head">
        <span class="icon"> ■ </span>
        <span class="name"> bash </span>
        <span class="arg command"> sleep 100 </span>
        <span class="chip"> no result </span>
      </div>
    </div>
    |}]
;;

let%expect_test "other tools: their main argument and output" =
  show
    [ {|{"event":"tool_end","call":{"id":"c1","name":"web_fetch","arguments":"{\"url\":\"https://x\"}"},"result":{"role":"tool_result","tool_call_id":"c1","tool_name":"web_fetch","text":"fetched","is_error":false}}|}
    ];
  [%expect
    {|
    <div class="ok tool tool-web_fetch">
      <div class="tool-head">
        <span class="icon"> ✓ </span>
        <span class="name"> web_fetch </span>
        <span class="arg"> https://x </span>
      </div>
      <div class="tool-body">
        <div class="output">
          <pre> fetched </pre>
        </div>
      </div>
    </div>
    |}]
;;
