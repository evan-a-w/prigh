open! Core
module H = Harness

(* The [computer] tool: the agent driving the user's screen. Its card says
   what it did and shows the screen after, the point marked; while a run
   drives the screen, a strip over the composer says whose, with Stop. *)

let png = "iVBORw0KGgo="

let call ?(id = "c1") arguments =
  sprintf
    {|{"id":"%s","name":"computer","arguments":%s}|}
    id
    (Jsonaf.to_string (`String arguments))
;;

let tool_end ?id ?(is_error = false) ?(images = true) arguments text =
  sprintf
    {|{"type":"event","event":"tool_end","call":%s,"result":{"role":"tool_result","tool_call_id":"%s","tool_name":"computer","text":%s,"is_error":%b%s}}|}
    (call ?id arguments)
    (Option.value id ~default:"c1")
    (Jsonaf.to_string (`String text))
    is_error
    (if images
     then sprintf {|,"images":[{"mime_type":"image/png","data":"%s"}]|} png
     else "")
;;

let shot action = sprintf "%s\ncursor at (640, 312)\nscreenshot 1280×800" action

let%expect_test
    "the card: the action, the screen after it with the point marked"
  =
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.chat
       [ tool_end
           {|{"action":"click","x":640,"y":312}|}
           (shot "click (640, 312) → (1280, 624) on the screen")
       ; tool_end
           ~id:"c2"
           {|{"action":"type","text":"hello"}|}
           (shot "typed 5 characters")
       ; tool_end
           ~id:"c3"
           ~images:false
           {|{"action":"wait","seconds":2}|}
           "waited 2s"
       ; tool_end
           ~id:"c4"
           ~is_error:true
           ~images:false
           {|{"action":"key","combo":"ctrl+t"}|}
           "[interrupted] you moved the mouse: stopped (computer use paused \
            until the next prompt)"
       ]);
  [%expect
    {|
    <div class="ok open tool tool-computer">
      <button type="button" data-fold="c1#open" aria-expanded="true" class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> computer </span>
        <span class="arg"> click (640, 312) </span>
        <span class="chip"> 1280×800 </span>
        <span class="chev">  </span>
      </button>
      <div class="tool-body">
        <div data-viewer-group="" class="screen">
          <button type="button"
                  title="[image: image/png, 8 B]"
                  aria-label="View the screenshot, [image: image/png, 8 B]"
                  data-viewer=""
                  class="image screen-shot">
            <img src="data:image/png;base64,iVBORw0KGgo="
                 alt="the screen after the action"
                 class="thumb"/>
            <span aria-hidden="true" class="screen-mark" style={ top: 39.000%; left: 50.000%; }> </span>
          </button>
        </div>
        <span class="caption"> click (640, 312) → (1280, 624) on the screen
    cursor at (640, 312) </span>
      </div>
    </div>
    <div class="ok open tool tool-computer">
      <button type="button" data-fold="c2#open" aria-expanded="true" class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> computer </span>
        <span class="arg"> type “hello” </span>
        <span class="chip"> 1280×800 </span>
        <span class="chev">  </span>
      </button>
      <div class="tool-body">
        <div data-viewer-group="" class="screen">
          <button type="button"
                  title="[image: image/png, 8 B]"
                  aria-label="View the screenshot, [image: image/png, 8 B]"
                  data-viewer=""
                  class="image screen-shot">
            <img src="data:image/png;base64,iVBORw0KGgo="
                 alt="the screen after the action"
                 class="thumb"/>
          </button>
        </div>
        <span class="caption"> typed 5 characters
    cursor at (640, 312) </span>
      </div>
    </div>
    <div class="ok open tool tool-computer">
      <button type="button" data-fold="c3#open" aria-expanded="true" class="tool-head">
        <span title="Done" role="img" aria-label="Done" class="done mark">
          <icon class="check"> </icon>
        </span>
        <span class="name"> computer </span>
        <span class="arg"> wait 2s </span>
        <span class="chev">  </span>
      </button>
      <div class="tool-body">
        <span class="caption"> waited 2s </span>
      </div>
    </div>
    <div class="error open tool tool-computer">
      <button type="button" data-fold="c4#open" aria-expanded="true" class="tool-head">
        <span title="Failed" role="img" aria-label="Failed" class="failed mark">
          <icon class="cross"> </icon>
        </span>
        <span class="name"> computer </span>
        <span class="arg"> key ctrl+t </span>
        <span class="chev">  </span>
      </button>
      <div class="tool-body">
        <div class="error output">
          <pre>
            [interrupted] you moved the mouse: stopped (computer use paused until the next prompt)
          </pre>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "while streaming: the action so far, running" =
  Chat_harness.show
    ~selector:".tool"
    (Chat_harness.chat
       ~running:true
       [ {|{"type":"event","event":"tool_start","call":{"id":"c1","name":"computer","arguments":"{\"action\":\"scroll\",\"x\":10,\"y\":20,\"dy\":3}"}}|}
       ]);
  [%expect
    {|
    <div class="running tool tool-computer">
      <div class="tool-head">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="name"> computer </span>
        <span class="arg"> scroll ↓3 at (10, 20) </span>
      </div>
    </div>
    |}]
;;

let running = sprintf {|{"event":"state","state":%s}|}

let state ?(control = true) ~running () =
  H.state_json
    ~fields:
      [ ("running", if running then `True else `False)
      ; ("computer_control", if control then `String "laptop" else `Null)
      ; "active_host", `String "host-1"
      ; ( "hosts"
        , Jsonaf.of_string
            {|[{"id":"host-1","name":"laptop","cwd":"/work","session_id":"s1","session_name":null,"computer":{"backend":"wlroots","detail":"Hyprland","platform":"linux","width":2560,"height":1600,"scale":2.0,"scaled_width":1280,"scaled_height":800}}]|}
        )
      ]
    ()
;;

let%expect_test
    "the banner: while the user allows prigh to control the screen; Stop \
     aborts the run"
  =
  let h = H.create () in
  let banner () = H.text h ~selector:".computer-banner" in
  (* Running, still asking (or never asked): no banner. *)
  H.quiet h (fun () ->
    H.event h (running (state ~control:false ~running:true ()));
    H.event
      h
      {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"computer","arguments":"{\"action\":\"key\",\"combo\":\"ctrl+t\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}}|});
  banner ();
  [%expect {| |}];
  (* Allowed. *)
  H.quiet h (fun () -> H.event h (running (state ~running:true ())));
  banner ();
  [%expect {| prigh is controlling laptop (Stop) |}];
  H.names h ~selector:".computer-banner button";
  [%expect {| button "Stop" -- Stop the agent (Esc) |}];
  H.click h ~selector:".computer-banner-stop";
  [%expect
    {|
    Abort
    (Rpc (method_ abort) (params ()) (tag Restored))
    |}];
  (* The run ended, and with it control. *)
  H.quiet h (fun () ->
    H.event h (running (state ~control:false ~running:false ())));
  banner ();
  [%expect {| |}];
  (* A stale grant on an idle session shows nothing either. *)
  H.quiet h (fun () -> H.event h (running (state ~running:false ())));
  banner ();
  [%expect {| |}]
;;
