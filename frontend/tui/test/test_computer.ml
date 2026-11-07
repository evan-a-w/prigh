open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* The [computer] tool (the agent driving the user's screen) in the terminal:
   one line per action, what it did and the screenshot it got back. *)

let shot : P.Image.t = { mime_type = "image/png"; bytes = 81234; data = "" }

let result_text ~action =
  sprintf "%s\ncursor at (640, 312)\nscreenshot 1280×800" action
;;

let%expect_test "every action, described for prigh-web and the terminal" =
  List.iter
    [ {|{"action":"screenshot"}|}
    ; {|{"action":"click","x":640,"y":312}|}
    ; {|{"action":"click","x":640,"y":312,"count":2}|}
    ; {|{"action":"click","x":5,"y":6,"button":"right","keys":"ctrl"}|}
    ; {|{"action":"click"}|}
    ; {|{"action":"move","x":1,"y":2}|}
    ; {|{"action":"drag","path":[[10,10],[50,20],[200,40]]}|}
    ; {|{"action":"mouse_down","button":"left","x":3,"y":4}|}
    ; {|{"action":"mouse_up"}|}
    ; {|{"action":"scroll","x":640,"y":312,"dx":0,"dy":3}|}
    ; {|{"action":"scroll","dx":-2,"dy":-1}|}
    ; {|{"action":"type","text":"hello"}|}
    ; {|{"action":"type","text":"a long text that goes on and on and on past the width"}|}
    ; {|{"action":"key","combo":"ctrl+shift+t"}|}
    ; {|{"action":"hold_key","combo":"shift","duration":1.5}|}
    ; {|{"action":"wait","seconds":2}|}
    ; {|{"action":"zoom","region":[0,0,100,80]}|}
    ; {|{"action":"cursor_position"}|}
    ; {|{"action":"teleport"}|}
    ; {|{"action":"type","text":"hel|}
    ; {|{"act|}
    ]
    ~f:(fun arguments ->
      let call = { P.Tool_call.id = "c"; name = "computer"; arguments } in
      match Computer_action.of_call call with
      | None -> printf "%-28s (no action yet)\n" arguments
      | Some action ->
        printf
          "%s | %s | %s\n"
          (Computer_action.describe action)
          (Computer_action.compact action)
          (Option.value_map
             (Computer_action.point action)
             ~default:"-"
             ~f:(fun { x; y } -> sprintf "%d,%d" x y)));
  [%expect
    {|
    screenshot | screenshot | -
    click (640, 312) | click 640,312 | 640,312
    double-click (640, 312) | double-click 640,312 | 640,312
    ctrl+right click (5, 6) | ctrl+right click 5,6 | 5,6
    click | click | -
    move (1, 2) | move 1,2 | 1,2
    drag (10, 10) → (200, 40) | drag 10,10 → 200,40 | 200,40
    left down (3, 4) | left down 3,4 | 3,4
    left up | left up | -
    scroll ↓3 at (640, 312) | scroll ↓3 at 640,312 | 640,312
    scroll ↑1 ←2 | scroll ↑1 ←2 | -
    type “hello” | type “hello” | -
    type “a long text that goes on and on and on …” | type “a long text that goes on and on and on …” | -
    key ctrl+shift+t | key ctrl+shift+t | -
    hold shift 1.5s | hold shift 1.5s | -
    wait 2s | wait 2s | -
    zoom [0, 0, 100, 80] | zoom [0, 0, 100, 80] | -
    cursor position | cursor position | -
    teleport | teleport | -
    type “hel” | type “hel” | -
    {"act                        (no action yet)
    |}];
  List.iter
    [ result_text ~action:"click (640, 312) → (1280, 624) on the screen"
    ; "waited 2s"
    ; "screenshot 0×5"
    ; "screenshot 1280x800"
    ]
    ~f:(fun text ->
      print_s
        [%sexp (Computer_action.screenshot_size text : (int * int) option)]);
  [%expect
    {|
    ((1280 800))
    ()
    ()
    ()
    |}]
;;

let%expect_test "one line per action: running, done with its screenshot, failed"
  =
  let h = Test_app.connected ~height:14 () in
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "open a new tab"));
  let click =
    tool_call
      ~name:"computer"
      ~arguments:{|{"action":"click","x":640,"y":312}|}
      "c1"
  in
  H.event h (Tool_start click);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ open a new tab
    … computer click 640,312
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.event
    h
    (Tool_end
       { call = click
       ; result =
           tool_result
             ~name:"computer"
             ~id:"c1"
             ~images:[ shot ]
             (result_text
                ~action:"click (640, 312) → (1280, 624) on the screen")
       });
  let keys =
    tool_call
      ~name:"computer"
      ~arguments:{|{"action":"key","combo":"ctrl+t"}|}
      "c2"
  in
  H.event h (Tool_start keys);
  H.event
    h
    (Tool_end
       { call = keys
       ; result =
           tool_result
             ~name:"computer"
             ~id:"c2"
             ~is_error:true
             "[interrupted] you moved the mouse: stopped (computer use paused \
              until the next prompt)"
       });
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ open a new tab
    ✓ computer click 640,312  → screenshot 1280×800
    ✕ computer key ctrl+t
        [interrupted] you moved the mouse: stopped (computer use
        paused until the next prompt)
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "protocol: a host offering computer use says what it controls" =
  let host json =
    match P.Json.parse json |> Or_error.bind ~f:P.Host.of_json with
    | Ok host -> print_s [%sexp (host.computer : P.Computer.t option)]
    | Error e -> print_s [%sexp (e : Error.t)]
  in
  host
    {|{"id":"host-1","name":"laptop","cwd":"/w","session_id":null,"session_name":null,"computer":{"backend":"wlroots","detail":"Hyprland","platform":"linux","output":"DP-1","width":2560,"height":1440,"scale":2.0,"scaled_width":1280,"scaled_height":720}}|};
  (* Without it, and from a newer host with fields missing. *)
  host {|{"id":"host-1","name":"laptop","cwd":"/w"}|};
  host
    {|{"id":"host-1","name":"laptop","cwd":"/w","computer":{"backend":"x11"}}|};
  host {|{"id":"host-1","name":"laptop","cwd":"/w","computer":true}|};
  [%expect
    {|
    ((
      (backend wlroots)
      (detail   (Hyprland))
      (platform (linux))
      (width         2560)
      (height        1440)
      (scaled_width  1280)
      (scaled_height 720)
      (scale         2)))
    ()
    ((
      (backend x11)
      (detail   ())
      (platform ())
      (width         0)
      (height        0)
      (scaled_width  0)
      (scaled_height 0)
      (scale         1)))
    ()
    |}]
;;
