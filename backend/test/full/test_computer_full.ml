open! Core
open! Prigh
open Tool_test_helpers
open! Test_computer

let%expect_test "helper: actions through the protocol, respawn, kill switch" =
  with_sandbox
  @@ fun _ ->
  let fake = Computer_fake.create () in
  let starts = ref 0 in
  let helper =
    Computer_helper.create (Computer_fake.info fake) ~start:(fun () ->
      incr starts;
      Computer_fake.transport fake)
  in
  let run json =
    printf "> %s\n" json;
    show_result
      (Computer_helper.run
         helper
         ~cancel:Cancellation.never
         (Json.of_string json))
  in
  run {|{"action":"screenshot"}|};
  run {|{"action":"click","x":640,"y":312}|};
  run {|{"action":"type","text":"hello"}|};
  run {|{"action":"scroll","x":10,"y":20,"dy":3}|};
  run {|{"action":"wait","seconds":0.01}|};
  run {|{"action":"zoom","region":[0,0,320,200]}|};
  run {|{"action":"click","x":1,"y":2,"button":"top"}|};
  printf "starts: %d\n" !starts;
  List.iter (Computer_fake.log fake) ~f:print_endline;
  [%expect
    {|
    > {"action":"screenshot"}
    screenshot
    cursor at (640, 400)
    screenshot 1280×800
      <image image/png, 1280x800>
    > {"action":"click","x":640,"y":312}
    click (640, 312) → (1280, 624) on the screen
    cursor at (640, 312)
    screenshot 1280×800
      <image image/png, 1280x800>
    > {"action":"type","text":"hello"}
    type “hello”
    cursor at (640, 312)
    screenshot 1280×800
      <image image/png, 1280x800>
    > {"action":"scroll","x":10,"y":20,"dy":3}
    scroll ↓3 at (10, 20) → (20, 40) on the screen
    cursor at (10, 20)
    screenshot 1280×800
      <image image/png, 1280x800>
    > {"action":"wait","seconds":0.01}
    wait 0.01s
    waited
    cursor at (10, 20)
    > {"action":"zoom","region":[0,0,320,200]}
    zoom [0, 0, 320, 200]
    (0, 0)-(640, 400) on the screen
    cursor at (10, 20)
    screenshot 640×400
      <image image/png, 640x400>
    > {"action":"click","x":1,"y":2,"button":"top"}
    error: click: "button" must be one of left, right, middle, back, forward, not "top"
    starts: 1
    move 1280,624
    click left×1
    type hello
    move 20,40
    scroll 0,3
    wait 0.01s
    |}];
  (* The user moves the mouse: the action is refused. *)
  Computer_fake.user_move fake ~x:5 ~y:5;
  run {|{"action":"click","x":640,"y":312}|};
  [%expect
    {|
    > {"action":"click","x":640,"y":312}
    error: [interrupted] you moved the mouse: stopped (computer use paused until the next prompt)
    |}];
  (* A helper that died is started again for the next action. *)
  Computer_fake.crash fake;
  run {|{"action":"key","combo":"ctrl+l"}|};
  printf "starts: %d\n" !starts;
  [%expect
    {|
    > {"action":"key","combo":"ctrl+l"}
    key ctrl+l
    cursor at (3, 3)
    screenshot 1280×800
      <image image/png, 1280x800>
    starts: 2
    |}];
  (* A cancelled call kills the helper. *)
  let cancel = Cancellation.create () in
  Cancellation.cancel cancel;
  show_result
    (Computer_helper.run
       helper
       ~cancel
       (Json.of_string {|{"action":"screenshot"}|}));
  run {|{"action":"screenshot"}|};
  printf "starts: %d\n" !starts;
  [%expect
    {|
    error: [cancelled]
    > {"action":"screenshot"}
    screenshot
    cursor at (3, 3)
    screenshot 1280×800
      <image image/png, 1280x800>
    starts: 3
    |}]
;;

let%expect_test
    "a sequence of actions: real coordinates, every screenshot under the cap"
  =
  let call id args = Reply.tool_call ~id ~name:"computer" ~arguments:args () in
  with_agent
    [ call "s1" {|{"action":"screenshot"}|}
    ; call "c2" {|{"action":"click","x":640,"y":312}|}
    ; call "t3" {|{"action":"type","text":"hello"}|}
    ; call "w4" {|{"action":"wait","seconds":0.01}|}
    ; call "k5" {|{"action":"key","combo":"Return"}|}
    ; call "z6" {|{"action":"zoom","region":[0,0,320,200]}|}
    ; Reply.text "done"
    ]
  @@ fun ~set_computer:_ fake agent ->
  Or_error.ok_exn (Agent.prompt agent "do it");
  Agent.wait_idle agent;
  print_endline "--- input on the screen";
  List.iter (Computer_fake.log fake) ~f:print_endline;
  [%expect
    {|
    notice: tools now run on desk in $DIR
    --- request
    assistant:
    allowed control of desk
    --- request
      sees s1: screenshot | cursor at (640, 400) | screenshot 1280×800 <screenshot>
    assistant:
    --- request
      sees s1: screenshot | cursor at (640, 400) | screenshot 1280×800 <screenshot>
      sees c2: click (640, 312) → (1280, 624) on the screen | cursor at (640, 312) | screenshot 1280×800 <screenshot>
    assistant:
    --- request
      sees s1: screenshot | cursor at (640, 400) | screenshot 1280×800 <screenshot>
      sees c2: click (640, 312) → (1280, 624) on the screen | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees t3: type “hello” | cursor at (640, 312) | screenshot 1280×800 <screenshot>
    assistant:
    --- request
      sees s1: screenshot | cursor at (640, 400) | screenshot 1280×800 <screenshot>
      sees c2: click (640, 312) → (1280, 624) on the screen | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees t3: type “hello” | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees w4: wait 0.01s | waited | cursor at (640, 312)
    assistant:
    --- request
      sees s1: screenshot | cursor at (640, 400) | screenshot 1280×800 <screenshot>
      sees c2: click (640, 312) → (1280, 624) on the screen | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees t3: type “hello” | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees w4: wait 0.01s | waited | cursor at (640, 312)
      sees k5: key Return | cursor at (640, 312) | screenshot 1280×800 <screenshot>
    assistant:
    --- request
      sees s1: screenshot | cursor at (640, 400) | screenshot 1280×800 <screenshot>
      sees c2: click (640, 312) → (1280, 624) on the screen | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees t3: type “hello” | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees w4: wait 0.01s | waited | cursor at (640, 312)
      sees k5: key Return | cursor at (640, 312) | screenshot 1280×800 <screenshot>
      sees z6: zoom [0, 0, 320, 200] | (0, 0)-(640, 400) on the screen | cursor at (640, 312) | screenshot 640×400 <screenshot>
    assistant: done
    --- input on the screen
    move 1280,624
    click left×1
    type hello
    wait 0.01s
    key Return
    |}]
;;
