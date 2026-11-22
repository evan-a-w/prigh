open! Core
open! Prigh
open! Test_computer_consent

let%expect_test
    "the first action asks (also in auto mode); while allowed the next ones do \
     not; the next run asks again"
  =
  with_agent
    [ click "c1"; click "c2"; Reply.text "done"; click "c3"; Reply.text "done" ]
  @@ fun _ agent fake ->
  print_endline (Approval_mode.to_string (Agent.state agent).approval_mode);
  prompt agent "open the menu";
  prompt agent "again";
  List.iter (Computer_fake.log fake) ~f:print_endline;
  [%expect
    {|
    notice: tools now run on desk in $DIR
    auto
    > open the menu
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    computer c1 -> click (10, 20) → (20, 40) on the screen
    computer c2 -> click (10, 20) → (20, 40) on the screen
    control: none
    > again
    confirm c3 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c3
    control: desk
    computer c3 -> click (10, 20) → (20, 40) on the screen
    control: none
    move 20,40
    click left×1
    move 20,40
    click left×1
    move 20,40
    click left×1
    |}]
;;

let%expect_test
    "a subagent asks through the same dialog; the consent covers the run"
  =
  with_agent
    ~sub_replies:[ click "s1"; click "s2"; Reply.text "clicked" ]
    [ Reply.tool_call
        ~id:"a1"
        ~name:"subagent"
        ~arguments:{|{"task":"click the menu","background":false}|}
        ()
    ; click "c1"
    ; Reply.text "done"
    ]
  @@ fun _ agent _ ->
  prompt agent "go";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    subagent a1 -> started agent a1 (click the menu); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm a1/s1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    confirm done a1/s1
    computer c1 -> click (10, 20) → (20, 40) on the screen
    control: none
      [a1] computer s1 -> click (10, 20) → (20, 40) on the screen
    confirm a1/s2 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done a1/s2
    control: desk
      [a1] computer s2 -> click (10, 20) → (20, 40) on the screen
    control: none
    |}]
;;
