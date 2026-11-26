open! Core
open Prigh_web_app

(* The page's scroll handling (web_main.ml): when the chat is at its end,
   when following must write [scrollTop], and a turn up that the chat does
   not take. *)

let%expect_test "at its end, and off it: overscroll and fractional scrollTop" =
  List.iter
    [ "the very end", 1000.
    ; "half a pixel short (a zoom level)", 999.5
    ; "past the end (a phone's overscroll)", 1030.
    ; "a line up", 980.
    ; "40px up", 960.
    ; "a screen up", 500.
    ]
    ~f:(fun (label, top) ->
      let p : Chat_follow.Position.t = { top; height = 1600.; client = 600. } in
      printf
        "%-38s at_end %-5b off_end %b\n"
        label
        (Chat_follow.at_end p)
        (Chat_follow.off_end p));
  [%expect
    {|
    the very end                           at_end true  off_end false
    half a pixel short (a zoom level)      at_end true  off_end false
    past the end (a phone's overscroll)    at_end true  off_end false
    a line up                              at_end true  off_end true
    40px up                                at_end false off_end true
    a screen up                            at_end false off_end true
    |}]
;;

let run label ~frames =
  let rec go turn = function
    | [] -> print_endline "  still waiting"
    | (top, held) :: rest ->
      (match Chat_follow.Turn_up.frame turn ~top ~held with
       | Waiting turn -> go turn rest
       | outcome -> print_s [%sexp (outcome : Chat_follow.Turn_up.Outcome.t)])
  in
  printf "%s: " label;
  go (Chat_follow.Turn_up.start ~top:1000.) frames
;;

let%expect_test "a turn up: taken when the chat moves up, else follows again" =
  run "the chat moves up" ~frames:[ 1000., false; 940., false ];
  run
    "overscroll giving way (nothing moves)"
    ~frames:(List.init 6 ~f:(fun _ -> 1000., false));
  run
    "new output only (the chat grows, top stays)"
    ~frames:[ 1000., false; 1000.4, false; 1000., false; 1000., false ];
  run "a finger held still" ~frames:(List.init 20 ~f:(fun _ -> 1000., true));
  run
    "a finger lifted, then the fling lands"
    ~frames:
      (List.init 10 ~f:(fun _ -> 1000., true) @ [ 1000., false; 990., false ]);
  [%expect
    {|
    the chat moves up: Taken
    overscroll giving way (nothing moves): Not_taken
    new output only (the chat grows, top stays): Not_taken
    a finger held still:   still waiting
    a finger lifted, then the fling lands: Taken
    |}]
;;

let%expect_test "another turn before the first settles waits afresh" =
  let turn = Chat_follow.Turn_up.start ~top:1000. in
  let rec frames turn n =
    match Chat_follow.Turn_up.frame turn ~top:1000. ~held:false with
    | Waiting turn -> frames turn (n + 1)
    | outcome -> n, outcome
  in
  let step turn =
    match Chat_follow.Turn_up.frame turn ~top:1000. ~held:false with
    | Waiting turn -> turn
    | _ -> assert false
  in
  let later = Chat_follow.Turn_up.again (step (step turn)) in
  print_s
    [%sexp
      (frames turn 1 : int * Chat_follow.Turn_up.Outcome.t)
    , (frames later 1 : int * Chat_follow.Turn_up.Outcome.t)];
  [%expect {| ((4 Not_taken) (4 Not_taken)) |}]
;;
