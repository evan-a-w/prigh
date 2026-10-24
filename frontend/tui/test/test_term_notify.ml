open! Core
open! Expect_test_helpers_core
open Prigh_tui

let%expect_test "focus reports are taken out of the input" =
  List.iter
    [ "abc"; "\027[I"; "x\027[Oy\027[Iz"; "\027[A\027[O"; "\027"; "\027[" ]
    ~f:(fun input ->
      let rest, reports = Term_focus.split input in
      print_s [%sexp (input : string), (rest : string), (reports : bool list)]);
  [%expect {|
    (abc abc ())
    ("\027[I" "" (true))
    ("x\027[Oy\027[Iz" xyz (false true))
    ("\027[A\027[O" "\027[A" (false))
    ("\027" "\027" ())
    ("\027[" "\027[" ())
    |}]
;;

let%expect_test "the bell, OSC 9, OSC 777 and tmux passthrough" =
  let n = { Prigh_ui.Notify.title = "prigh: done in proj"; body = "a;b\027c" } in
  List.iter
    [ Prigh_ui.Notify.Mode.Bell, Some "xterm-256color", false
    ; Desktop, Some "xterm-256color", false
    ; Desktop, Some "foot", false
    ; Desktop, Some "tmux-256color", true
    ; Bell, Some "tmux-256color", true
    ; Off, None, false
    ]
    ~f:(fun (mode, term, tmux) ->
      print_s
        [%sexp
          (mode : Prigh_ui.Notify.Mode.t)
        , (term : string option)
        , (tmux : bool)
        , (Term_notify.sequence mode ~term ~tmux n : string)]);
  [%expect {|
    (Bell (xterm-256color) false "\007")
    (Desktop (xterm-256color) false "\027]9;prigh: done in proj: a;b c\007")
    (Desktop (foot) false "\027]777;notify;prigh: done in proj;a b c\027\\")
    (Desktop
      (tmux-256color)
      true
      "\027Ptmux;\027\027]9;prigh: done in proj: a;b c\007\027\\")
    (Bell (tmux-256color) true "\007")
    (Off () false "")
    |}]
;;
