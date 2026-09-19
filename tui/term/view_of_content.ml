open! Core
open Bonsai_term
module Style = Prigh_ui.Style

let color (c : Style.Color.t) =
  match c with
  | Default -> None
  | Red -> Some Attr.Color.Expert.red
  | Green -> Some Attr.Color.Expert.green
  | Yellow -> Some Attr.Color.Expert.yellow
  | Blue -> Some Attr.Color.Expert.blue
  | Magenta -> Some Attr.Color.Expert.magenta
  | Cyan -> Some Attr.Color.Expert.cyan
  | Gray -> Some Attr.Color.Expert.lightblack
  | White -> Some Attr.Color.Expert.white
;;

let attr (s : Style.t) =
  let fg =
    if s.dim && Style.Color.equal s.fg Default
    then Some Attr.Color.Expert.lightblack
    else color s.fg
  in
  List.filter_opt
    [ Option.map fg ~f:Attr.fg
    ; Option.some_if s.bold Attr.bold
    ; Option.some_if s.italic Attr.italic
    ; Option.some_if s.underline Attr.underline
    ; Option.some_if s.invert Attr.invert
    ]
;;

let line (l : Prigh_ui.Content.Line.t) =
  match l with
  | [] -> View.text ""
  | spans ->
    View.hcat
      (List.map spans ~f:(fun s -> View.text ~attrs:(attr s.style) s.text))
;;

let screen (s : Prigh_ui.Screen.t) =
  View.vcat
    (List.map s.lines ~f:(fun l ->
       View.crop ~r:(Int.max 0 (View.width (line l) - s.width)) (line l)))
;;
