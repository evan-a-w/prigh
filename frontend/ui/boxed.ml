open! Core

let span ~style text = { Content.Span.text; style }

let pad_line_to (line : Content.Line.t) ~width =
  let w = Content.Line.width line in
  if w >= width
  then Content.Line.truncate line ~width
  else
    line
    @ [ { Content.Span.text = String.make (width - w) ' '; style = Style.plain }
      ]
;;

let inner_width ~width = Int.max 1 (width - 4)
let rule n = String.concat (List.init (Int.max 0 n) ~f:(fun _ -> "─"))

let render ?(border = Style.fg Gray) ?footer ~title ~width (body : Content.t)
  : Content.t
  =
  let inner = inner_width ~width in
  let title = Text_width.truncate title ~width:(Int.max 1 (width - 6)) in
  let title_w = Text_width.string title in
  let top : Content.Line.t =
    [ span ~style:border "┌─ "
    ; span ~style:(Style.bold Style.plain) title
    ; span ~style:border (" " ^ rule (width - 5 - title_w))
    ; span ~style:border "┐"
    ]
  in
  let rows =
    List.map body ~f:(fun line ->
      let line =
        pad_line_to (Content.Line.truncate line ~width:inner) ~width:inner
      in
      (span ~style:border "│ " :: line) @ [ span ~style:border " │" ])
  in
  let bottom : Content.Line.t =
    match footer with
    | Some footer when Text_width.string footer + 6 <= width ->
      let footer_w = Text_width.string footer in
      [ span ~style:border ("└" ^ rule (width - 5 - footer_w) ^ " ")
      ; span ~style:(Style.dim Style.plain) footer
      ; span ~style:border " ─┘"
      ]
    | _ -> [ span ~style:border ("└" ^ rule (width - 2) ^ "┘") ]
  in
  (top :: rows) @ [ bottom ]
;;
