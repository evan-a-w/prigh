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

let render ?(border = Style.fg Gray) ~title ~width (body : Content.t)
  : Content.t
  =
  let inner = inner_width ~width in
  let title = Text_width.truncate title ~width:(Int.max 1 (width - 6)) in
  let title_w = Text_width.string title in
  let fill = Int.max 0 (width - 5 - title_w) in
  let top : Content.Line.t =
    [ span ~style:border "┌─ "
    ; span ~style:(Style.bold Style.plain) title
    ; span ~style:border " "
    ; span ~style:border (String.concat (List.init fill ~f:(fun _ -> "─")))
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
    [ span ~style:border "└"
    ; span
        ~style:border
        (String.concat (List.init (Int.max 0 (width - 2)) ~f:(fun _ -> "─")))
    ; span ~style:border "┘"
    ]
  in
  (top :: rows) @ [ bottom ]
;;
