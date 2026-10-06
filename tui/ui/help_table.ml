open! Core

let render rows =
  let width =
    List.fold rows ~init:0 ~f:(fun acc (k, _) ->
      Int.max acc (Text_width.string k))
  in
  List.map rows ~f:(fun (k, help) ->
    [ { Content.Span.text = Text_width.pad_right k ~width
      ; style = Style.bold Style.plain
      }
    ; { text = "  " ^ help; style = Style.plain }
    ])
;;
