open! Core

type t =
  { gen_id : string
  ; amend : bool
  ; text : string
  }
[@@deriving sexp_of, equal]

let create ~gen_id ~amend = { gen_id; amend; text = "" }
let add_delta t delta = { t with text = t.text ^ delta }
let dim = Style.dim Style.plain

let render t ~width ~max_rows =
  let inner = Boxed.inner_width ~width in
  let text =
    if String.is_empty (String.strip t.text)
    then []
    else Content.wrap (Content.lines (String.strip t.text)) ~width:inner
  in
  let footer =
    [ Content.Line.of_string
        ~style:dim
        "drafting from the staged changes… · Esc stops"
    ]
  in
  let room = Int.max 1 (max_rows - 2 - List.length footer) in
  let n = List.length text in
  let text =
    if n <= room
    then text
    else
      Content.Line.of_string
        ~style:dim
        (sprintf "… %d lines above" (n - room + 1))
      :: List.drop text (n - room + 1)
  in
  Boxed.render
    ~border:(Style.fg Cyan)
    ~title:(if t.amend then "amended commit message" else "commit message")
    ~width
    (text @ footer)
;;
