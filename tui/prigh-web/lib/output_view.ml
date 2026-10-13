open! Core
open! Import

let trim text = String.rstrip ~drop:(Char.equal '\n') text

let count_lines text =
  let rec go pos n =
    match String.index_from text pos '\n' with
    | Some i -> go (i + 1) (n + 1)
    | None -> n
  in
  if String.is_empty text then 0 else go 0 1
;;

let line_count text = count_lines (trim text)

(* The first [n] lines of [text], and the last [n]: without splitting it all,
   since an output can be long. *)
let head_lines text n =
  let rec go pos n =
    if n = 0
    then String.prefix text (pos - 1)
    else (
      match String.index_from text pos '\n' with
      | Some i -> go (i + 1) (n - 1)
      | None -> text)
  in
  if n = 0 then "" else go 0 n
;;

let tail_lines text n =
  let rec go pos n =
    if n = 0
    then String.drop_prefix text (pos + 2)
    else if pos < 0
    then text
    else (
      match String.rindex_from text pos '\n' with
      | Some i -> go (i - 1) (n - 1)
      | None -> text)
  in
  if n = 0 then "" else go (String.length text - 1) n
;;

let pre text =
  if String.is_empty text then Node.none else Node.pre [ Node.text text ]
;;

let note cls text = Node.div ~attrs:[ Attr.class_ cls ] [ Node.text text ]

let live ~lines ~missed ~tail text =
  let total =
    if String.is_empty text || String.is_suffix text ~suffix:"\n"
    then lines
    else lines + 1
  in
  let shown = tail_lines (trim text) tail in
  let hidden = Int.max 0 (total - count_lines shown) in
  Chat_html.div
    (if hidden > 0 then "output live clipped from-end" else "output live")
    [ (if hidden > 0
       then note "earlier" (Chat_html.plural hidden "earlier line")
       else Node.none)
    ; (if missed
       then
         note
           "missed"
           "Output from while this page was away shows when it ends."
       else Node.none)
    ; pre shown
    ]
;;

let load_attribute = "data-load-output"

(* A cut output: its first lines and its last, the rest fetched when it is
   shown whole. *)
let elided ~error ~from_end ~fold:(fold, key) ~lines (e : Elided.t) text =
  let head, tail = Elided.split e text in
  let head = trim head
  and tail = trim tail in
  let classes, body =
    if Fold.expanded fold key
    then (
      let hidden = Int.max 0 (e.lines - count_lines head - count_lines tail) in
      ( [ "expanded"; "elided" ]
      , [ pre head
        ; Node.div
            ~attrs:[ Attr.class_ "gap" ]
            [ Node.text (Chat_html.plural hidden "more line")
            ; Node.button
                ~attrs:
                  [ Attr.class_ "load-output"
                  ; Attr.type_ "button"
                  ; Attr.create load_attribute key
                  ]
                [ Node.text "Load them" ]
            ]
        ; pre tail
        ; Fold.button fold key ~total:e.lines ~shown:lines
        ] ))
    else
      ( [ "clipped"; (if from_end then "from-end" else "from-start") ]
      , [ pre
            (if from_end then tail_lines tail lines else head_lines head lines)
        ; Fold.button fold key ~total:e.lines ~shown:lines
        ] )
  in
  Chat_html.div
    (String.concat
       ~sep:" "
       ("output" :: ((if error then [ "error" ] else []) @ classes)))
    body
;;

let plain ~error ~from_end ~fold:(fold, key) ~lines text =
  let text = trim text in
  let total = count_lines text in
  let classes, body =
    if total <= lines + 1
    then [], [ pre text ]
    else if Fold.expanded fold key
    then [ "expanded" ], [ pre text; Fold.button fold key ~total ~shown:lines ]
    else
      ( [ "clipped"; (if from_end then "from-end" else "from-start") ]
      , [ pre
            (if from_end then tail_lines text lines else head_lines text lines)
        ; Fold.button fold key ~total ~shown:lines
        ] )
  in
  Chat_html.div
    (String.concat
       ~sep:" "
       ("output" :: ((if error then [ "error" ] else []) @ classes)))
    body
;;

let view ?(error = false) ?(from_end = error) ?elided:cut ~fold ~lines text =
  match cut with
  | Some e -> elided ~error ~from_end ~fold ~lines e text
  | None -> plain ~error ~from_end ~fold ~lines text
;;
