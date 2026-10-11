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

let live ~lines ~tail text =
  let total = if String.is_suffix text ~suffix:"\n" then lines else lines + 1 in
  let shown = tail_lines (trim text) tail in
  let hidden = Int.max 0 (total - count_lines shown) in
  Chat_html.div
    "output live"
    [ (if hidden > 0
       then note "earlier" (Chat_html.plural hidden "earlier line")
       else Node.none)
    ; pre shown
    ]
;;

let view ?(error = false) ~fold:(fold, key) ~head ~tail text =
  let text = trim text in
  let total = count_lines text in
  let body, expanded =
    if total <= head + tail + 1
    then [ pre text ], false
    else if Fold.expanded fold key
    then [ pre text; Fold.button fold key ~total ~shown:(head + tail) ], true
    else
      ( [ pre (head_lines text head)
        ; Fold.button fold key ~total ~shown:(head + tail)
        ; pre (tail_lines text tail)
        ]
      , false )
  in
  Chat_html.div
    (String.concat
       ~sep:" "
       (List.filter_opt
          [ Some "output"
          ; Option.some_if error "error"
          ; Option.some_if expanded "expanded"
          ]))
    body
;;
