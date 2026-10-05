open! Core
open! Import

let lines text = String.split_lines (String.rstrip ~drop:(Char.equal '\n') text)
let line_count text = List.length (lines text)

let pre lines =
  match lines with
  | [] -> Node.none
  | lines -> Node.pre [ Node.text (String.concat ~sep:"\n" lines) ]
;;

let view ?(error = false) ~head ~tail text =
  let lines = lines text in
  let count = List.length lines in
  let body =
    if count <= head + tail + 1
    then [ pre lines ]
    else (
      let hidden = count - head - tail in
      [ pre (List.take lines head)
      ; Node.details
          ~attrs:[ Attr.class_ "more" ]
          [ Node.summary [ Node.text (Chat_html.plural hidden "more line") ]
          ; pre (List.sub lines ~pos:head ~len:hidden)
          ]
      ; pre (List.drop lines (head + hidden))
      ])
  in
  Chat_html.div (if error then "output error" else "output") body
;;
