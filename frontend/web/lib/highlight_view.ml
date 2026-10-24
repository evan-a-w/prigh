open! Core
open! Import

let spans tokens =
  List.map tokens ~f:(fun ({ kind; text } : Highlight.Token.t) ->
    match kind with
    | Plain -> Node.text text
    | kind ->
      Node.span
        ~attrs:[ Attr.class_ (Highlight.Kind.css kind) ]
        [ Node.text text ])
;;

let default_limit = 64 * 1024

let code ?(limit = default_limit) lang text =
  match lang with
  | None -> [ Node.text text ]
  | Some lang ->
    if String.length text <= limit
    then spans (Highlight.tokens lang text)
    else (
      let cut =
        match String.index_from text limit '\n' with
        | Some k -> k
        | None -> String.length text
      in
      spans (Highlight.tokens lang (String.prefix text cut))
      @ [ Node.text (String.drop_prefix text cut) ])
;;
