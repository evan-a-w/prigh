open! Core

module Status = struct
  type t =
    | Streaming
    | Done
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; question : string
  ; answer : string
  ; status : Status.t
  }
[@@deriving sexp_of, equal]

let create ~id ~question = { id; question; answer = ""; status = Streaming }

let is_streaming t =
  match t.status with
  | Streaming -> true
  | Done | Failed _ -> false
;;

let add_delta t delta = { t with answer = t.answer ^ delta }
let finish t ~text = { t with answer = text; status = Done }
let fail t error = { t with status = Failed error }
let border = Style.fg Magenta
let dim = Style.dim Style.plain
let max_rows ~height = Int.max 5 (height / 2)

let render t ~width ~max_rows =
  let inner = Boxed.inner_width ~width in
  let question =
    Content.Line.wrap
      [ { Content.Span.text = "? "; style = Style.bold border }
      ; { text = String.concat ~sep:" " (String.split_lines t.question)
        ; style = Style.bold Style.plain
        }
      ]
      ~width:inner
  in
  let answer =
    if String.is_empty (String.strip t.answer)
    then []
    else Content.wrap (Markdown.render ~width:inner t.answer) ~width:inner
  in
  let footer : Content.t =
    match t.status with
    | Streaming ->
      [ Content.Line.of_string ~style:dim "answering… · Esc to dismiss" ]
    | Done -> [ Content.Line.of_string ~style:dim "Esc to dismiss" ]
    | Failed error ->
      Content.Line.wrap
        (Content.Line.of_string ~style:(Style.fg Red) ("error: " ^ error))
        ~width:inner
      @ [ Content.Line.of_string ~style:dim "Esc to dismiss" ]
  in
  let room =
    Int.max 1 (max_rows - 2 - List.length question - List.length footer)
  in
  let answer =
    let n = List.length answer in
    if n <= room
    then answer
    else
      Content.Line.of_string
        ~style:dim
        (sprintf "… %d lines above" (n - room + 1))
      :: List.drop answer (n - room + 1)
  in
  Boxed.render ~border ~title:"btw" ~width (question @ answer @ footer)
;;
