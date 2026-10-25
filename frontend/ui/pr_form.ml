open! Core
open! Import

module Field = struct
  type t =
    | Title
    | Body
    | Base
    | Draft
    | Push
  [@@deriving sexp_of, equal, enumerate]
end

type t =
  { defaults : P.Pr_defaults.t
  ; focus : Field.t
  ; title : string
  ; body : string
  ; base : string
  ; draft : bool
  ; push : bool
  ; error : string option
  ; submitting : bool
  }
[@@deriving sexp_of, equal, fields ~getters]

let create ?title (defaults : P.Pr_defaults.t) =
  { defaults
  ; focus = Title
  ; title = Option.value title ~default:defaults.title
  ; body = defaults.body
  ; base = defaults.base
  ; draft = false
  ; push = not defaults.pushed
  ; error = None
  ; submitting = false
  }
;;

let edit t ~f =
  if t.submitting
  then t
  else (
    match t.focus with
    | Title -> { t with title = f t.title }
    | Body -> { t with body = f t.body }
    | Base -> { t with base = f t.base }
    | Draft | Push -> t)
;;

let toggle t =
  if t.submitting
  then t
  else (
    match t.focus with
    | Draft -> { t with draft = not t.draft }
    | Push -> { t with push = not t.push }
    | Title | Body | Base -> t)
;;

let insert t text =
  match t.focus with
  | Draft | Push -> if String.equal text " " then toggle t else t
  | Body -> edit t ~f:(fun field -> field ^ text)
  | Title | Base ->
    let text =
      String.map text ~f:(fun c -> if Char.equal c '\n' then ' ' else c)
    in
    edit t ~f:(fun field -> field ^ text)
;;

let newline t =
  match t.focus with
  | Body -> edit t ~f:(fun body -> body ^ "\n")
  | Title | Base | Draft | Push -> t
;;

let backspace t =
  edit t ~f:(fun field ->
    String.concat
      (List.map
         (List.drop_last (Text_width.uchars field) |> Option.value ~default:[])
         ~f:fst))
;;

let clear t = edit t ~f:(fun _ -> "")

let kill_word t =
  edit t ~f:(fun field ->
    let trimmed = String.rstrip field in
    match String.rindex trimmed ' ', String.rindex trimmed '\n' with
    | None, None -> ""
    | a, b ->
      let i =
        Int.max (Option.value a ~default:(-1)) (Option.value b ~default:(-1))
      in
      String.prefix trimmed (i + 1))
;;

let move t ~by =
  let n = List.length Field.all in
  let index, _ =
    List.findi_exn Field.all ~f:(fun _ f -> Field.equal f t.focus)
  in
  { t with focus = List.nth_exn Field.all ((index + by + n) % n) }
;;

let next t = move t ~by:1
let prev t = move t ~by:(-1)

let submit t =
  if t.submitting
  then `Wait t
  else (
    let title = String.strip t.title in
    if String.is_empty title
    then `Wait { t with focus = Title; error = Some "type a title" }
    else (
      let base = String.strip t.base in
      `Create
        ( { t with submitting = true; error = None }
        , P.Request.Method.Pr_create
            { title
            ; body = t.body
            ; base = Option.some_if (not (String.is_empty base)) base
            ; head = Some t.defaults.head
            ; draft = t.draft
            ; push = t.push
            } )))
;;

let failed t ~error = { t with submitting = false; error = Some error }
