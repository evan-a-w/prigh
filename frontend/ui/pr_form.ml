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

module Drafting = struct
  type t =
    { gen_id : string
    ; text : string (** streamed so far *)
    ; title : bool (** the title too, not just the body *)
    }
  [@@deriving sexp_of, equal]
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
  ; drafting : Drafting.t option
  ; before_draft : (string * string) option
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
  ; drafting = None
  ; before_draft = None
  }
;;

let edit t ~f =
  if t.submitting
  then t
  else (
    match t.focus with
    | (Title | Body) when Option.is_some t.drafting -> t
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
  else if Option.is_some t.drafting
  then `Wait { t with error = Some "wait for the draft, or Esc stops it" }
  else (
    let title = String.strip t.title in
    if String.is_empty title
    then `Wait { t with focus = Title; error = Some "type a title" }
    else (
      let base = String.strip t.base in
      `Create
        ( { t with submitting = true; error = None }
        , P.Request.Method.Pr_create
            { dir = None
            ; title
            ; body = t.body
            ; base = Option.some_if (not (String.is_empty base)) base
            ; head = Some t.defaults.head
            ; draft = t.draft
            ; push = t.push
            } )))
;;

let failed t ~error = { t with submitting = false; error = Some error }
let generating t = Option.map t.drafting ~f:(fun d -> d.gen_id)
let can_undo t = Option.is_some t.before_draft && Option.is_none t.drafting

let generate ?(title = true) t ~gen_id =
  if t.submitting || Option.is_some t.drafting
  then None
  else (
    let base = String.strip t.base in
    Some
      ( { t with
          drafting = Some { gen_id; text = ""; title }
        ; before_draft = Some (t.title, t.body)
        ; error = None
        }
      , P.Request.Method.Generate_text
          { gen_id
          ; kind = Pr
          ; dir = None
          ; base = Option.some_if (not (String.is_empty base)) base
          ; task = None
          } ))
;;

let drafted t (d : Drafting.t) ~title ~body =
  { t with title = (if d.title then title else t.title); body }
;;

let with_drafting t ~gen_id ~f =
  match t.drafting with
  | Some d when String.equal d.gen_id gen_id -> f d
  | _ -> t
;;

let generated_delta t ~gen_id delta =
  with_drafting t ~gen_id ~f:(fun d ->
    let d = { d with text = d.text ^ delta } in
    let title, body = P.Generated.split_pr d.text in
    { (drafted t d ~title ~body) with drafting = Some d })
;;

let generated t (g : P.Generated.t) =
  with_drafting t ~gen_id:g.gen_id ~f:(fun d ->
    let split_title, split_body = P.Generated.split_pr g.text in
    { (drafted
         t
         d
         ~title:(Option.value g.title ~default:split_title)
         ~body:(Option.value g.body ~default:split_body))
      with
      drafting = None
    ; error = None
    })
;;

let generation_failed t ~gen_id ~error =
  with_drafting t ~gen_id ~f:(fun _ ->
    let title, body = Option.value t.before_draft ~default:(t.title, t.body) in
    { t with
      title
    ; body
    ; drafting = None
    ; before_draft = None
    ; error = Some ("could not draft it: " ^ error)
    })
;;

let stop t = { t with drafting = None; error = None }

let undo t =
  match t.before_draft with
  | Some (title, body) when Option.is_none t.drafting ->
    { t with title; body; before_draft = Some (t.title, t.body) }
  | _ -> t
;;
