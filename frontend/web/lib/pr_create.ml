open! Core
open! Import

module Action = struct
  type t =
    | Title of string
    | Body of string
    | Base of string
    | Toggle_draft
    | Toggle_push
  [@@deriving sexp_of, equal]
end

type t =
  { defaults : Pr_defaults.t option
  ; failed : string option
  ; title : string
  ; body : string
  ; base : string
  ; draft : bool
  ; push : bool
  ; busy : bool
  ; error : string option
  ; back : Pull_requests.t option
  }
[@@deriving sexp_of, equal]

let create ?back ~title () =
  { defaults = None
  ; failed = None
  ; title
  ; body = ""
  ; base = ""
  ; draft = false
  ; push = false
  ; busy = false
  ; error = None
  ; back
  }
;;

let set_defaults t (d : Pr_defaults.t) =
  let keep typed default = if String.is_empty typed then default else typed in
  { t with
    defaults = Some d
  ; title = keep t.title d.title
  ; body = keep t.body d.body
  ; base = keep t.base d.base
  ; push = not d.pushed
  }
;;

let update t (action : Action.t) =
  let t = { t with error = None } in
  match action with
  | Title title -> { t with title }
  | Body body -> { t with body }
  | Base base -> { t with base }
  | Toggle_draft -> { t with draft = not t.draft }
  | Toggle_push -> { t with push = not t.push }
;;

let request t : (Request.Method.t, string) Result.t =
  match t.defaults, String.strip t.title with
  | None, _ -> Error "Wait for the branch's details to load."
  | Some _, "" -> Error "Give the pull request a title."
  | Some d, title ->
    let base = String.strip t.base in
    Ok
      (Pr_create
         { title
         ; body = t.body
         ; base = (if String.is_empty base then None else Some base)
         ; head = Some d.head
         ; draft = t.draft
         ; push = t.push
         })
;;
