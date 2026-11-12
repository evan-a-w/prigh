open! Core
open! Import

module Which = struct
  type t =
    | Dir
    | Budget
  [@@deriving sexp_of, equal]
end

module Field = struct
  type t =
    { draft : string option
    ; error : string option
    }
  [@@deriving sexp_of, equal]

  let empty = { draft = None; error = None }
end

type t =
  { dir : Field.t
  ; budget : Field.t
  }
[@@deriving sexp_of, equal]

let empty = { dir = Field.empty; budget = Field.empty }

let get t : Which.t -> Field.t = function
  | Dir -> t.dir
  | Budget -> t.budget
;;

let set t (which : Which.t) field =
  match which with
  | Dir -> { t with dir = field }
  | Budget -> { t with budget = field }
;;

let draft t which = (get t which).draft
let error t which = (get t which).error
let input t which text = set t which { draft = Some text; error = None }
let refuse t which ~draft e = set t which { draft = Some draft; error = Some e }
let clear t which = set t which Field.empty

let commit t which =
  match (get t which).draft with
  | None -> t, None
  | Some text -> set t which Field.empty, Some (String.strip text)
;;

let drafted t =
  List.filter [ Which.Dir; Budget ] ~f:(fun w -> Option.is_some (draft t w))
;;
