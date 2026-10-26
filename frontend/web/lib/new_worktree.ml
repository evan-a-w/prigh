open! Core
open! Import

module Field = struct
  type t =
    | Branch
    | Base
  [@@deriving sexp_of, equal]

  let id = function
    | Branch -> "worktree-branch"
    | Base -> "worktree-base"
  ;;
end

type t =
  { branch : string
  ; base : string
  ; head : string option
  ; project_dir : string option
  ; repo : string option
  ; busy : bool
  ; error : string option
  }
[@@deriving sexp_of, equal]

let create ?repo ~head ~project_dir () =
  { branch = ""
  ; base = ""
  ; head
  ; project_dir
  ; repo
  ; busy = false
  ; error = None
  }
;;

let set t (field : Field.t) text =
  match field with
  | Branch -> { t with branch = text; error = None }
  | Base -> { t with base = text; error = None }
;;

let request t =
  match String.strip t.branch, String.strip t.base with
  | "", _ -> Error "Type a branch name (Esc cancels)."
  | branch, _ when String.exists branch ~f:Char.is_whitespace ->
    Error "A branch name has no spaces: try one-word names like fix/login."
  | branch, "" -> Ok (branch, None)
  | branch, base -> Ok (branch, Some base)
;;

let command ~branch ~base =
  String.concat ~sep:" " ([ "/worktree new"; branch ] @ Option.to_list base)
;;
