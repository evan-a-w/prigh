open! Core
open! Import

type t =
  | Empty
  | Loading of string
  | Loaded of
      { key : string
      ; skills : P.Skill.t list
      }
[@@deriving sexp_of]

let empty = Empty

let key (state : P.State.t option) =
  match state with
  | None -> ""
  | Some s -> sprintf "%s %s:%s" s.session_id s.active_host s.cwd
;;

let find t ~key =
  match t with
  | Loaded { key = k; skills } when String.equal k key -> Some skills
  | Empty | Loading _ | Loaded _ -> None
;;

let request t ~key =
  match t with
  | (Loading k | Loaded { key = k; _ }) when String.equal k key -> None
  | Empty | Loading _ | Loaded _ -> Some (Loading key)
;;

let set t ~key skills =
  match t with
  | Loading k when String.equal k key -> Loaded { key; skills }
  | Empty | Loading _ | Loaded _ -> t
;;
