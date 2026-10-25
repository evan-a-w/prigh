open! Core
open! Import

type 'a t =
  | Empty
  | Loading of string
  | Loaded of
      { key : string
      ; value : 'a
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
  | Loaded { key = k; value } when String.equal k key -> Some value
  | Empty | Loading _ | Loaded _ -> None
;;

let request t ~key =
  match t with
  | (Loading k | Loaded { key = k; _ }) when String.equal k key -> None
  | Empty | Loading _ | Loaded _ -> Some (Loading key)
;;

let set t ~key value =
  match t with
  | Loading k when String.equal k key -> Loaded { key; value }
  | Empty | Loading _ | Loaded _ -> t
;;

let put _ ~key value = Loaded { key; value }
