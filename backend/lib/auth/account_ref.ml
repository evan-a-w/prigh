open! Core
open! Import

type t =
  | Own of Account_id.t
  | Shared of
      { owner : string
      ; id : Account_id.t
      }
[@@deriving sexp, equal, compare]

let main = Own Account_id.main

let to_string = function
  | Own id -> Account_id.to_string id
  | Shared { owner; id } -> sprintf "~%s/%s" owner (Account_id.to_string id)
;;

let of_string s =
  match String.chop_prefix s ~prefix:"~" with
  | None -> Or_error.map (Account_id.of_string s) ~f:(fun id -> Own id)
  | Some rest ->
    (match String.lsplit2 rest ~on:'/' with
     | Some (owner, id) when not (String.is_empty owner) ->
       Or_error.map (Account_id.of_string id) ~f:(fun id ->
         Shared { owner; id })
     | _ ->
       Or_error.errorf
         "%S is not a shared login: write ~OWNER/ACCOUNT (as auth_status lists \
          it)"
         s)
;;

let owner = function
  | Own _ -> None
  | Shared { owner; _ } -> Some owner
;;

let same written account =
  String.equal
    (Option.value written ~default:(to_string main))
    (to_string (Option.value account ~default:main))
;;
