open! Core
open! Import

type t =
  | Web
  | Tui
  | Acp
  | Other
[@@deriving sexp, jsonaf, equal, enumerate]

let to_string = function
  | Web -> "web"
  | Tui -> "tui"
  | Acp -> "acp"
  | Other -> "other"
;;

let of_hello = function
  | None -> Other
  | Some s ->
    List.find all ~f:(fun t -> String.equal (to_string t) s)
    |> Option.value ~default:Other
;;

let describe = function
  | Web -> "prigh-web"
  | Tui -> "prigh-tui"
  | Acp -> "an editor (ACP)"
  | Other -> "another client"
;;

let shows_components = function
  | Web -> true
  | Tui | Acp | Other -> false
;;
