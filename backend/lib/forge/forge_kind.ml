open! Core

type t =
  | Github
  | Forgejo
[@@deriving sexp, equal, compare, enumerate]

let to_string = function
  | Github -> "github"
  | Forgejo -> "forgejo"
;;

let of_string = function
  | "github" -> Ok Github
  | "forgejo" | "gitea" -> Ok Forgejo
  | other ->
    Or_error.errorf "unknown git host kind %S (github or forgejo)" other
;;

let label = function
  | Github -> "GitHub"
  | Forgejo -> "Forgejo"
;;
