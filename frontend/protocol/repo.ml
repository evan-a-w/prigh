open! Core

type t =
  { root : string
  ; worktree : string
  ; branch : string option
  ; main : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind root = Json.string_field j "root" in
  let%bind worktree = Json.string_field j "worktree" in
  let%bind branch = Json.string_opt_field j "branch" in
  let%map main = Json.bool_default_field j "main" ~default:false in
  { root; worktree; branch; main }
;;
