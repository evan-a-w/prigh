open! Core

type t =
  { root : string
  ; against : string
  ; diff : string
  ; truncated : bool
  ; branch : string option
  ; base : string
  ; main : bool
  ; branches : string list
  }
[@@deriving sexp_of, equal]

let params ?path ?base ?dir () =
  List.filter_opt
    [ Option.map path ~f:(fun p -> "path", Json.str p)
    ; Option.map base ~f:(fun b -> "base", Json.str b)
    ; Option.map dir ~f:(fun d -> "dir", Json.str d)
    ]
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind root = Json.string_field j "root" in
  let%bind against = Json.string_field j "against" in
  let%bind diff = Json.string_field j "diff" in
  let%bind truncated = Json.bool_default_field j "truncated" ~default:false in
  let%bind branch = Json.string_opt_field j "branch" in
  let%bind base = Json.string_opt_field j "base" in
  let%bind main = Json.bool_default_field j "main" ~default:true in
  let%map branches =
    Json.optional_list_field j "branches" ~f:(function
      | `String s -> Ok s
      | _ -> Or_error.error_string "a branch is a string")
  in
  { root
  ; against
  ; diff
  ; truncated
  ; branch
  ; base = Option.value base ~default:"HEAD"
  ; main
  ; branches
  }
;;
