open! Core

type t =
  | Ask
  | Auto_edit
  | Auto
[@@deriving sexp_of, equal, enumerate]

let to_string = function
  | Ask -> "ask"
  | Auto_edit -> "auto-edit"
  | Auto -> "auto"
;;

let of_string s =
  match String.lowercase (String.strip s) with
  | "ask" -> Some Ask
  | "auto-edit" | "auto_edit" -> Some Auto_edit
  | "auto" -> Some Auto
  | _ -> None
;;

let of_json j =
  Or_error.bind (Json.to_string_or_error j) ~f:(fun s ->
    match of_string s with
    | Some t -> Ok t
    | None -> Or_error.errorf "unknown approval mode %S" s)
;;
