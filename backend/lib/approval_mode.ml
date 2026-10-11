open! Core
open! Import

type t =
  | Ask
  | Auto_edit
  | Auto
[@@deriving sexp, equal, compare, enumerate]

let to_string = function
  | Ask -> "ask"
  | Auto_edit -> "auto-edit"
  | Auto -> "auto"
;;

let of_string s =
  match String.lowercase (String.strip s) with
  | "ask" -> Ok Ask
  | "auto-edit" | "auto_edit" -> Ok Auto_edit
  | "auto" -> Ok Auto
  | _ ->
    Or_error.errorf
      "unknown approval mode %S; the modes are %s"
      s
      (String.concat ~sep:", " (List.map all ~f:to_string))
;;

let describe = function
  | Ask -> "destructive tools ask first"
  | Auto_edit -> "file edits run without asking; bash and MCP tools ask"
  | Auto -> "tools run without asking"
;;
