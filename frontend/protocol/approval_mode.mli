open! Core

(** When the session's tools ask first (the backend's [Approval_mode]):
    [Ask] for every destructive tool, [Auto_edit] for everything but file
    edits, [Auto] never. *)
type t =
  | Ask
  | Auto_edit
  | Auto
[@@deriving sexp_of, equal, enumerate]

(** ["ask"], ["auto-edit"], ["auto"]: the wire form, also what users type. *)
val to_string : t -> string

val of_string : string -> t option
val of_json : Json.t -> t Or_error.t
