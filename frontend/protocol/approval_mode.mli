open! Core

(** When the session's tools ask first (the backend's [Approval_mode]):
    [Ask] for every destructive tool, [Auto_edit] for everything but file
    edits, [Smart] when the approval model (the config's [approval_model],
    else the session's) judges a call not safe to run without the user,
    [Auto] never. *)
type t =
  | Ask
  | Auto_edit
  | Smart
  | Auto
[@@deriving sexp_of, equal, enumerate]

(** ["ask"], ["auto-edit"], ["smart"], ["auto"]: the wire form, also what
    users type. *)
val to_string : t -> string

val of_string : string -> t option
val of_json : Json.t -> t Or_error.t
