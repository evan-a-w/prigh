open! Core

(** Where a session's tools run: the backend or a connected client. *)
type t =
  { id : string
  ; name : string
  ; cwd : string
  }
[@@deriving sexp_of, equal]

val backend_id : string
val of_json : Json.t -> t Or_error.t
