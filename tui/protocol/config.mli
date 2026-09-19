open! Core

(** TUI mirror of the backend's persisted configuration. *)

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t
