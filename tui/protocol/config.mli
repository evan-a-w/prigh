open! Core

(** TUI mirror of the backend's persisted configuration. *)

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
  ; default_thinking : string option
  }
[@@deriving sexp_of, equal]

val default : t
val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t
