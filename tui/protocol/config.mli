open! Core

(** TUI mirror of the backend's persisted configuration. *)

type t =
  { scoped_models : string list
  ; confirm_tools : bool
  ; default_model : string option
  ; default_thinking : string option
  ; fallback_models : string list
    (** model keys that take over, in order, when a model's usage runs out *)
  ; default_cwd : string option (** where new sessions start *)
  ; archive_after_days : float
    (** sessions inactive this long show as archived; [0.]: never *)
  }
[@@deriving sexp_of, equal]

val default : t
val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t
