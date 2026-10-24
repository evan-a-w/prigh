open! Core

(** An agent skill, as [list_skills] lists it. *)
type t =
  { name : string
  ; description : string
  ; path : string (** its [SKILL.md] *)
  ; model_invocable : bool
    (** [false] when only the user may invoke it ([/skill:NAME]) *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
