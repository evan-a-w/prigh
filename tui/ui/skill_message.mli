open! Core

(** The backend expands [/skill:NAME ARGS] into a user message
    [<skill name="NAME" location="PATH">\nBODY\n</skill>], followed by
    [\n\nARGS] when there are arguments. *)

type t =
  { name : string
  ; location : string (** the skill's [SKILL.md] *)
  ; body : string
  ; args : string
  }
[@@deriving sexp_of, equal]

(** [None] unless [text] is an expanded skill invocation. *)
val parse : string -> t option

(** What the user typed: [/skill:NAME ARGS]. *)
val invocation : t -> string
