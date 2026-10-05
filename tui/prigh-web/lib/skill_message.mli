open! Core

(** A user message that invoked a skill: the backend expands [/skill:NAME ARGS]
    into [<skill name="NAME" location="PATH">BODY</skill>] followed, after a
    blank line, by [ARGS]. *)

type t =
  { name : string
  ; location : string (** the skill's SKILL.md *)
  ; body : string (** what the model was given, without the [<skill>] tags *)
  ; args : string
  }
[@@deriving sexp_of, equal]

val parse : string -> t option

(** What the user typed: [/skill:NAME ARGS]. *)
val invocation : t -> string
