open! Core
open! Import

(** Which engine does a session's agent work: the [Harness] (prigh itself
    talks to the model APIs and runs the tools) or the [Composer] (prigh
    drives an external coding agent, such as Codex or Claude Code). *)
type t =
  | Harness
  | Composer
[@@deriving sexp, equal, compare, enumerate]

(** [harness], [composer]. *)
val to_string : t -> string

val of_string : string -> t Or_error.t
