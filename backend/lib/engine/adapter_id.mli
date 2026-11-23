open! Core
open! Import

(** Which external coding agent a composer session drives: Codex
    ([codex app-server]), Claude Code ([claude], stream-json), an agent
    speaking ACP (named by its command, [acp:<name>]), or prigh's own harness
    ([prigh]). *)
type t =
  | Codex
  | Claude
  | Acp of string
  | Prigh
[@@deriving sexp, equal, compare]

(** [codex], [claude], [acp:<name>], [prigh]. *)
val to_string : t -> string

(** The inverse of [to_string]; an error lists what is known. *)
val of_string : string -> t Or_error.t

(** For people: [Codex], [Claude Code], [<name> (ACP)], [prigh]. *)
val label : t -> string
