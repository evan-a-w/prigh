open! Core
open! Import

(** When a session's tools need the user's approval (see {!Approval}):
    [Ask] holds every destructive tool for confirmation, [Auto_edit] runs
    file edits ([edit], [write], [apply_patch]) without asking but still
    asks for everything else destructive ([bash], MCP tools), [Auto] asks
    for nothing. Rules ({!Approval_rule}) refine every mode. *)
type t =
  | Ask
  | Auto_edit
  | Auto
[@@deriving sexp, equal, compare, enumerate]

(** ["ask"], ["auto-edit"], ["auto"]. *)
val to_string : t -> string

(** Also accepts ["auto_edit"] and is case-insensitive; the error lists the
    modes. *)
val of_string : string -> t Or_error.t

(** What it means, for help texts and notices. *)
val describe : t -> string
