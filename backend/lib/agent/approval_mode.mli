open! Core
open! Import

(** When a session's tools need the user's approval (see {!Approval}):
    [Ask] holds every destructive tool for confirmation, [Auto_edit] runs
    file edits ([edit], [write], [apply_patch]) without asking but still
    asks for everything else destructive ([bash], MCP tools), [Auto] asks
    for nothing, and [Smart] asks the approval model ({!Approval_judge})
    about each call [Ask] would hold: the calls it judges safe run, the
    others ask the user. Rules ({!Approval_rule}) refine every mode. *)
type t =
  | Ask
  | Auto_edit
  | Auto
  | Smart
[@@deriving sexp, equal, compare, enumerate]

(** ["ask"], ["auto-edit"], ["auto"], ["smart"]. *)
val to_string : t -> string

(** Also accepts ["auto_edit"] and is case-insensitive; the error lists the
    modes. *)
val of_string : string -> t Or_error.t
