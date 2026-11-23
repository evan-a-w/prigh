open! Core
open! Import

(** Claude Code's permission modes, as its [--permission-mode] flag and
    [set_permission_mode] control request name them. *)
type t =
  | Default (** ask before anything not allowed by a rule *)
  | Accept_edits (** file edits run without asking *)
  | Plan (** read-only; the model proposes a plan with [ExitPlanMode] *)
  | Bypass_permissions (** nothing asks *)
  | Dont_ask (** deny whatever is not allowed by a rule, never ask *)
  | Auto (** Claude Code's classifier decides *)
[@@deriving sexp_of, equal, enumerate]

(** ["default"], ["acceptEdits"], ["plan"], ["bypassPermissions"],
    ["dontAsk"], ["auto"]. *)
val to_string : t -> string

val of_string : string -> t option
