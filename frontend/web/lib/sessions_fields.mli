open! Core
open! Import

(** The settings page's number fields for the session list: what is typed
    in each until it is applied (Enter, or leaving the field), and why it
    was refused. *)

module Which : sig
  type t =
    | Worktree_hours (** {!Worktree_folds.hours} *)
    | Archive_days (** the config's [archive_after_days] *)
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of, equal]

val empty : t

(** Typed, not yet applied. *)
val draft : t -> Which.t -> string option

(** Said under the field. *)
val error : t -> Which.t -> string option

val input : t -> Which.t -> string -> t

(** Applies the draft: the number to set ([0]: off), if it is one; an empty
    or untouched field sets nothing. *)
val commit : t -> Which.t -> t * float option

(** The fields with a draft. *)
val drafted : t -> Which.t list
