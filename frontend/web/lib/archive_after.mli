open! Core
open! Import

(** The settings page's "Archive after" field: what is typed there until it
    is applied (Enter, or leaving the field), and why it was refused. *)

type t [@@deriving sexp_of, equal]

val empty : t

(** Typed, not yet applied. *)
val draft : t -> string option

(** Said under the field. *)
val error : t -> string option

val input : t -> string -> t

(** Applies the draft: the days to set (the config's [archive_after_days];
    [0]: off), if it is one; an empty or untouched field sets nothing. *)
val commit : t -> t * float option
