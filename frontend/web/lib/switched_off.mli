open! Core
open! Import

(** The settings page's numbers behind an On/Off switch (Show worktrees,
    Archive after, Compact at): what each was when switched off, so that
    switching it on again restores that, else its default. *)

module Key : sig
  type t =
    | Worktree_hours
    | Archive_days
    | Compact_at
  [@@deriving sexp_of, equal, compare]
end

type t [@@deriving sexp_of, equal]

val empty : t

(** [key] was [value] (when above [0]) until switched off. *)
val remember : t -> Key.t -> float -> t

(** What switching [key] on sets: what it was, else [default]. *)
val value : t -> Key.t -> default:float -> float
