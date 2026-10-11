open! Core
open! Import

(** The sidebar's selection mode: which sessions are marked (by path) for
    deleting several at once. A shift-click marks the range from the last
    session clicked. *)

type t =
  { selecting : bool
  ; marked : String.Set.t (** paths *)
  ; anchor : string option (** the last path clicked, for ranges *)
  }
[@@deriving sexp_of, equal]

val empty : t
val start : t

(** Marks or unmarks [path]; with [range], marks every path from the anchor
    to [path] in [order] (the sidebar's order) instead. *)
val click : t -> path:string -> range:bool -> order:string list -> t

(** Forgets marks of sessions that are gone. *)
val prune : t -> paths:string list -> t

(** The marked sessions, in the list's order. *)
val marked : t -> Session_summary.t list -> Session_summary.t list
