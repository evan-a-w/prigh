open! Core

(** Which profile generations a Nix garbage collection deletes, besides the
    unreferenced store paths ([nix_store_gc]'s [delete_generations]):
    [None] where a [t option] is taken keeps them all. Every user's current
    generation is always kept. *)
type t =
  | Older_than_days of int (** at least 1 *)
  | All (** all but the current one ([0] days) *)
[@@deriving sexp_of, equal]

(** The most days the backend accepts. *)
val max_days : int

(** [0] is [All]; negative or more than [max_days] is an error. *)
val of_days : int -> t Or_error.t

(** [null], a number of days or ["all"]. *)
val of_json : Json.t option -> t option Or_error.t

val to_json : t option -> Json.t
