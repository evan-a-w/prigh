open! Core
open! Import

(** Which old generations of the users' Nix profiles a garbage collection
    deletes first. The current generation of every profile is always kept. *)
type t =
  | Older_than_days of int (** at least 1 *)
  | All (** every generation but the current one *)
[@@deriving sexp_of, equal]

(** A request's [delete_generations]: absent or [null] (none), a whole number
    of days ([0] is [All]) or ["all"]. *)
val of_json : Json.t option -> t option Or_error.t

val to_json : t option -> Json.t

(** The container helper's form: [keep], [all] or [Nd]. *)
val to_arg : t option -> string

val of_arg : string -> t option Or_error.t
