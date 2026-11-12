open! Core
open! Import

(** The "New sessions" section's text fields: what is typed in each until it
    is applied (Enter, or leaving the field), and why it was refused. *)

module Which : sig
  type t =
    | Dir (** the config's [default_cwd] *)
    | Budget (** the config's [max_budget_usd] *)
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of, equal]

val empty : t

(** Typed, not yet applied. *)
val draft : t -> Which.t -> string option

(** Said under the field. *)
val error : t -> Which.t -> string option

val input : t -> Which.t -> string -> t

(** Refuses [draft] (back in the field), saying why under it. *)
val refuse : t -> Which.t -> draft:string -> string -> t

(** Applies the draft: the text to set, stripped ([""]: back to the
    built-in), if there is a draft. *)
val commit : t -> Which.t -> t * string option

(** The fields with a draft. *)
val drafted : t -> Which.t list
