open! Core
open! Import

(** The skills [list_skills] returned, for [/skill:] completion. Skills depend
    on the session's tool host and directory, so they are kept under a key made
    of those and the session: once the key changes they are fetched again. *)

type t [@@deriving sexp_of]

val empty : t
val key : P.State.t option -> string

(** [None] unless fetched for [key]. *)
val find : t -> key:string -> P.Skill.t list option

(** [Some t] when [key]'s skills should be fetched now: neither known nor
    already asked for. *)
val request : t -> key:string -> t option

(** The reply to [request ~key]; ignored unless [key]'s are still awaited. *)
val set : t -> key:string -> P.Skill.t list -> t
