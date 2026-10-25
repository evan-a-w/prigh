open! Core
open! Import

(** What the backend lists for the session's tool host and directory (its
    skills, its repository's worktrees), for completion and pickers. It is
    kept under a key made of those and the session: once the key changes it
    is fetched again. *)

type 'a t [@@deriving sexp_of]

val empty : 'a t
val key : P.State.t option -> string

(** [None] unless fetched for [key]. *)
val find : 'a t -> key:string -> 'a option

(** [Some t] when [key]'s should be fetched now: neither known nor already
    asked for. *)
val request : 'a t -> key:string -> 'a t option

(** The reply to [request ~key]; ignored unless [key]'s is still awaited. *)
val set : 'a t -> key:string -> 'a -> 'a t

(** Known for [key], whatever was awaited (a reply fetched for another
    reason). *)
val put : 'a t -> key:string -> 'a -> 'a t
