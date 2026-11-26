open! Core
open! Import

(** Which login of a model's provider a session or request uses: one of the
    user's own ({!Account_id}), or one another user shares with them
    ({!Account_shares}). Written ["work"] and ["~alice/work"]. *)

type t =
  | Own of Account_id.t
  | Shared of
      { owner : string
      ; id : Account_id.t
      }
[@@deriving sexp, equal, compare]

val main : t
val to_string : t -> string

(** Errors say what a ref looks like. *)
val of_string : string -> t Or_error.t

val owner : t -> string option

(** Whether a reply written on [written] (as {!Message.Assistant} records
    it: [None] for the own [main]) was written on [account] ([None]: also
    the own [main]). *)
val same : string option -> t option -> bool
