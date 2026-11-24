open! Core
open! Import

(** A login a user may run a provider's models on: their own (in their
    order), then those other users share with them and they did not hide.
    The default is their own default, else (having none of their own) the
    first shared one. *)

type t =
  { ref_ : Account_ref.t
  ; label : string
  ; default : bool
  ; fallback : bool
  }
[@@deriving sexp_of]

val list
  :  ?member:Account_shares.Member.t
  -> Auth_store.t
  -> Provider_id.t
  -> t list Or_error.t

(** By ref ({!Account_ref.to_string}), id or label (case-insensitive); the
    error lists the choices. *)
val find
  :  ?member:Account_shares.Member.t
  -> Auth_store.t
  -> Provider_id.t
  -> string
  -> t Or_error.t
