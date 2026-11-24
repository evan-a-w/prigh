open! Core
open! Import

(** What prigh knows about a provider's accounts beyond their credentials:
    labels, the default and the fallback order. auth.json keeps it under
    {!key} (no secrets), so pi, which ignores keys it does not know, still
    reads the [main] accounts' credentials. A provider it does not mention
    has the accounts its credentials make, in {!Provider_accounts}' default
    order: nothing is written until the user names, adds, reorders or
    removes one. *)

module Account : sig
  type t =
    { id : Account_id.t
    ; label : string
    ; fallback : bool
    (** takes over when another of the provider's accounts hits its usage
        limit *)
    ; created_ms : int option
    }
  [@@deriving sexp, equal]
end

module Provider_meta : sig
  type t =
    { default : Account_id.t option
    ; accounts : Account.t list (** in fallback order *)
    }
  [@@deriving sexp, equal]
end

type t [@@deriving sexp_of, equal]

(** ["prigh:accounts"] *)
val key : string

val empty : t
val is_empty : t -> bool
val find : t -> Provider_id.t -> Provider_meta.t option
(** A provider without accounts is dropped. *)
val set : t -> Provider_id.t -> Provider_meta.t -> t

(** Errors say to fix or remove the entry. *)
val of_json : Json.t -> t Or_error.t

val to_json : t -> Json.t
