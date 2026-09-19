open! Core
open! Import

(** Which login methods each provider offers, and how a request token is
    obtained from the store or the environment. A stored credential owns the
    provider; environment variables are consulted only when nothing is stored,
    and a failed OAuth refresh is an error rather than a silent fallback. *)

module Method : sig
  type t =
    | Api_key
    | Oauth
  [@@deriving sexp, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t option
  val label : Provider_id.t -> t -> string
end

module Resolved : sig
  type t =
    { token : string
    ; method_ : Method.t
    ; account_id : string option
    ; source : string
    }
  [@@deriving sexp_of]
end

module Status : sig
  type t =
    { provider : Provider_id.t
    ; methods : Method.t list
    ; configured : (Method.t * string) option
    ; expires_ms : int option
    }
  [@@deriving sexp_of]
end

val methods : Provider_id.t -> Method.t list
val env_vars : Provider_id.t -> string list

(** Refreshes OAuth tokens that expire within five minutes, persisting the
    rotated credential under the store lock. [None] means not configured. *)
val resolve
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?getenv:(string -> string option)
  -> ?refresh:
       (Provider_id.t -> refresh_token:string -> Credential.Oauth.t Or_error.t)
  -> Auth_store.t
  -> Provider_id.t
  -> Resolved.t option Or_error.t

(** Side-effect free (no refresh). *)
val status
  :  ?getenv:(string -> string option)
  -> Auth_store.t
  -> Status.t list Or_error.t

val login
  :  env:Env.t
  -> Auth_store.t
  -> Provider_id.t
  -> Method.t
  -> Auth_interaction.t
  -> unit Or_error.t

val logout : Auth_store.t -> Provider_id.t -> unit Or_error.t
