open! Core
open! Import

(** Which login methods each provider offers, and how a request token is
    obtained from the store or the environment. A stored credential owns the
    provider; environment variables are consulted only when nothing is stored,
    and a failed OAuth refresh is an error rather than a silent fallback. *)

module Method = Login_method

module Resolved : sig
  type t =
    { token : string
    ; method_ : Method.t
    ; account_id : string option
    ; source : string
    ; account : Account_id.t option (** [None]: from the environment *)
    }
  [@@deriving sexp_of]
end

(** One of a provider's stored logins ({!Provider_accounts}). *)
module Account_status : sig
  type t =
    { id : Account_id.t
    ; label : string
    ; method_ : Method.t
    ; source : string
    ; expires_ms : int option
    ; default : bool
    ; fallback : bool
    ; shared_with : Account_meta.Grant.t list
    }
  [@@deriving sexp_of]
end

module Status : sig
  type t =
    { provider : Provider_id.t
    ; methods : Method.t list
    ; configured : (Method.t * string) option
    ; expires_ms : int option
    ; custom : Custom_provider.t option
    ; accounts : Account_status.t list
      (** [configured] and [expires_ms] are the default one's *)
    }
  [@@deriving sexp_of]
end

val methods : Provider_id.t -> Method.t list
val env_vars : Provider_id.t -> string list

(** Refreshes OAuth tokens that expire within five minutes, persisting the
    rotated credential under the store lock. [None] means not configured.
    [account] (default: the provider's default one) must be logged in. *)
val resolve
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?getenv:(string -> string option)
  -> ?refresh:
       (Provider_id.t -> refresh_token:string -> Credential.Oauth.t Or_error.t)
  -> ?account:Account_id.t
  -> Auth_store.t
  -> Provider_id.t
  -> Resolved.t option Or_error.t

(** Side-effect free (no refresh). The built-in providers, then [custom].
    A custom provider is always usable: its [configured] source is
    ["stored api key"], the environment variable, or ["no key"]. *)
val status
  :  ?getenv:(string -> string option)
  -> ?custom:Custom_provider.t list
  -> Auth_store.t
  -> Status.t list Or_error.t

(** Stores the login as {!Provider_accounts.save_login} does; returns its
    account. *)
val login
  :  env:Env.t
  -> ?account:Account_id.t
  -> ?label:string
  -> Auth_store.t
  -> Provider_id.t
  -> Method.t
  -> Auth_interaction.t
  -> Account_id.t Or_error.t

(** [account] defaults to the default one; nothing to do without one. *)
val logout
  :  ?account:Account_id.t
  -> Auth_store.t
  -> Provider_id.t
  -> unit Or_error.t
