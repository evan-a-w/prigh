open! Core
open! Import

(** Antigravity's (agy's) Google login: its OAuth client, refresh, a paste
    login, and the token document agy reads.

    The client (an "installed app" client id and secret, both in agy's
    binary) is Google's: prigh does not ship it. It is found in an installed
    agy ({!Client.of_candidates}: the pair whose SHA-256 is the known
    consumer client's) and kept beside [auth.json] ({!Client.save}), or comes
    from [PRIGH_ANTIGRAVITY_CLIENT_ID] and [PRIGH_ANTIGRAVITY_CLIENT_SECRET].

    Google does not rotate refresh tokens: refreshing does not retire another
    copy of the login. *)

module Client : sig
  type t =
    { id : string
    ; secret : string
    }
  [@@deriving sexp_of, equal]

  (** An extended regular expression matching the candidates in agy's
      binary ([grep -aoE]). *)
  val pattern : string

  (** The consumer client among the strings [pattern] matched, if there. *)
  val of_candidates : string list -> t option

  (** The environment's, else the one saved beside [store]. *)
  val load : ?getenv:(string -> string option) -> Auth_store.t -> t option

  val save : Auth_store.t -> t -> unit Or_error.t

  (** What to do without one. *)
  val missing : string
end

val token_url : string

(** A refresh: Google's answer keeps [refresh_token]. *)
val refresh
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?token_url:string
  -> client:Client.t
  -> refresh_token:string
  -> unit
  -> Credential.Oauth.t Or_error.t

(** The paste login (Google redirects to antigravity.google, which shows the
    code), PKCE with the client's secret, offline access. *)
val login
  :  env:Env.t
  -> client:Client.t
  -> Auth_interaction.t
  -> Credential.t Or_error.t

(** The document agy reads from [antigravity-oauth-token], with the access
    token only (agy cannot refresh it), on one line. *)
val agy_token_document : access:string -> expires_ms:int -> string

module Agy_token : sig
  type t =
    { access : string option
    ; refresh : string option
    ; auth_method : string option (** ["consumer"], ["gcp"] *)
    ; email : string option
    }
  [@@deriving sexp_of]

  (** agy's token document (file or keyring item). *)
  val parse : string -> t Or_error.t
end

module For_testing : sig
  val credential_of_body
    :  now_ms:int
    -> refresh_token:string
    -> string
    -> Credential.Oauth.t Or_error.t

  val authorize_url
    :  client:Client.t
    -> challenge:string
    -> state:string
    -> string
end
