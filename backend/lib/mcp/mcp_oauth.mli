open! Core
open! Import

(** The MCP authorization spec's OAuth 2.1 client side, without state:
    discovering a server's authorization server (RFC 9728 Protected Resource
    Metadata, then RFC 8414 or OpenID Connect metadata), Dynamic Client
    Registration (RFC 7591), the authorization code request with PKCE (S256)
    and the [resource] parameter (RFC 8707), and the token endpoint
    (exchange, refresh, revocation). {!Mcp_auth} keeps the state. *)

(** A [WWW-Authenticate: Bearer ...] challenge. *)
module Challenge : sig
  type t =
    { resource_metadata : string option
    ; scope : string option
    ; error : string option
    }
  [@@deriving sexp_of]

  (** [None] unless the header has a [Bearer] challenge. *)
  val parse : string -> t option
end

(** An authorization server's metadata (RFC 8414). *)
module Server_metadata : sig
  type t =
    { issuer : string
    ; authorization_endpoint : string
    ; token_endpoint : string
    ; registration_endpoint : string option
    ; revocation_endpoint : string option
    ; scopes_supported : string list
    }
  [@@deriving sexp_of]
end

module Discovery : sig
  type t =
    { resource : string (** the [resource] parameter: the server's URI *)
    ; resource_metadata : string option
      (** where the Protected Resource Metadata was found; [None] for a
          server without it, whose authorization server is its origin (the
          2025-03-26 spec) *)
    ; server : Server_metadata.t
    ; scopes : string list (** what to ask for, when not configured *)
    }
  [@@deriving sexp_of]
end

(** The URLs to try for a server's Protected Resource Metadata, in order:
    the challenge's [resource_metadata], then the well-known URI with the
    server's path, then at its root. *)
val resource_metadata_urls : url:string -> Challenge.t option -> string list

(** The URLs to try for an issuer's metadata, in order: RFC 8414's
    [/.well-known/oauth-authorization-server] (with the issuer's path
    inserted after it), then OpenID Connect's [openid-configuration] (path
    inserted, then appended). *)
val server_metadata_urls : issuer:string -> string list

(** The challenge of the server at [url] to a request without a token
    (an [initialize]), if it answers 401 with one. *)
val probe : env:Env.t -> url:string -> Challenge.t option

(** Finds the authorization server of the MCP server at [url] (which
    answered 401 with [challenge]). The error says what is missing. *)
val discover
  :  env:Env.t
  -> url:string
  -> ?challenge:Challenge.t
  -> unit
  -> Discovery.t Or_error.t

(** An OAuth client: registered ({!register}) or configured. *)
module Client : sig
  module Auth_method : sig
    type t =
      | None_ (** a public client *)
      | Basic (** [client_secret_basic] *)
      | Post (** [client_secret_post] *)
    [@@deriving sexp_of, equal]
  end

  type t =
    { id : string
    ; secret : string option
    ; auth_method : Auth_method.t
    }

  (** With the secret hidden. *)
  val sexp_of_t : t -> Sexp.t

  val to_json : t -> Json.t
  val of_json : Json.t -> t Or_error.t
end

(** Dynamic Client Registration of a public client ("prigh") with
    [redirect_uri]. Errors when the server has no registration endpoint,
    saying to configure a client id. *)
val register
  :  env:Env.t
  -> Server_metadata.t
  -> redirect_uri:string
  -> Client.t Or_error.t

(** The authorization request (code, S256 PKCE, [state], [resource]). *)
val authorize_url
  :  Server_metadata.t
  -> Client.t
  -> redirect_uri:string
  -> challenge:string
  -> state:string
  -> resource:string
  -> scopes:string list
  -> string

module Token : sig
  type t =
    { access : string
    ; refresh : string option
    ; expires_ms : int option (** absolute, Unix ms *)
    }

  (** With the tokens hidden. *)
  val sexp_of_t : t -> Sexp.t

  val to_json : t -> Json.t
  val of_json : Json.t -> t Or_error.t

  (** Whether it expires within a minute of [now_ms]. *)
  val expiring : t -> now_ms:int -> bool
end

(** Exchanges an authorization code for a token. *)
val exchange
  :  env:Env.t
  -> now_ms:int
  -> token_endpoint:string
  -> Client.t
  -> code:string
  -> verifier:string
  -> redirect_uri:string
  -> resource:string
  -> Token.t Or_error.t

(** A new token from the refresh token; keeps that refresh token when the
    server sends none. *)
val refresh
  :  env:Env.t
  -> now_ms:int
  -> token_endpoint:string
  -> Client.t
  -> refresh:string
  -> resource:string
  -> Token.t Or_error.t

(** RFC 7009 revocation of [token]. *)
val revoke
  :  env:Env.t
  -> revocation_endpoint:string
  -> Client.t
  -> token:string
  -> unit Or_error.t
