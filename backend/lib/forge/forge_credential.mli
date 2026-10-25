open! Core
open! Import

(** A git host's sign-in, kept in [auth.json] under ["forge:HOST"] (the
    provider credentials' file: same lock, atomic 0600 writes):
    [{"type":"forge","kind","method","token","refresh"?,"expires"?,
    "client_id"?,"user"?}]. *)

module Method : sig
  type t =
    | Oauth
    | Token (** a pasted personal access token *)
  [@@deriving sexp, equal]

  val to_string : t -> string
  val of_string : string -> t Or_error.t
end

type t =
  { kind : Forge_kind.t
  ; method_ : Method.t
  ; token : string
  ; refresh : string option
  ; expires_ms : int option (** [None]: does not expire *)
  ; client_id : string option (** the OAuth client that refreshes it *)
  ; user : string option (** the account's login *)
  }
[@@deriving sexp, equal]

(** ["forge:" ^ host] *)
val key : host:string -> string

val key_prefix : string
val to_json : t -> Json.t
val of_json : Json.t -> t Or_error.t

(** Expires within {!Credential.refresh_margin_ms}. *)
val needs_refresh : ?now_ms:int -> t -> bool

(** The [Authorization] header's value: [Bearer] for OAuth tokens and
    GitHub's, [token] for Forgejo's personal access tokens. *)
val authorization : t -> string
