open! Core
open! Import

(** Claude Pro/Max login (the "Anthropic (Claude Pro/Max)" flow in pi):
    PKCE authorization code flow against claude.ai with the Claude Code client
    id and scopes, a loopback callback on port 53692 raced against a pasted
    code, and JSON token exchange/refresh at platform.claude.com. *)

module Config : sig
  type t =
    { client_id : string
    ; authorize_url : string
    ; token_url : string
    ; callback_host : string
    ; callback_port : int
    ; callback_path : string
    ; scopes : string
    }

  val default : t
  val redirect_uri : t -> string
end

val login
  :  env:Env.t
  -> ?config:Config.t
  -> Auth_interaction.t
  -> Credential.t Or_error.t

val refresh
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?config:Config.t
  -> refresh_token:string
  -> unit
  -> Credential.Oauth.t Or_error.t

module For_testing : sig
  val authorize_url : Config.t -> Pkce.Pair.t -> string

  val credential_of_body
    :  ?now_ms:int
    -> string
    -> Credential.Oauth.t Or_error.t
end
