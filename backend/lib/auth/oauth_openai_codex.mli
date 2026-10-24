open! Core
open! Import

(** ChatGPT Plus/Pro login (pi's "OpenAI (ChatGPT Plus/Pro)" browser flow):
    PKCE authorization code flow against auth.openai.com with a random state,
    a loopback callback on port 1455 raced against a pasted code, form-encoded
    token exchange/refresh, and the ChatGPT account id taken from the access
    token's JWT claims. *)

module Config : sig
  type t =
    { client_id : string
    ; authorize_url : string
    ; token_url : string
    ; callback_host : string
    ; callback_port : int
    ; callback_path : string
    ; originator : string
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
  val authorize_url : Config.t -> Pkce.Pair.t -> state:string -> string
  val account_id_of_token : string -> string option

  val credential_of_body
    :  ?now_ms:int
    -> string
    -> Credential.Oauth.t Or_error.t
end
