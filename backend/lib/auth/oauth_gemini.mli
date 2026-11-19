open! Core
open! Import

(** Google Cloud desktop OAuth credentials, supplied by the operator. This
    does not use Gemini consumer subscriptions or Google Code Assist. *)
module Config : sig
  type t

  val of_env : ?getenv:(string -> string option) -> unit -> t Or_error.t
  val model : t -> string
end

(** Stream only to the configured Vertex model at Google's fixed HTTPS origin.
    No endpoint override, redirects or token in the URL. *)
val post_stream
  :  env:Env.t
  -> cancel:Cancellation.t
  -> Config.t
  -> access_token:string
  -> body:string
  -> on_response:(Http_client.Response.t -> unit)
  -> on_chunk:(string -> unit)
  -> (Http_client.Response.t, Http_client.Error.t) Result.t Or_error.t

val login
  :  env:Env.t
  -> Config.t
  -> Auth_interaction.t
  -> Credential.t Or_error.t

val refresh
  :  env:Env.t
  -> cancel:Cancellation.t
  -> Config.t
  -> refresh_token:string
  -> Credential.Oauth.t Or_error.t

module For_testing : sig
  val stream_url : Config.t -> string

  val authorize_url
    :  Config.t
    -> Pkce.Pair.t
    -> state:string
    -> redirect_uri:string
    -> string

  val redirect_uri : int -> string

  val manual_code
    :  redirect_uri:string
    -> expected_state:string
    -> string
    -> string Or_error.t

  val credential_of_body
    :  ?now_ms:int
    -> ?previous_refresh:string
    -> string
    -> Credential.Oauth.t Or_error.t
end
