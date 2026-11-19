open! Core
open! Import

(** Google Cloud desktop OAuth credentials, supplied by the operator. This
    does not use Gemini consumer subscriptions or Google Code Assist. *)
module Config : sig
  type t

  val of_env : ?getenv:(string -> string option) -> unit -> t Or_error.t
end

(** Headers for the native Generative Language API, never for an OpenAI
    compatible endpoint or a user-supplied URL. *)
val headers : Config.t -> access_token:string -> (string * string) list

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
