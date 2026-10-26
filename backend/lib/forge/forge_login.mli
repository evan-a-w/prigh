open! Core
open! Import

(** Signing in to a git host through {!Auth_interaction} prompts, so every
    frontend runs it:
    - GitHub: the OAuth device flow (RFC 8628) with the configured OAuth
      app's client id (no secret): the user enters a code at
      [github.com/login/device] while prigh polls for the token.
    - Forgejo/Gitea: OAuth authorization code with PKCE as a public client
      (by default the instance's pre-registered git-credential-oauth app),
      redirected to a loopback server on [127.0.0.1], or the redirect URL
      pasted when the browser cannot reach it.
    - Either: a pasted personal access token.

    Every token is checked by fetching the signed-in user. *)

module Listen : sig
  (** Starts the loopback server that receives the OAuth redirect on
      [127.0.0.1] at path [/]. *)
  type t =
    sw:Switch.t -> expected_state:string -> Oauth_callback_server.t Or_error.t

  val real : env:Env.t -> t
end

(** OAuth when the instance has a client id and [method_] is not [Token]. *)
val login
  :  http:Forge_http.t
  -> sleep:(float -> unit)
  -> ?listen:Listen.t
  -> ?now_ms:(unit -> int)
  -> Forge_instance.t
  -> method_:Forge_credential.Method.t option
  -> Auth_interaction.t
  -> Forge_credential.t Or_error.t

(** A fresh OAuth token for one that expires soon. *)
val refresh
  :  http:Forge_http.t
  -> ?cancel:Cancellation.t
  -> ?now_ms:int
  -> Forge_instance.t
  -> Forge_credential.t
  -> Forge_credential.t Or_error.t

(** Why OAuth is unavailable for the instance and how to enable it, or what
    its consent page will name. *)
val oauth_note : Forge_instance.t -> string option

val github_scope : string
val forgejo_scope : string
