open! Core
open! Import

(** Google Gemini Developer API's native streaming endpoint. [headers] includes
    [x-goog-api-key] for API-key authentication. *)
val create
  :  env:Env.t
  -> ?base_url:string
  -> ?timeout:Time_ns.Span.t
  -> headers:(string * string) list
  -> unit
  -> Provider.t

(** Google Cloud Vertex AI, authenticated with the configured OAuth token.
    Uses the same request/response format as the Developer API, at Vertex's
    fixed publisher endpoint. *)
val vertex
  :  env:Env.t
  -> ?timeout:Time_ns.Span.t
  -> ?idle_timeout:Time_ns.Span.t
  -> config:Oauth_gemini.Config.t
  -> access_token:string
  -> unit
  -> Provider.t

module For_testing : sig
  val request_body : Provider.Request.t -> Json.t
  val response : model:Model.t -> Json.t list -> Message.Assistant.t
end
