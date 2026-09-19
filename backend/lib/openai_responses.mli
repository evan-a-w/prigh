open! Core
open! Import

(** OpenAI Responses API provider, for api.openai.com (API key) and for the
    ChatGPT Codex backend (OAuth access token plus the [chatgpt-account-id]
    header). Requests use [store: false] and replay reasoning items through
    their encrypted content, stored as the thinking block's signature. *)

module Endpoint : sig
  type t =
    | Openai of { api_key : string }
    | Codex of
        { access_token : string
        ; account_id : string
        }

  val default_base_url : t -> string
end

val create
  :  env:Env.t
  -> ?base_url:string
  -> ?timeout:Time_ns.Span.t
  -> endpoint:Endpoint.t
  -> unit
  -> Provider.t

module For_testing : sig
  val request_body : endpoint:Endpoint.t -> Provider.Request.t -> Json.t
  val headers : Endpoint.t -> (string * string) list

  val parse_events
    :  string list
    -> Assistant_event.t list * Stop_reason.t * Usage.t
end
