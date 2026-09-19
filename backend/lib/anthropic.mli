open! Core
open! Import

(** Anthropic Messages API provider. With an OAuth (Claude Pro/Max) token the
    request is shaped like Claude Code's: Bearer auth with the claude-cli
    user agent, the [claude-code-20250219] and [oauth-2025-04-20] betas, the
    Claude Code identity as the first system block, and tool names in Claude
    Code's canonical casing (mapped back to ours on the way in). *)

module Auth : sig
  type t =
    | Api_key of string
    | Oauth of string
end

val default_base_url : string

(** Tokens containing ["sk-ant-oat"] are OAuth tokens even when they arrive
    via [ANTHROPIC_API_KEY]/[ANTHROPIC_OAUTH_TOKEN]. *)
val auth_of_token : method_:Provider_auth.Method.t -> string -> Auth.t

val create
  :  env:Env.t
  -> ?base_url:string
  -> ?timeout:Time_ns.Span.t
  -> auth:Auth.t
  -> unit
  -> Provider.t

module For_testing : sig
  val request_body : oauth:bool -> Provider.Request.t -> Json.t
  val headers : auth:Auth.t -> thinking_on:bool -> (string * string) list

  val parse_events
    :  tools:Tool_spec.t list
    -> string list
    -> Assistant_event.t list
       * [ `Stop_reason of string option ]
       * [ `Usage of Usage.t ]
       * [ `Error of string option ]
end
