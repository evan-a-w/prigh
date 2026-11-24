open! Core
open! Import

(** How much of their subscription's usage limits the user's Claude and
    ChatGPT logins have used, for [usage_limits] (Settings → Usage, [/usage]).

    - Claude (an [anthropic] OAuth login): [GET
      https://api.anthropic.com/api/oauth/usage] with the access token and the
      [oauth-2025-04-20] beta, as Claude Code's [/usage] does. It answers
      windows by name ([five_hour], [seven_day], [seven_day_opus], ...: each
      [{utilization, resets_at}] or [null]) and, lately, [limits]: the
      server's own rows ([{kind, group, percent, resets_at, scope}]), which
      are shown instead when present.
    - ChatGPT (an [openai-codex] login): [GET
      https://chatgpt.com/backend-api/wham/usage] with the access token and
      [chatgpt-account-id], as the Codex CLI's [/status] does: [plan_type],
      [rate_limit.primary_window]/[secondary_window]
      ([{used_percent, limit_window_seconds, reset_after_seconds, reset_at}])
      and [additional_rate_limits].

    Unknown windows are kept, labelled by their names; missing fields are
    tolerated. Tokens are refreshed as for model requests
    ({!Provider_auth.resolve}). *)

module Window : sig
  type t =
    { id : string (** the provider's name for it, e.g. [five_hour] *)
    ; label : string (** e.g. ["5-hour session"] *)
    ; used_percent : float (** 0 to 100 *)
    ; resets_at : float option (** seconds since the epoch *)
    }
  [@@deriving sexp_of, equal]
end

module Status : sig
  type t =
    | Not_signed_in
    | Api_key (** signed in with an API key: billed per token, no limits *)
    | Limits of
        { plan : string option (** e.g. ["pro"] *)
        ; windows : Window.t list
        }
    | Failed of string (** says what to do next *)
  [@@deriving sexp_of, equal]
end

module Account_usage : sig
  type t =
    { id : Account_id.t
    ; shared_by : string option (** another user's, shared with this one *)
    ; label : string
    ; default : bool
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

module Provider_usage : sig
  type t =
    { provider : Provider_id.t
    ; status : Status.t (** the default account's *)
    ; accounts : Account_usage.t list (** each stored login's, in order *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { fetched_at : float
  ; providers : Provider_usage.t list (** Claude, then ChatGPT *)
  }
[@@deriving sexp_of, equal]

(** [{fetched_at_ms, providers: [{provider, name, status, plan?, windows?,
    error?, accounts: [{ref, id, label, default, shared_by?, status, ...}]}]}], [status] one of [not_signed_in], [api_key], [limits] and
    [error]; a window is [{id, label, used_percent, resets_at_ms}]. *)
val to_json : t -> Json.t

(** The providers with subscription limits: [anthropic], [openai-codex]. *)
val providers : Provider_id.t list

(** ["Claude"], ["ChatGPT"]. *)
val name : Provider_id.t -> string

(** The HTTP GETs this needs, as a value so that tests answer without a
    network: the status, headers and body, or why there was none. *)
module Http : sig
  type t =
    url:string
    -> headers:(string * string) list
    -> unit
    -> (int * (string * string) list * string, string) Result.t

  (** {!Http_client.get}, with a 20 second timeout. *)
  val real : env:Env.t -> t
end

(** The usage endpoints' answers. [now] turns ChatGPT's
    [reset_after_seconds] into a time when [reset_at] is missing. *)
val parse_anthropic : string -> Status.t

val parse_codex : now:float -> string -> Status.t

(** A login another user shares with this one: its usage is asked for with
    the owner's credential ([resolve]), never shown. *)
module Shared : sig
  type t =
    { owner : string
    ; provider : Provider_id.t
    ; id : Account_id.t
    ; label : string
    ; resolve : unit -> Provider_auth.Resolved.t option Or_error.t
    }
end

(** The signed-in providers' usage, asked for at once. [PRIGH_USAGE_ORIGIN]
    (from [getenv]) replaces both providers' origins, for prigh-web's e2e. *)
val fetch
  :  env:Env.t
  -> ?getenv:(string -> string option)
  -> ?refresh:
       (Provider_id.t -> refresh_token:string -> Credential.Oauth.t Or_error.t)
  -> ?shared:Shared.t list (** after the user's own *)
  -> http:Http.t
  -> now:(unit -> float)
  -> Auth_store.t
  -> t
