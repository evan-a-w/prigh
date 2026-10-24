open! Core
open! Import

(** Whether a provider error means the model cannot be used for now: its
    usage allowance or balance is exhausted (not a transient rate limit), or
    prigh has no credentials for it. Such errors are not retried; the agent
    hands the run over to the next model in [fallback_models] instead. *)
val unavailable : string -> bool

(** An {!unavailable} error that is about missing credentials, not an
    allowance that resets. *)
val credentials_missing : string -> bool

(** Appended by [Sse_request] to errors whose code or headers say the
    usage limit was reached, so that {!unavailable} sees it. *)
val marker : string

(** When an exhausted limit resets (seconds since the epoch), from what the
    provider sent: the error body's [resets_at] or [resets_in_seconds]
    (ChatGPT/Codex), the [x-codex-*-reset-at] headers (the window that is
    used up, else the latest), Anthropic's [anthropic-ratelimit-unified-*reset]
    headers, else [retry-after]. Header names are compared case-insensitively. *)
val reset_time
  :  now:float
  -> headers:(string * string) list
  -> body:string
  -> float option

(** [2026-10-11T14:05:00Z]. *)
val utc_string : float -> string

(** [" (resets at <ISO 8601 UTC>)"], appended to the error after {!marker}. *)
val reset_note : float -> string

(** The reset time noted in an error message by {!reset_note}. *)
val resets_at : string -> float option
