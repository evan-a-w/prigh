open! Core

(** Whether a provider error means the model cannot be used for now: its
    usage allowance or balance is exhausted (not a transient rate limit), or
    prigh has no credentials for it. Such errors are not retried; the agent
    hands the run over to the next model in [fallback_models] instead. *)
val unavailable : string -> bool

(** Appended by [Sse_request] to errors whose code or headers say the
    usage limit was reached, so that {!unavailable} sees it. *)
val marker : string
