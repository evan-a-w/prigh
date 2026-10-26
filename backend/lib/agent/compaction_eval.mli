open! Core
open! Import

(** An offline evaluation of context compaction: replays a saved session's
    conversation (the messages of its active path, as they were recorded,
    before any compaction or masking) request by request, under each
    {!Strategy}, as the agent would have managed its context, and reports
    what each costs and what it keeps. A developer's tool ([prigh
    compaction-eval]) for tuning {!Observation_mask} and {!Compaction}.

    Tokens are estimated as {!Compaction.estimate_tokens} counts them. Each
    assistant message is one request, whose input is the context before it
    plus the system prompt and [overhead_tokens]. Before each request (but
    the first), masking and then summarising run when the agent would run
    them. Offline, a summary is a stub: a placeholder of [summary_tokens]
    and the files list ({!Compaction_files}); the pinned user messages are
    carried as the agent carries them. With [live], a model writes the
    summaries and answers questions about the final context. *)

module Strategy : sig
  type t =
    | None_ (** the raw conversation *)
    | Mask (** {!Observation_mask} only *)
    | Summarise (** {!Compaction} only *)
    | Mask_and_summarise (** both, as the agent does *)
  [@@deriving sexp_of, jsonaf_of, equal, enumerate]

  (** [none], [mask], [summarise], [mask+summarise]. *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

module Config : sig
  type t =
    { window : int option (** default: the model's context window *)
    ; threshold_tokens : int option
      (** summarise above this (and mask urgently); default: 80% of the
          window, as {!Compaction.should_compact} *)
    ; keep_recent_tokens : int (** {!Compaction.default_keep_recent_tokens} *)
    ; keep_turns : int (** {!Observation_mask.keep_turns} *)
    ; mask_min_bytes : int (** {!Observation_mask.min_bytes} *)
    ; mask_at : float (** {!Observation_mask.worth_masking}'s [at] *)
    ; mask_min_saving : float (** and its [min_saving] *)
    ; summary_tokens : int (** the offline stub summary's size *)
    ; overhead_tokens : int
      (** what each request sends besides the messages and the system prompt
          (the tools' specifications) *)
    ; strategies : Strategy.t list
    }
  [@@deriving sexp_of]

  val default : t
end

(** [hit] of [total] probes found. *)
module Fraction : sig
  type t =
    { hit : int
    ; total : int
    }
  [@@deriving sexp_of, jsonaf_of]
end

(** What a model made of a strategy's context, with [live]. *)
module Live : sig
  type t =
    { summaries : int
    ; failures : int (** summaries that failed (the context was kept) *)
    ; usage : Usage.t (** of the summaries and the questions *)
    ; cost_usd : float
    ; probed : bool (** false: the final context did not fit the model *)
    ; files_modified : Fraction.t
      (** the files changed whose names the answer gives *)
    ; constraints : Fraction.t
      (** the user's instructions ("don't...", "must...") the answer recalls
          (most of their words) *)
    ; last_error : Fraction.t (** whether the answer recalls the last error *)
    ; answer : string
    }
  [@@deriving sexp_of, jsonaf_of]

  module Setup : sig
    type t =
      { provider : Provider.t
      ; model : Model.t (** writes the summaries and answers *)
      ; cancel : Cancellation.t
      }
  end
end

module Result : sig
  type t =
    { strategy : Strategy.t
    ; requests : int
    ; peak_tokens : int (** the largest request *)
    ; over_window : int (** requests larger than the window *)
    ; input_tokens : int
      (** all requests' input, the summarising ones included: the cost
          without a prompt cache *)
    ; uncached_tokens : int
      (** of [input_tokens], those after the prefix each request shares with
          the one before: the cost with a prompt cache *)
    ; cached_cost_tokens : int
      (** [input_tokens] priced as with Anthropic's prompt cache, in plain
          input tokens: the uncached ones written to the cache (1.25 each),
          the rest read from it (0.1 each) *)
    ; cache_invalidations : int
      (** requests that do not extend the one before *)
    ; mask_batches : int
    ; compactions : int
    ; final_tokens : int
    ; users_verbatim : int
      (** of the user's own messages, those in the final context as they
          are *)
    ; users_shortened : int (** or shortened ({!Pinned_messages.shorten}) *)
    ; users_total : int
    ; files_read : Fraction.t
      (** the files read by successful tool calls whose paths the final
          context mentions *)
    ; files_modified : Fraction.t (** and those modified *)
    ; errors : Fraction.t
      (** the error results whose first line is in the final context *)
    ; live : Live.t option
    }
  [@@deriving sexp_of, jsonaf_of]
end

module Session_report : sig
  type t =
    { id : string
    ; title : string (** the start of the first prompt *)
    ; model : string
    ; window : int
    ; threshold_tokens : int
    ; messages : int
    ; recorded_input_tokens : int
      (** what the recorded requests' usage says they sent (input and cache
          reads) *)
    ; results : Result.t list
    }
  [@@deriving sexp_of, jsonaf_of]
end

module Report : sig
  type t =
    { sessions : Session_report.t list
    ; aggregate : Result.t list
      (** each strategy's results summed over the sessions ([peak_tokens]:
          the largest) *)
    }
  [@@deriving sexp_of, jsonaf_of]

  (** A table per session, then the aggregate. *)
  val to_table : t -> string
end

(** [model] defaults to the session's (built-in) model, else
    {!Model.default}; it sets the default window. *)
val session
  :  ?live:Live.Setup.t
  -> ?model:Model.t
  -> Config.t
  -> Session.t
  -> Session_report.t

val report : Session_report.t list -> Report.t
