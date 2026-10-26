open! Core
open! Import

(** Replaces the older part of a conversation with a model-written summary so
    the context stays within the model's window: a session's (recorded as a
    compaction entry) or a subagent's in-memory one. *)

(** True when the last request's input tokens exceed the model's window
    threshold. *)
val should_compact : Model.t -> input_tokens:int -> bool

val render_transcript : Message.t list -> string

(** How much of the conversation's end is kept verbatim by default. *)
val default_keep_recent_tokens : int

(** What the agent's own requests share, so that the summarising request is
    the agent's next request with the request for a summary at its end and
    the provider's prompt cache serves the conversation. *)
module Prompt : sig
  type t =
    { system : string option
    ; tools : Tool_spec.t list
    ; thinking : Thinking.t
    ; cache_key : string option
    }

  (** No system prompt or tools, thinking off. *)
  val none : t
end

module Outcome : sig
  type t =
    { summary : string
    ; usage : Usage.t (** of the summarising requests *)
    }
  [@@deriving sexp_of]
end

(** Which of a conversation's messages a compaction keeps: those from
    [kept_from] on (a user or an assistant message, so tool calls keep their
    results), roughly the last [keep_recent_tokens] worth but at least the
    last turn, and [pinned], before [kept_from], carried ahead of the
    summary: the user's own messages ({!Pinned_messages.shorten}ed; the
    newest first, about 6k tokens' worth) and the user's latest, as it is.
    [None] when there is nothing to summarise. *)
module Plan : sig
  module Pin : sig
    type t =
      { index : int
      ; shortened : bool
      }
    [@@deriving sexp_of]
  end

  type t =
    { kept_from : int
    ; pinned : Pin.t list
    }
  [@@deriving sexp_of]
end

val plan : keep_recent_tokens:int -> Message.t list -> Plan.t option

(** Summarises a conversation held in memory, returning the messages that
    replace it: the pinned ones, the summary ({!Session.summary_message}),
    then the kept ones. [instructions] (the user's) say what to focus on.

    The summary has fixed sections (goal and constraints, decisions, failed
    approaches and exact errors, files, current state, next steps); an
    earlier summary among the summarised messages is updated rather than
    summarised again. The summarised part goes as it is, as the agent's next
    request under [prompt], ending with the request for a summary; when that
    would not fit the window, [overflowed] (the provider rejected the
    context as too long) or the provider rejects it as too long, a rendered
    transcript goes in a request of its own instead. The files the
    summarised tool calls read and modified (and the earlier summary's) are
    appended to the summary ({!Compaction_files}), relative ones resolved
    against [cwd]. *)
val compact_messages
  :  provider:Provider.t
  -> model:Model.t
  -> prompt:Prompt.t
  -> ?overflowed:bool
  -> ?keep_recent_tokens:int
  -> ?cancel:Cancellation.t
  -> ?cwd:string
  -> ?instructions:string
  -> Message.t list
  -> (Message.t list * Outcome.t) Or_error.t

(** Like [compact_messages] on the session's {!Session.messages}, appending a
    compaction entry; paths are resolved against the cwd each message was
    made in ({!Session.context_cwds}). *)
val compact
  :  env:Env.t
  -> provider:Provider.t
  -> model:Model.t
  -> prompt:Prompt.t
  -> ?overflowed:bool
  -> ?keep_recent_tokens:int
  -> ?cancel:Cancellation.t
  -> ?instructions:string
  -> Session.t
  -> Outcome.t Or_error.t
