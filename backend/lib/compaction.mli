open! Core
open! Import

(** Replaces the older part of a conversation with a model-written summary so
    the context stays within the model's window: a session's (recorded as a
    compaction entry) or a subagent's in-memory one. *)

val estimate_tokens : Message.t list -> int

(** True when the last request's input tokens exceed the model's window
    threshold. *)
val should_compact : Model.t -> input_tokens:int -> bool

val render_transcript : Message.t list -> string

(** How much of the conversation's end is kept verbatim by default. *)
val default_keep_recent_tokens : int

module Outcome : sig
  type t =
    { summary : string
    ; usage : Usage.t (** of the summarising request *)
    }
  [@@deriving sexp_of]
end

(** Which of a conversation's messages a compaction keeps: those from
    [kept_from] on (a user or an assistant message, so tool calls keep their
    results), roughly the last [keep_recent_tokens] worth but at least the
    last turn, and [pinned], the user's latest message, when it comes before
    [kept_from]. [None] when there is nothing to summarise. *)
module Plan : sig
  type t =
    { kept_from : int
    ; pinned : int option
    }
  [@@deriving sexp_of]
end

val plan : keep_recent_tokens:int -> Message.t list -> Plan.t option

(** Summarises a conversation held in memory, returning the messages that
    replace it: the pinned message, the summary
    ({!Session.summary_message}), then the kept ones. [instructions] (the
    user's) say what to focus on. *)
val compact_messages
  :  provider:Provider.t
  -> model:Model.t
  -> ?keep_recent_tokens:int
  -> ?cancel:Cancellation.t
  -> ?instructions:string
  -> Message.t list
  -> (Message.t list * Outcome.t) Or_error.t

(** Like [compact_messages] on the session's {!Session.messages}, appending a
    compaction entry. *)
val compact
  :  env:Env.t
  -> provider:Provider.t
  -> model:Model.t
  -> ?keep_recent_tokens:int
  -> ?cancel:Cancellation.t
  -> ?instructions:string
  -> Session.t
  -> Outcome.t Or_error.t
