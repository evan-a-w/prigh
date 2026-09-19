open! Core
open! Import

(** Replaces the older part of a session's conversation with a model-written
    summary so the context stays within the model's window. *)

val estimate_tokens : Message.t list -> int

(** True when the last request's input tokens exceed the model's window
    threshold. *)
val should_compact : Model.t -> input_tokens:int -> bool

val render_transcript : Message.t list -> string

(** Summarises everything but roughly the last [keep_recent_tokens] worth of
    messages (split at a user message), appends a compaction entry and
    returns the summary. *)
val compact
  :  env:Env.t
  -> provider:Provider.t
  -> model:Model.t
  -> ?keep_recent_tokens:int
  -> ?cancel:Cancellation.t
  -> Session.t
  -> string Or_error.t
