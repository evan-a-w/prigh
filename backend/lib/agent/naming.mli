open! Core
open! Import

(** Short model-written titles: of sessions ({!Session_description}) and of
    subagents ({!Subagent_name}). *)

(** Normalises a model reply (or any text) into a title: its first non-empty
    line, without a [Title:] label, Markdown emphasis, quotes or trailing
    punctuation, its first letter capitalised, and cut at a word boundary
    (with […]) to [max_length] (default 60) characters. *)
val clean : ?max_length:int -> string -> string

(** One tool-less request to [model] without thinking: [system] and the
    single user message [user]; the reply {!clean}ed. An error says what
    failed (prefixed by [what]), including an empty reply. *)
val ask
  :  provider:Provider.t
  -> model:Model.t
  -> ?cancel:Cancellation.t
  -> what:string
  -> system:string
  -> string
  -> string Or_error.t

(** Like {!ask}, delegating the underlying completion to [complete] (so
    callers can route through direct providers or external harnesses). *)
val ask_with
  :  complete:
       (system:string
        -> user:string
        -> max_tokens:int
        -> cancel:Cancellation.t
        -> (string, [ `Aborted | `Error of string ]) Result.t)
  -> ?cancel:Cancellation.t
  -> what:string
  -> system:string
  -> string
  -> string Or_error.t
