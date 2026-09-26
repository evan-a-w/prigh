open! Core
open! Import

(** A one-line, model-written description of what a session is about,
    recorded as a [Description] entry once the conversation is long enough
    to say. *)

(** True when the session has no description yet and enough conversation
    (a second user turn, or a long first one) to describe. *)
val wanted : Session.t -> bool

(** Asks [model] for the description, appends it to the session and returns
    it. *)
val describe
  :  provider:Provider.t
  -> model:Model.t
  -> ?cancel:Cancellation.t
  -> Session.t
  -> string Or_error.t

(** Normalises a model reply into one short line: first non-empty line,
    quotes and trailing period stripped, truncated. *)
val clean : string -> string
