open! Core
open! Import

(** A short, model-written title saying what a session is about, recorded as
    a [Description] entry. The first is written after the first turn, so
    the session list names the session at once; it is rewritten once, when
    the conversation has {!refine_after} messages from the user and so says
    better what the session is about. *)

(** How many of the user's messages make a session worth naming again. *)
val refine_after : int

(** True when the session has no description yet and a message from the
    user, or when its description predates the user's {!refine_after}th
    message and the conversation has reached it. *)
val wanted : Session.t -> bool

(** Asks [model] for the title ({!instructions}, {!conversation}), appends it
    to the session and returns it. *)
val describe
  :  provider:Provider.t
  -> model:Model.t
  -> ?cancel:Cancellation.t
  -> Session.t
  -> string Or_error.t

(** What the model is asked to write. *)
val instructions : string

(** What the model is given: the user's messages and the assistant's replies
    (without tool calls, tool outputs or the harness's notes), each cut
    short, keeping the first request and the latest ones when it is long;
    the current title, when there is one, to keep it if it still fits. *)
val conversation : ?current:string -> Message.t list -> string
