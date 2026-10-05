open! Core
open! Import

(** [/btw]: a side question answered by one tool-less model call over a
    snapshot of the conversation, never added to the session. *)

(** Makes a conversation snapshot, possibly taken mid-turn, acceptable to
    providers: every tool call is followed by exactly one result (missing
    ones become a "still running" placeholder), results without a call and
    empty assistant messages are dropped. *)
val sanitize : Message.t list -> Message.t list

(** The question as the final user message, with a note that it is a side
    question to answer briefly without tools. *)
val question_message : string -> Message.t

val request
  :  model:Model.t
  -> system:string
  -> messages:Message.t list
  -> question:string
  -> Provider.Request.t
