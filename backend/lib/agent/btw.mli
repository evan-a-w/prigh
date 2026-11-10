open! Core
open! Import

(** [/btw]: a side question answered by one tool-less model call over a
    snapshot of the conversation, never added to the session. *)

(** Makes a conversation snapshot, possibly taken mid-turn, acceptable to
    providers: every tool call is followed by exactly one result (missing
    ones become a "still running" placeholder), results without a call and
    empty assistant messages are dropped. *)
val sanitize : Message.t list -> Message.t list

(** Who asks: the user ([/btw]), or another session's agent
    ([Tool_ask_session]). *)
module Asker : sig
  type t =
    | User
    | Session
end

(** The question as the final user message, with a note that it is a side
    question to answer briefly without tools. [asker] defaults to [User]. *)
val question_message : ?asker:Asker.t -> string -> Message.t

(** [cache_key]: the session's, so the question reuses its cached prefix. *)
val request
  :  ?asker:Asker.t
  -> ?cache_key:string
  -> model:Model.t
  -> system:string
  -> messages:Message.t list
  -> question:string
  -> unit
  -> Provider.Request.t
