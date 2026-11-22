open! Core
open! Import

(** A subagent's chat in the session's place ([Model.subagent_chat]): its
    transcript, live, as the chat shows one, then how it failed. The top
    bar's breadcrumb says whose it is and leads back; the composer writes to
    it. *)
val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> string
  -> Node.t
