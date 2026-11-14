open! Core
open! Import

(** While a message outside chat mode cannot run because no tool host can
    run it (the state's [no_host]), a strip above the composer says so (the
    reason's first clause, the whole of it in the title) with "Chat without
    a host", which switches the session to chat mode and sends the draft
    when there is one ([App.Action.Chat_without_host]). Nothing otherwise. *)

(** The reason, when the strip shows. *)
val reason : App.Model.t -> string option

val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
