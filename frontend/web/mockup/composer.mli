(** The composer: the message box, its chips (model, thinking, mode, host),
    the status (context, cost) and send, and the checkout under it. *)

open! Core
open Bonsai_web

val view : ?queued:string list -> tip:Tip.t -> unit -> Vdom.Node.t
