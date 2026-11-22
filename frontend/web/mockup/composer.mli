(** The composer: the message box, its chips (model, thinking, mode, host),
    the status (context, cost) and send, and the checkout under it. *)

open! Core
open Bonsai_web

(** [wrap_chip id]: a replacement for the tooltip around the chip [id]
    (["model"], ["thinking"], ["mode"], ["host"]), e.g. a popover with the
    chip as its trigger. [draft]: the text in the box. [above]: shown over
    the box (a completion menu), across its width. *)
val view
  :  ?queued:string list
  -> ?wrap_chip:(string -> (Vdom.Node.t -> Vdom.Node.t) option)
  -> ?draft:string
  -> ?above:Vdom.Node.t
  -> tip:Tip.t
  -> unit
  -> Vdom.Node.t
