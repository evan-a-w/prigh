(** The composer: the message box; under it attach and the context ring
    (context and cost in its tooltip) on the left, the model and host chips
    and send on the right; the checkout under the box. *)

open! Core
open Bonsai_web

(** [wrap_chip id]: a replacement for the tooltip around the chip [id]
    (["model"], ["host"]), e.g. a popover with the
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
