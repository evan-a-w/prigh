(** [#model]: the model picker from the composer's model chip (a drawer on
    phones), with the thinking level under it. *)

open! Core
open Bonsai_web

val component : tip:Tip.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
