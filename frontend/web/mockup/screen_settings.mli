(** [#settings]: settings, on its Appearance section (on phones, the list of sections first). *)

open! Core
open Bonsai_web

val component
  :  tip:Tip.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
