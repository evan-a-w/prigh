(** [#login]: the sign-in page. *)

open! Core
open Bonsai_web

val component
  :  tip:Tip.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
