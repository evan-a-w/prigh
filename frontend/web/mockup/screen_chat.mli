(** [#chat]: a session mid-turn, with tool calls, markdown and the
    composer. *)

open! Core
open Bonsai_web

val component
  :  tip:Tip.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
