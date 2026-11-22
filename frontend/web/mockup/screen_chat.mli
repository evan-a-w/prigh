(** [#chat]: a session mid-turn, with tool calls, markdown and the
    composer. *)

open! Core
open Bonsai_web

(** [composer]: in place of the default composer (for screens showing its
    menus). *)
val component
  :  ?composer:(Tip.t -> Vdom.Node.t) Bonsai.t
  -> tip:Tip.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
