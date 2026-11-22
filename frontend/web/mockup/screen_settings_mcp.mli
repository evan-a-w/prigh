(** [#settings-mcp]: the MCP servers section. *)

open! Core
open Bonsai_web

val component
  :  tip:Tip.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
