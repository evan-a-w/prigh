(** [#review]: the chat beside a resizable review panel: changed files, a
    diff with a comment thread, Commit and Open PR. *)

open! Core
open Bonsai_web

val component : tip:Tip.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
