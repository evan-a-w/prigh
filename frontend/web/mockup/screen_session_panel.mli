(** [#session-panel]: the session menu from the top bar's ⋯ (T3 Code's
    panel): host, open in, branch, commit, changes, previous agents, then
    fork, rewind, rename, export and delete. *)

open! Core
open Bonsai_web

val component : tip:Tip.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
