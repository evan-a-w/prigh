(** [#chat]: a session mid-turn, with tool calls, markdown and the
    composer. *)

open! Core
open Bonsai_web

(** [banner] shows above the transcript, [above_composer] between it and
    the composer (a confirmation, a question); [queued]: the queued
    messages' pills. *)
val component
  :  ?banner:Vdom.Node.t Bonsai.t
  -> ?above_composer:Vdom.Node.t Bonsai.t
  -> ?queued:string list
  -> tip:Tip.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
