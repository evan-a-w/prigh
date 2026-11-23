(** [#completion] and [#completion-files]: the menu over the composer as
    the user types a slash command ("/re") or a file ("@comp"). *)

open! Core
open Bonsai_web

module Kind : sig
  type t =
    | Commands
    | Files
end

val component
  :  Kind.t
  -> tip:Tip.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
