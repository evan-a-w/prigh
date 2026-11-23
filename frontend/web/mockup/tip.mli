(** Tooltips on icon buttons, one open at a time. *)

open! Core
open Bonsai_web

type t

val component : local_ Bonsai.graph -> t Bonsai.t

(** [wrap t ~id label trigger]: [trigger] with the tooltip [label]; [id]
    unique on the page. *)
val wrap
  :  ?side:Loon_ui.Popper.Side.t
  -> t
  -> id:string
  -> string
  -> Vdom.Node.t
  -> Vdom.Node.t
