(** Pieces the side and bottom panels share. *)

open! Core
open Bonsai_web

(** A panel: a header row ([title], then [actions] on the right) over a
    scrolling body. *)
val view
  :  ?class_:string
  -> title:Vdom.Node.t list
  -> actions:Vdom.Node.t list
  -> Vdom.Node.t list
  -> Vdom.Node.t

(** A small muted icon button for panel headers and rows. *)
val icon_button : Tip.t -> id:string -> string -> Vdom.Node.t -> Vdom.Node.t

(** [+12 −3], green and red. *)
val counts : int -> int -> Vdom.Node.t
