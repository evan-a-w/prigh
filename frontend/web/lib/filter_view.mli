open! Core
open! Import

(** The transcript filter ([/filter], {!Filter_menu}) on the page. *)

(** Left of the file browser's toggle: opens the menu; accented, with how
    many kinds are hidden, while a filter is on. *)
val button : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The menu, under the button. *)
val menu
  :  App.Model.t
  -> Filter_menu.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** At the top of the transcript while a filter is on: what it hides, how
    much of what is loaded, and "Show all". *)
val note : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t list
