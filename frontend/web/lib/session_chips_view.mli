open! Core
open! Import

(** The chips that open a menu over themselves ({!Chip}, a
    [Dialog.Popover]): the composer's (model, host) and the thinking level
    and the mode, which this draws wherever the status style puts them. *)

(** The session's thinking level and mode: quiet figures (the level's bars
    and word, the mode's short name) that open their menus over or under
    them (Alt+T, Alt+M cycle; /thinking, /mode). [cls] says where they are
    (CSS styles them by it); [quiet_ask] leaves the mode out while it is
    [ask], the default (the send ring's). *)
val view
  :  ?cls:string
  -> ?quiet_ask:bool
  -> App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** [chip]'s button with its menu, when open, positioned over or under it. *)
val anchor
  :  App.Model.t
  -> Chip.t
  -> Node.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** Whether one of [chips]' menus is open: what lifts their group over the
    menu's backdrop. *)
val menu_open : App.Model.t -> Chip.t list -> bool

(** What a chip's button says about its menu, open or not. *)
val menu_attrs : App.Model.t -> Chip.t -> Attr.t list

(** A choice the backend has still to confirm. *)
val pending_chip
  :  ?attrs:Attr.t list
  -> ?title:string
  -> cls:string
  -> string
  -> Node.t
