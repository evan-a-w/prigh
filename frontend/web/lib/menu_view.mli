open! Core
open! Import

(** Menus drawn as loon's DropdownMenu (its [data-slot]s; style.css draws
    the hook classes [menu-surface], [menu-section], [menu-heading],
    [menu-item], [menu-command]): pure views, the state (open, the item
    under the keys) in App. Every menu of the page uses these: context
    menus, the field menu, the chips', the account's. *)

(** The clear layer behind an open menu: a click or a right-click on it
    (not on the menu) runs [on_close]. *)
val backdrop : ?cls:string -> on_close:unit Effect.t -> Node.t list -> Node.t

(** What the items float on ([role=menu]); [cls] adds hook names and
    placement. *)
val surface
  :  ?cls:string
  -> ?attrs:Attr.t list
  -> label:string
  -> Node.t list
  -> Node.t

(** A group of items, apart from the one before by a separator. *)
val section : ?cls:string -> Node.t list -> Node.t

(** A group's label. *)
val heading : string -> Node.t

(** An item: [icon], [label] (with [hint] under it: why it is disabled),
    [shortcut] at the right (a key or a command). [selected] is the one
    the keys are on; [danger] destructive; [disabled] dimmed and
    [aria-disabled]. [attrs] carries its handlers and [data-item]. *)
val item
  :  ?cls:string
  -> ?selected:bool
  -> ?danger:bool
  -> ?disabled:bool
  -> ?icon:Html.Icon.t
  -> ?hint:string
  -> ?shortcut:string
  -> ?attrs:Attr.t list
  -> string
  -> Node.t

(** The classes of an item ([menu-item]'s look), for an item built by hand
    (a checkbox item). *)
val item_look : string

(** An item's label: one line, cut with an ellipsis. *)
val label_look : string
