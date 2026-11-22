(** A visually persistent menu common in desktop applications that
    provides quick access to a consistent set of commands. Port of
    shadcn/ui's [Menubar] (Radix Menubar: ArrowLeft and ArrowRight move
    between the triggers, and between the open menus; Enter, Space or
    ArrowDown opens a menu; once one is open, the pointer over another
    trigger opens that one; inside a menu the keyboard works as in
    {!Dropdown_menu}).

    {[
      Menubar.component
        (return
           [ Menubar.menu
               (Menubar.trigger [ Node.text "File" ])
               (Menubar.content
                  [ Menubar.item [ Node.text "New Tab" ] ])
           ])
        graph
    ]} *)

open! Import

module State : sig
  (** The open menu's value, the trigger that has (or last had) the focus,
      and the open menu's highlight. *)
  type t =
    { value : string option
    ; current : string option
    ; menu : Menu.State.t
    }
  [@@deriving sexp_of, equal]

  val closed : t
end

module Trigger : sig
  type t
end

module Content : sig
  type t
end

module Menu_ : sig
  type t
end

(** A trigger: a button in the bar. *)
val trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> Node.t list
  -> Trigger.t

(** A menu's content, below its trigger ([side_offset] 8, [align] [Start],
    [align_offset] -4 by default). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side:Popper.Side.t
  -> ?side_offset:float
  -> ?align:Popper.Align.t
  -> ?align_offset:float
  -> ?avoid_collisions:bool
  -> Menu.Item.t list
  -> Content.t

(** shadcn's [MenubarMenu]. [value] (default: its index) names it in the
    state. *)
val menu : ?value:string -> Trigger.t -> Content.t -> Menu_.t

(** Controlled. [id] (unique on the page) prefixes the parts' ids. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> state:State.t
  -> on_change:(State.t -> unit Effect.t)
  -> Menu_.t list
  -> Node.t

(** Keeps the state, every menu closed at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> Menu_.t list Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** {2 The items} (see {!Menu} for [text_value] and [close_on_select]) *)

val group : ?attrs:Attr.t list -> Menu.Item.t list -> Menu.Item.t

val label
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?inset:bool
  -> Node.t list
  -> Menu.Item.t

(** [variant] [Destructive] colours it red. *)
val item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?inset:bool
  -> ?variant:Menu.Variant.t
  -> ?disabled:bool
  -> ?text_value:string
  -> ?close_on_select:bool
  -> ?on_select:unit Effect.t
  -> ?href:string
  -> Node.t list
  -> Menu.Item.t

val checkbox_item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> ?text_value:string
  -> ?close_on_select:bool
  -> checked:bool
  -> on_checked_change:(bool -> unit Effect.t)
  -> Node.t list
  -> Menu.Item.t

val radio_group
  :  ?attrs:Attr.t list
  -> value:string
  -> on_value_change:(string -> unit Effect.t)
  -> Menu.Item.t list
  -> Menu.Item.t

val radio_item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> ?text_value:string
  -> ?close_on_select:bool
  -> value:string
  -> Node.t list
  -> Menu.Item.t

val separator : ?attrs:Attr.t list -> ?class_:string -> unit -> Menu.Item.t

(** A keyboard shortcut at the end of an item, e.g. [⌘T]. *)
val shortcut : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

val sub : Menu.Sub_trigger.t -> Menu.Sub_content.t -> Menu.Item.t

val sub_trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?inset:bool
  -> ?disabled:bool
  -> ?text_value:string
  -> Node.t list
  -> Menu.Sub_trigger.t

val sub_content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side_offset:float
  -> ?align_offset:float
  -> Menu.Item.t list
  -> Menu.Sub_content.t
