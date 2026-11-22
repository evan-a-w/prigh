(** Displays a menu to the user, such as a set of actions or functions,
    triggered by a button. Port of shadcn/ui's [DropdownMenu] (Radix
    DropdownMenu: the trigger opens it on click, or from the keyboard with
    Enter, Space or ArrowDown, highlighting the first item; arrows, Home,
    End and typeahead move through the items; ArrowRight and ArrowLeft open
    and close sub-menus; Enter or Space chooses; Esc or a click outside
    closes it and focus returns to the trigger).

    {[
      Dropdown_menu.component
        (return (fun menu ->
          [ Dropdown_menu.trigger menu
              (Button.view ~variant:Outline [ Node.text "Open" ])
          ; Dropdown_menu.content menu ~class_:"w-56" ~align:Start
              [ Dropdown_menu.label [ Node.text "My Account" ]
              ; Dropdown_menu.item ~on_select [ Node.text "Profile" ]
              ]
          ]))
        graph
    ]} *)

open! Import

module State : sig
  type t =
    | Closed
    | Open of Menu.State.t
  [@@deriving sexp_of, equal]

  val is_open : t -> bool
end

(** The menu's state, for its parts. *)
type t

(** Controlled. [id] (unique on the page) prefixes the parts' ids; [modal]
    (default [true]): while open, the rest of the page ignores the pointer
    and does not scroll. *)
val view
  :  ?id:string
  -> ?modal:bool
  -> state:State.t
  -> on_change:(State.t -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps the state: closed at first, or open with [default_open]. *)
val component
  :  ?default_open:bool
  -> ?modal:bool
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** Opens or closes the menu (for an item that opens a dialog, say). *)
val set_open : t -> bool -> unit Effect.t

(** The element that opens the menu (usually a [Button.view]), with the
    trigger's attributes added: Radix's [<DropdownMenuTrigger asChild>]. *)
val trigger : ?attrs:Attr.t list -> ?disabled:bool -> t -> Node.t -> Node.t

(** The menu, shown while it is open, beside the trigger ([side] [Bottom],
    [side_offset] 4, [align] [Center] by default; it flips to the other
    side when there is no room). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side:Popper.Side.t
  -> ?side_offset:float
  -> ?align:Popper.Align.t
  -> ?align_offset:float
  -> ?avoid_collisions:bool
  -> t
  -> Menu.Item.t list
  -> Node.t

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

(** A keyboard shortcut at the end of an item, e.g. [⇧⌘P]. *)
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
