(** Displays a menu to the user, such as a set of actions or functions,
    triggered by a right click. Port of shadcn/ui's [ContextMenu] (Radix
    ContextMenu: a right click (the [contextmenu] event) in the trigger area
    opens the menu at the pointer; the keyboard works as in
    {!Dropdown_menu}).

    {[
      Context_menu.component
        (return (fun menu ->
          [ Context_menu.trigger menu ~class_:"..." [ Node.text "Right click here" ]
          ; Context_menu.content menu ~class_:"w-52"
              [ Context_menu.item [ Node.text "Back" ] ]
          ]))
        graph
    ]} *)

open! Import

module State : sig
  (** Open at a point of the viewport. *)
  type t =
    | Closed
    | Open of
        { x : float
        ; y : float
        ; menu : Menu.State.t
        }
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

(** Keeps the state, closed at first. *)
val component
  :  ?modal:bool
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The area that opens the menu on a right click (a [<span>]). *)
val trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> t
  -> Node.t list
  -> Node.t

(** The menu, shown while open, at the pointer (on its right, flipping to
    its left when there is no room). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
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

(** A keyboard shortcut at the end of an item, e.g. [⌘R]. *)
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
