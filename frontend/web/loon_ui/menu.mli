(** What Dropdown_menu, Context_menu and Menubar share: Radix's Menu. The
    items (plain, checkbox and radio items, labels, separators, groups and
    sub-menus) are values that the menu renders with the component's
    classes, so that it can move a highlight through them: ArrowUp/ArrowDown
    (not wrapping), Home and End, typeahead, Enter or Space to choose,
    ArrowRight to open a sub-menu and ArrowLeft to close it, Esc to close the
    menu; the pointer highlights the item under it and opens sub-menus. The
    highlighted item has [data-highlighted] and the focus (shadcn styles it
    with [focus:]). *)

open! Import

module Variant : sig
  type t =
    | Default
    | Destructive
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** A component's class strings (its own copy of shadcn's), and the prefix
    of its [data-slot]s and Radix CSS variables (["dropdown-menu"]). *)
module Classes : sig
  type t =
    { slot : string
    ; item : string
    ; checkbox_item : string
    ; radio_item : string
    ; checkbox_indicator : string (** the box of a checkbox item's mark *)
    ; radio_indicator : string
    ; indicator_slots : bool
      (** whether the boxes have [data-slot]s (["<slot>-checkbox-item-indicator"]) *)
    ; label : string
    ; separator : string
    ; shortcut : string
    ; sub_trigger : string
    ; sub_trigger_icon : string
    ; sub_content : string
    }
end

module Item : sig
  type t
end

module Sub_trigger : sig
  type t
end

module Sub_content : sig
  type t
end

(** {2 The parts, re-exported by each menu}

    [text_value] is the text typeahead matches (by default the item's text
    nodes). [close_on_select] (default [true]): choosing the item closes the
    menu, as Radix does unless [onSelect] prevents it. [href] makes an item
    an [<a>] (Radix's [asChild] over a link). *)

val item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?inset:bool
  -> ?variant:Variant.t
  -> ?disabled:bool
  -> ?text_value:string
  -> ?close_on_select:bool
  -> ?on_select:unit Effect.t
  -> ?href:string
  -> Node.t list
  -> Item.t

val checkbox_item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> ?text_value:string
  -> ?close_on_select:bool
  -> checked:bool
  -> on_checked_change:(bool -> unit Effect.t)
  -> Node.t list
  -> Item.t

val radio_group
  :  ?attrs:Attr.t list
  -> value:string
  -> on_value_change:(string -> unit Effect.t)
  -> Item.t list
  -> Item.t

(** [indicator]: the checked item's mark (by default a check). *)
val radio_item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?indicator:Node.t
  -> ?disabled:bool
  -> ?text_value:string
  -> ?close_on_select:bool
  -> value:string
  -> Node.t list
  -> Item.t

val label
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?inset:bool
  -> Node.t list
  -> Item.t

val separator : ?attrs:Attr.t list -> ?class_:string -> unit -> Item.t
val group : ?attrs:Attr.t list -> Item.t list -> Item.t

(** A sub-menu: its trigger (an item with a chevron) and its content, placed
    on the trigger's right. *)
val sub : Sub_trigger.t -> Sub_content.t -> Item.t

val sub_trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?inset:bool
  -> ?disabled:bool
  -> ?text_value:string
  -> Node.t list
  -> Sub_trigger.t

val sub_content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side_offset:float
  -> ?align_offset:float
  -> Item.t list
  -> Sub_content.t

(** Any node among the items, which the keyboard does not reach. *)
val node : Node.t -> Item.t

(** A keyboard shortcut at the end of an item. *)
val shortcut
  :  Classes.t
  -> ?attrs:Attr.t list
  -> ?class_:string
  -> Node.t list
  -> Node.t

(** {2 The menu} *)

(** The highlighted item and the open sub-menus, as paths of indices: each
    index counts the items (not labels or separators) of a level. *)
module State : sig
  type t =
    { highlighted : int list option
    ; open_sub : int list option
    }
  [@@deriving sexp_of, equal]

  val initial : t
end

(** A menu's state and what it does, for rendering its content. *)
type t =
  { id : string
  ; classes : Classes.t
  ; state : State.t
  ; set_state : State.t -> unit Effect.t
  ; close : unit Effect.t (** closes the whole menu *)
  ; on_edge : ([ `Left | `Right ] -> unit Effect.t) option
    (** ArrowLeft or ArrowRight with nothing to open or close (a menubar
        moves to the next menu) *)
  }

val trigger_id : string -> string
val content_id : string -> string

(** The state with the first enabled item highlighted (a menu opened from
    the keyboard), or the last (ArrowUp on a menubar trigger). *)
val first_highlighted : Item.t list -> State.t

val last_highlighted : Item.t list -> State.t

(** The top-level content: [role="menu"] with the items, placed by
    {!Popper} against [anchor], dismissed by Esc and a pointer down outside
    it but for the elements [inside] ({!Dismiss}; [modal] by default), its
    exit animated ({!Presence}). [labelled_by] (default [true]): labelled by
    the trigger, [trigger_id t.id]. [on_dismiss] replaces [t.close] when
    dismissed (to tell Escape from a click outside). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side:Popper.Side.t
  -> ?side_offset:float
  -> ?align:Popper.Align.t
  -> ?align_offset:float
  -> ?avoid_collisions:bool
  -> ?modal:bool
  -> ?inside:string list
  -> ?labelled_by:bool
  -> ?on_dismiss:(Dismiss.Reason.t -> unit Effect.t)
  -> base_class:string
  -> anchor:Popper.Anchor.t
  -> t
  -> Item.t list
  -> Node.t
