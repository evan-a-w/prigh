(** Content placed next to an anchor, as Radix's Popper (floating-ui): on
    a [side] of the anchor, [side_offset] pixels away, aligned to its
    [Start], [Center] or [End] ([align_offset] along that edge), flipped to
    the other side when it does not fit, shifted to stay in the viewport,
    and followed while the page scrolls or resizes. The content's
    [data-side] and [data-align] say where it went (for shadcn's
    [data-[side=bottom]:slide-in-from-top-2]), and CSS variables
    [--radix-<name>-content-transform-origin], [-content-available-height],
    [-content-available-width], [-trigger-width] and [-trigger-height] are
    set as Radix sets them. *)

open! Import

module Side : sig
  type t =
    | Top
    | Right
    | Bottom
    | Left
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Align : sig
  type t =
    | Start
    | Center
    | End
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** What the content is placed against: the element with an id, or a point
    in the viewport (a context menu's pointer). *)
module Anchor : sig
  type t =
    | Element of string
    | Point of
        { x : float
        ; y : float
        }
  [@@deriving sexp_of, equal]
end

(** The element Radix wraps popper content in (fixed, at least as wide as
    its content), with the hook that positions it. [name] picks the CSS
    variables' names (["popover"], ["dropdown-menu"], ...). The content
    (one element) should carry {!content_attrs}. *)
val wrapper
  :  ?side:Side.t
  -> ?side_offset:float
  -> ?align:Align.t
  -> ?align_offset:float
  -> ?avoid_collisions:bool
  -> ?collision_padding:float
  -> ?attrs:Attr.t list
  -> name:string
  -> anchor:Anchor.t
  -> Node.t
  -> Node.t

(** The content's [data-side] and [data-align] as asked for (the hook
    changes them when it flips the content). *)
val content_attrs : ?side:Side.t -> ?align:Align.t -> unit -> Attr.t list

(** Radix's [Arrow]: an SVG triangle placed on the content's edge facing
    the anchor; [class_] styles the [<svg>]. *)
val arrow : ?class_:string -> unit -> Node.t

(** Radix Select's item-aligned position: the content is placed over the
    trigger [anchor] so that its selected item (the element with
    [data-state="checked"], or the first item) lies over the trigger's
    value, and kept within the viewport. *)
val item_aligned_wrapper : ?attrs:Attr.t list -> name:string -> anchor:string -> Node.t -> Node.t
