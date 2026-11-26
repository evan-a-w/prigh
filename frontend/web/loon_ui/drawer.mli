(** A drawer component for React. Port of shadcn/ui's [Drawer] (vaul,
    simplified: a modal dialog, as {!Dialog}, that slides in from an edge;
    from the bottom, the handle and the panel can be dragged down to close
    it). *)

open! Import

(** The drawer's state, for its parts. *)
type t

module Direction : sig
  type t =
    | Top
    | Bottom
    | Left
    | Right
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** Controlled. [id] (unique on the page) prefixes the parts' ids
    ([trigger_id], if given, is the trigger's: for a {!Label}'s [for_]);
    [direction] (default [Bottom]) is the edge it comes from. *)
val view
  :  ?id:string
  -> ?trigger_id:string
  -> ?direction:Direction.t
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps whether it is open ([default_open], [false] by default). *)
val component
  :  ?default_open:bool
  -> ?direction:Direction.t
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The element that opens it (vaul's [<DrawerTrigger asChild>]). *)
val trigger : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** An element that closes it (vaul's [<DrawerClose asChild>]). *)
val close : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The overlay and the panel, while open, with the handle when it comes
    from the bottom. *)
val content : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val footer : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val title : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

val description
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> t
  -> Node.t list
  -> Node.t

(** vaul's rules for dragging a bottom drawer (exposed for tests). *)
module Drag_rules : sig
  type release =
    | Close
    | Reset
  [@@deriving sexp_of]

  (** The panel's offset down and the overlay's opacity (unchanged when
      [None]) for a pointer [dragged_up] pixels up from the press (negative:
      down), the panel [height] tall. *)
  val follow : height:float -> dragged_up:float -> float * float option

  (** Letting go, the panel [swipe] down, the pointer [dragged_up] from the
      press [ms] ago: closes it a quarter of its height down or flicked down
      (faster than 0.4px/ms over the drag). *)
  val release : height:float -> swipe:float -> dragged_up:float -> ms:float -> release
end
