(** Augments native scroll functionality for custom, cross-browser
    styling. Port of shadcn/ui's [Scroll Area] (Radix ScrollArea: the
    viewport scrolls natively with its own scrollbars hidden, and thin
    scrollbars drawn over it show its position; drag a thumb, press a track
    to jump, or use the wheel over one).

    {[
      Scroll_area.root ~class_:"h-72 w-48 rounded-md border" [ ... ]
    ]}

    A horizontal scrollbar is added by putting
    [Scroll_area.scroll_bar ~orientation:Horizontal ()] among the children,
    as in shadcn. *)

open! Import

module Type : sig
  (** When the scrollbars show (Radix's [type]): [Hover] (default) while the
      pointer is over the area, [Scroll] while it scrolls, [Auto] whenever
      the content overflows, [Always] always. Except with [Always], a
      scrollbar shows only along an axis the content overflows. *)
  type t =
    | Auto
    | Always
    | Scroll
    | Hover
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Orientation : sig
  type t =
    | Vertical
    | Horizontal
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** The area: a viewport holding [children], and a vertical scrollbar.
    [scroll_hide_delay] (default 600ms): how long a [Hover] or [Scroll]
    scrollbar stays after the pointer leaves or scrolling stops. *)
val root
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?type_:Type.t
  -> ?scroll_hide_delay:Time_ns.Span.t
  -> Node.t list
  -> Node.t

(** A scrollbar ([Vertical] by default) of the enclosing [root]. *)
val scroll_bar
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Orientation.t
  -> unit
  -> Node.t
