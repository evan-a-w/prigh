(** Displays content within a desired ratio. Port of shadcn/ui's
    [AspectRatio] (Radix AspectRatio: a wrapper as wide as its container,
    as tall as [ratio] makes it, holding the content absolutely). *)

open! Import

(** [ratio] is width over height (default 1); [class_] and [attrs] go on
    the inner element, as Radix's props do. *)
val view : ?attrs:Attr.t list -> ?class_:string -> ?ratio:float -> Node.t list -> Node.t
