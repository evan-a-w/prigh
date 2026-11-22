(** A versatile component that you can use to display any content. Port of
    shadcn/ui's [Item].

    {[
      Item.view ~variant:Outline
        [ Item.content
            [ Item.title [ Node.text "Basic Item" ]
            ; Item.description [ Node.text "A simple item with title and description." ]
            ]
        ; Item.actions [ Button.view ~variant:Outline ~size:Sm [ Node.text "Action" ] ]
        ]
    ]} *)

open! Import

module Variant : sig
  type t =
    | Default
    | Outline
    | Muted
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Size : sig
  type t =
    | Default
    | Sm
    | Xs
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Media_variant : sig
  type t =
    | Default
    | Icon
    | Image
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** shadcn's [itemVariants], for other elements that should look like an
    item. *)
val classes : ?variant:Variant.t -> ?size:Size.t -> ?class_:string -> unit -> string

(** A [<div>]. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> ?size:Size.t
  -> Node.t list
  -> Node.t

(** An [<a>] that is an item: shadcn's [<Item asChild><a>]. *)
val link
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> ?size:Size.t
  -> href:string
  -> Node.t list
  -> Node.t

(** Items in a column ([role="list"]). *)
val group : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** A horizontal separator between items in a group (a [Separator]'s DOM). *)
val separator : ?attrs:Attr.t list -> ?class_:string -> unit -> Node.t

val media
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Media_variant.t
  -> Node.t list
  -> Node.t

val content : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val title : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** A [<p>]. *)
val description : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

val actions : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val footer : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
