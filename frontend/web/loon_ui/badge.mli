(** Displays a badge or a component that looks like a badge. Port of
    shadcn/ui's [Badge]. *)

open! Import

module Variant : sig
  type t =
    | Default
    | Secondary
    | Destructive
    | Outline
    | Ghost
    | Link
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** shadcn's [badgeVariants]. *)
val classes : ?variant:Variant.t -> ?class_:string -> unit -> string

(** A [<span>]. *)
val view : ?attrs:Attr.t list -> ?class_:string -> ?variant:Variant.t -> Node.t list -> Node.t

(** An [<a>] that looks like a badge (and darkens on hover): shadcn's
    [<Badge asChild><a>]. *)
val link
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> href:string
  -> Node.t list
  -> Node.t
