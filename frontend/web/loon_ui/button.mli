(** Displays a button or a component that looks like a button. Port of
    shadcn/ui's [Button]. *)

open! Import

module Variant : sig
  type t =
    | Default
    | Destructive
    | Outline
    | Secondary
    | Ghost
    | Link
  [@@deriving enumerate, sexp_of, compare, equal]

  (** shadcn's name, as in [data-variant]: [default], [destructive], ... *)
  val name : t -> string
end

module Size : sig
  type t =
    | Default
    | Xs
    | Sm
    | Lg
    | Icon
    | Icon_xs
    | Icon_sm
    | Icon_lg
  [@@deriving enumerate, sexp_of, compare, equal]

  (** shadcn's name, as in [data-size]: [default], [icon-sm], ... *)
  val name : t -> string
end

(** shadcn's [buttonVariants]: the classes of a button of this variant and
    size, then [class_], for other elements that should look like one. *)
val classes : ?variant:Variant.t -> ?size:Size.t -> ?class_:string -> unit -> string

(** A [<button>] ([type="button"] unless [type_] says otherwise). *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> ?size:Size.t
  -> ?disabled:bool
  -> ?type_:[ `Button | `Submit | `Reset ]
  -> ?on_click:unit Effect.t
  -> Node.t list
  -> Node.t

(** An [<a>] that looks like a button: shadcn's [<Button asChild><a>]. *)
val link
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> ?size:Size.t
  -> href:string
  -> Node.t list
  -> Node.t
