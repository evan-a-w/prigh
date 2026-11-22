(** Visually or semantically separates content. Port of shadcn/ui's
    [Separator] (Radix Separator: [role="none"] while [decorative], the
    default; otherwise [role="separator"]). *)

open! Import

module Orientation : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  (** [horizontal], [vertical], as in [data-orientation]. *)
  val name : t -> string
end

val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Orientation.t
  -> ?decorative:bool
  -> unit
  -> Node.t
