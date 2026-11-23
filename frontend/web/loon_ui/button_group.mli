(** A container that groups related buttons together with consistent
    styling. Port of shadcn/ui's [ButtonGroup] ([role="group"]: its
    children's inner corners and borders merge).

    {[
      Button_group.root
        [ Button.view ~variant:Outline [ Node.text "Archive" ]
        ; Button.view ~variant:Outline [ Node.text "Report" ]
        ]
    ]} *)

open! Import

module Orientation : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** shadcn's [buttonGroupVariants]: the classes of a group, then
    [class_]. *)
val classes : ?orientation:Orientation.t -> ?class_:string -> unit -> string

(** The group; nest groups to space them apart. *)
val root
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Orientation.t
  -> Node.t list
  -> Node.t

(** Text in the group, boxed like its buttons. *)
val text : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** A line between buttons ([Vertical] by default, for a horizontal
    group). *)
val separator
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Orientation.t
  -> unit
  -> Node.t
