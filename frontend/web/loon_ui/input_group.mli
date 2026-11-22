(** Display additional information or actions to an input or textarea.
    Port of shadcn/ui's [InputGroup]: a bordered row (or column) holding a
    borderless {!Input} or {!Textarea} and addons of icons, text and
    buttons.

    {[
      Input_group.root
        [ Input_group.input ~placeholder:"Search..." ()
        ; Input_group.addon [ Icon.view Search ]
        ]
    ]} *)

open! Import

module Align : sig
  type t =
    | Inline_start (** before the control, in the row *)
    | Inline_end
    | Block_start (** above the control, full width *)
    | Block_end
  [@@deriving enumerate, sexp_of, compare, equal]

  (** As in [data-align]: [inline-start], ... *)
  val name : t -> string
end

module Button_size : sig
  type t =
    | Xs
    | Sm
    | Icon_xs
    | Icon_sm
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** The group ([role="group"]). [disabled] sets [data-disabled="true"],
    which fades the addons. *)
val root : ?attrs:Attr.t list -> ?class_:string -> ?disabled:bool -> Node.t list -> Node.t

(** Icons, text or buttons beside the control; a click on it (not on a
    button) focuses the group's input. *)
val addon
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?align:Align.t
  -> Node.t list
  -> Node.t

(** A small {!Button} for an addon ([Ghost], [Xs] by default). *)
val button
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Button.Variant.t
  -> ?size:Button_size.t
  -> ?disabled:bool
  -> ?type_:[ `Button | `Submit | `Reset ]
  -> ?on_click:unit Effect.t
  -> Node.t list
  -> Node.t

(** Text in an addon. *)
val text : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** The group's {!Input}, without its own border. *)
val input
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?type_:Input.Type.t
  -> ?id:string
  -> ?name:string
  -> ?placeholder:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?value:string
  -> ?on_input:(string -> unit Effect.t)
  -> unit
  -> Node.t

(** The group's {!Textarea}, without its own border. *)
val textarea
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?name:string
  -> ?placeholder:string
  -> ?rows:int
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?value:string
  -> ?on_input:(string -> unit Effect.t)
  -> unit
  -> Node.t
