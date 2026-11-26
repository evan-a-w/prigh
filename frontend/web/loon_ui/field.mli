(** Combine labels, controls, and help text to compose accessible form
    fields and grouped inputs. Port of shadcn/ui's [Field] family: [root]
    is [Field], [label] is [FieldLabel], and so on.

    {[
      Field.set
        [ Field.legend [ Node.text "Profile" ]
        ; Field.group
            [ Field.root
                [ Field.label ~for_:"name" [ Node.text "Name" ]
                ; Input.view ~id:"name" ()
                ; Field.description [ Node.text "Your full name." ]
                ]
            ]
        ]
    ]} *)

open! Import

module Orientation : sig
  type t =
    | Vertical (** the label above the control *)
    | Horizontal (** side by side: checkboxes, switches *)
    | Responsive (** vertical, side by side in a wide [group] *)
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Legend_variant : sig
  type t =
    | Legend
    | Label
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** A [<fieldset>]: related fields, with a [legend]. *)
val set : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** The [<legend>] of a [set]; [Label] makes it the size of a label. *)
val legend
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Legend_variant.t
  -> Node.t list
  -> Node.t

(** Stacks fields; a container, so [Responsive] fields inside go side by
    side when it is wide. *)
val group : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** One field ([role="group"]): a label, a control, a description, an
    error. [invalid] sets [data-invalid="true"] (the field turns
    destructive), [disabled] [data-disabled="true"]. *)
val root
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Orientation.t
  -> ?invalid:bool
  -> ?disabled:bool
  -> Node.t list
  -> Node.t

(** Stacks a label and a description beside a control in a horizontal
    field. *)
val content : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** A {!Label} for a field's control; wrapping a whole [root] it makes a
    choice card. *)
val label : ?attrs:Attr.t list -> ?class_:string -> ?for_:string -> Node.t list -> Node.t

(** A title that is not a [<label>] (e.g. inside a choice card). *)
val title : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** Help text ([<p>]). *)
val description : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** A line between sections, with optional text in the middle. *)
val separator : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** An error ([role="alert"]): its children, else [errors] (duplicates
    dropped; several as a list). Nothing when both are empty. *)
val error
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?errors:string list
  -> Node.t list
  -> Node.t
