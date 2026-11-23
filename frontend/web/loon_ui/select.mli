(** Displays a list of options for the user to pick from—triggered by a
    button. Port of shadcn/ui's [Select] (Radix Select: Enter, Space or the
    arrows on the trigger open it, typing a letter on the closed trigger
    picks the next item starting with it; in the list the arrows, Home/End
    and typeahead move the highlight, Enter, Space or a click picks, Esc or
    a click outside closes and focus returns to the trigger. Its content is
    placed with the selected item over the trigger ([Item_aligned], the
    default) or below it ([Popper]); scroll buttons appear when the list
    overflows).

    {[
      Select.component
        (return (Select.trigger ~class_:"w-[180px]" ~placeholder:"Select a fruit" ()))
        (return
           (Select.content
              [ Select.group
                  [ Select.label [ Node.text "Fruits" ]
                  ; Select.item ~value:"apple" [ Node.text "Apple" ]
                  ]
              ]))
        graph
    ]} *)

open! Import

module Size : sig
  type t =
    | Default
    | Sm
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module Position : sig
  type t =
    | Item_aligned
    | Popper
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

module State : sig
  (** Whether the list is open, and its highlighted item's value. *)
  type t =
    { open_ : bool
    ; highlighted : string option
    }
  [@@deriving sexp_of, equal]

  val closed : t
end

(** A part of the list. *)
type part

val group : ?attrs:Attr.t list -> ?class_:string -> part list -> part
val label : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> part

(** An option; [text_value] (default: the children's text) is what
    typeahead matches. *)
val item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?text_value:string
  -> ?disabled:bool
  -> value:string
  -> Node.t list
  -> part

val separator : ?attrs:Attr.t list -> ?class_:string -> unit -> part

module Trigger : sig
  type t
end

module Content : sig
  type t
end

(** The button showing the selected item's children, or [placeholder]
    (shadcn's [SelectTrigger] with its [SelectValue]; [value_class] is the
    value's). [id] is its id (by default the select's, then [-trigger]),
    for a {!Label}'s [for_]. *)
val trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?size:Size.t
  -> ?placeholder:string
  -> ?value_class:string
  -> unit
  -> Trigger.t

(** The list ([position] [Item_aligned] by default; [side], [side_offset],
    [align] and [align_offset] place it with [Popper]). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?position:Position.t
  -> ?side:Popper.Side.t
  -> ?side_offset:float
  -> ?align:Popper.Align.t
  -> ?align_offset:float
  -> part list
  -> Content.t

(** Controlled. [id] (unique on the page) prefixes the parts' ids; with
    [name], a hidden native [<select>] carries the value in forms. *)
val view
  :  ?id:string
  -> ?name:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> value:string option
  -> on_value_change:(string -> unit Effect.t)
  -> state:State.t
  -> on_state_change:(State.t -> unit Effect.t)
  -> Trigger.t
  -> Content.t
  -> Node.t

(** Keeps the value (initially [default_value]) and the state; returns the
    value and the view. *)
val component
  :  ?name:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?default_value:string
  -> Trigger.t Bonsai.t
  -> Content.t Bonsai.t
  -> local_ Bonsai.graph
  -> (string option * Node.t) Bonsai.t
