(** A vertically stacked set of interactive headings that each reveal a
    section of content. Port of shadcn/ui's [Accordion] (Radix Accordion:
    one or several items open, Up/Down, Home and End between the triggers,
    the content's height animated with [animate-accordion-down]/[-up]).

    {[
      Accordion.component ~type_:Single ~collapsible:true ~default_value:[ "item-1" ]
        (return (fun acc ->
          [ Accordion.item acc ~value:"item-1"
              [ Accordion.trigger [ Node.text "Is it accessible?" ]
              ; Accordion.content [ Node.text "Yes. It adheres to the WAI-ARIA design pattern." ]
              ]
          ]))
        graph
    ]} *)

open! Import

module Type : sig
  (** [Single]: one item open at a time; [Multiple]: any number. *)
  type t =
    | Single
    | Multiple
  [@@deriving enumerate, sexp_of, compare, equal]
end

module Orientation : sig
  type t =
    | Vertical
    | Horizontal
  [@@deriving enumerate, sexp_of, compare, equal]
end

(** The accordion's state, for its items. *)
type t

(** A trigger or a content, placed in an {!item}. *)
module Part : sig
  type t
end

(** The open items' values after the item [value] is pressed, from [open_]
    ([collapsible]: a [Single] accordion's open item closes when pressed). *)
val toggled
  :  Type.t
  -> collapsible:bool
  -> open_:string list
  -> string
  -> string list

(** Controlled: [value] lists the open items' values (at most one for
    [Single]). [id] (unique on the page, ["accordion"] by default) prefixes
    the parts' ids. [orientation] (default [Vertical]) picks the arrow
    keys. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?collapsible:bool
  -> ?disabled:bool
  -> ?orientation:Orientation.t
  -> ?id:string
  -> type_:Type.t
  -> value:string list
  -> on_change:(string list -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps which items are open, [default_value] (none) at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?collapsible:bool
  -> ?disabled:bool
  -> ?orientation:Orientation.t
  -> ?default_value:string list
  -> type_:Type.t
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** An item: its {!trigger} and {!content}. *)
val item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> t
  -> value:string
  -> Part.t list
  -> Node.t

(** The heading's button, with a chevron that turns when open. *)
val trigger : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Part.t

(** The panel ([class_] goes on its inner [div], as in shadcn). *)
val content : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Part.t
