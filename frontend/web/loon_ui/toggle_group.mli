(** A set of two-state buttons that can be toggled on or off. Port of
    shadcn/ui's [ToggleGroup] (Radix Toggle Group: one pressed at most
    ([single]) or any number ([multiple]); arrow keys, Home and End move
    between the items).

    {[
      Toggle_group.multiple_component ~variant:Outline
        (return (fun group ->
          [ Toggle_group.item group ~value:"bold"
              ~attrs:[ Attr.create "aria-label" "Toggle bold" ]
              [ Icon.view Bold ]
          ; ...
          ]))
        graph
    ]} *)

open! Import

(** The group's state and styles, for its items. *)
type t

(** One pressed at most ([role="radio"] items): pressing it again unpresses
    it. *)
val single
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Toggle.Variant.t
  -> ?size:Toggle.Size.t
  -> ?spacing:int
  -> ?orientation:Roving.Orientation.t
  -> ?disabled:bool
  -> value:string option
  -> on_change:(string option -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Any number pressed ([aria-pressed] items); [value] in the order they
    were pressed. *)
val multiple
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Toggle.Variant.t
  -> ?size:Toggle.Size.t
  -> ?spacing:int
  -> ?orientation:Roving.Orientation.t
  -> ?disabled:bool
  -> value:string list
  -> on_change:(string list -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

val single_component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Toggle.Variant.t
  -> ?size:Toggle.Size.t
  -> ?spacing:int
  -> ?orientation:Roving.Orientation.t
  -> ?disabled:bool
  -> ?default_value:string
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> string option Control.t Bonsai.t

val multiple_component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Toggle.Variant.t
  -> ?size:Toggle.Size.t
  -> ?spacing:int
  -> ?orientation:Roving.Orientation.t
  -> ?disabled:bool
  -> ?default_value:string list
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> string list Control.t Bonsai.t

(** An item; the group's [variant] and [size] win over its own. *)
val item
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Toggle.Variant.t
  -> ?size:Toggle.Size.t
  -> ?disabled:bool
  -> t
  -> value:string
  -> Node.t list
  -> Node.t
