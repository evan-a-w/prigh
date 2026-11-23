(** A set of layered sections of content, known as tab panels, that are
    displayed one at a time. Port of shadcn/ui's [Tabs] (Radix Tabs'
    behaviour: arrow keys move between the triggers and select them, Home
    and End go to the first and last, disabled triggers are skipped).

    {[
      Tabs.component ~default_value:"account"
        (return (fun tabs ->
          [ Tabs.list tabs
              [ Tabs.trigger ~value:"account" [ Node.text "Account" ]
              ; Tabs.trigger ~value:"password" [ Node.text "Password" ]
              ]
          ; Tabs.content tabs ~value:"account" [ ... ]
          ; Tabs.content tabs ~value:"password" [ ... ]
          ]))
        graph
    ]} *)

open! Import

module Orientation : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]
end

module List_variant : sig
  type t =
    | Default
    | Line
  [@@deriving enumerate, sexp_of, compare, equal]
end

(** The tabs' state, for its parts: the selected value and how to change
    it. *)
type t

module Trigger : sig
  type t
end

(** Controlled: [value] is the selected tab's; [id] (unique on the page,
    ["tabs"] by default) prefixes the parts' ids. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Orientation.t
  -> ?id:string
  -> value:string
  -> on_change:(string -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps the selected value, [default_value] at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?orientation:Orientation.t
  -> default_value:string
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The row of triggers ([role="tablist"]). *)
val list
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:List_variant.t
  -> t
  -> Trigger.t list
  -> Node.t

val trigger
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> value:string
  -> Node.t list
  -> Trigger.t

(** The panel shown while [value] is selected (empty and hidden otherwise). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> t
  -> value:string
  -> Node.t list
  -> Node.t
