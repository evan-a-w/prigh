(** For sighted users to preview content available behind a link. Port of
    shadcn/ui's [HoverCard] (Radix HoverCard: the pointer resting on the
    trigger for [open_delay] ms, or focus on it, opens it; leaving the
    trigger and the content for [close_delay] ms closes it; Esc or a click
    outside closes it).

    {[
      Hover_card.component
        (return (fun card ->
          [ Hover_card.trigger card
              (Button.view ~variant:Link [ Node.text "@nextjs" ])
          ; Hover_card.content card ~class_:"w-80" [ ... ]
          ]))
        graph
    ]} *)

open! Import

(** The card's state, for its parts. *)
type t

(** Controlled. [id] (unique on the page) prefixes the parts' ids;
    [open_delay] (700) and [close_delay] (300) are in milliseconds. *)
val view
  :  ?id:string
  -> ?open_delay:float
  -> ?close_delay:float
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps whether it is open ([default_open], [false] by default). *)
val component
  :  ?default_open:bool
  -> ?open_delay:float
  -> ?close_delay:float
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The element it previews (usually a link), with the trigger's
    attributes: Radix's [<HoverCardTrigger asChild>]. *)
val trigger : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The card, shown while open ([side] [Bottom], [side_offset] 4, [align]
    [Center] by default). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side:Popper.Side.t
  -> ?side_offset:float
  -> ?align:Popper.Align.t
  -> ?align_offset:float
  -> ?avoid_collisions:bool
  -> t
  -> Node.t list
  -> Node.t
