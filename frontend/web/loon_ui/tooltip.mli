(** A popup that displays information related to an element when the
    element receives keyboard focus or the mouse hovers over it. Port of
    shadcn/ui's [Tooltip] (Radix Tooltip, with shadcn's delay of 0: the
    pointer over the trigger or keyboard focus on it opens it; leaving both
    the trigger and the content, blur, a press on the trigger or Esc closes
    it; the trigger is described by the tooltip's text while it is open).

    {[
      Tooltip.component
        (return (fun tooltip ->
          [ Tooltip.trigger tooltip (Button.view ~variant:Outline [ Node.text "Hover" ])
          ; Tooltip.content tooltip [ Node.p [ Node.text "Add to library" ] ]
          ]))
        graph
    ]} *)

open! Import

(** The tooltip's state, for its parts. *)
type t

(** Controlled. [id] (unique on the page) prefixes the parts' ids. *)
val view
  :  ?id:string
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps whether it is open ([default_open], [false] by default). *)
val component
  :  ?default_open:bool
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The element it describes (usually a [Button.view]), with the trigger's
    attributes: Radix's [<TooltipTrigger asChild>]. *)
val trigger : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The tooltip with its arrow, shown while open ([side] [Top], [side_offset]
    0, [align] [Center] by default; it flips when there is no room). *)
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
