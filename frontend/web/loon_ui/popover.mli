(** Displays rich content in a portal, triggered by a button. Port of
    shadcn/ui's [Popover] (Radix Popover: the trigger toggles it; focus
    moves into the content when it opens and back to the trigger when it
    closes; Esc, a click outside or focus leaving it closes it).

    {[
      Popover.component
        (return (fun popover ->
          [ Popover.trigger popover
              (Button.view ~variant:Outline [ Node.text "Open popover" ])
          ; Popover.content popover ~class_:"w-80" [ ... ]
          ]))
        graph
    ]} *)

open! Import

(** The popover's state, for its parts. *)
type t

(** Controlled: [open_] and [on_open_change]. [id] (unique on the page)
    prefixes the parts' ids ([trigger_id], if given, is the trigger's: for a
    {!Label}'s [for_]). [modal] (default [false]): while open, the rest
    of the page ignores the pointer and focus stays inside. *)
val view
  :  ?id:string
  -> ?trigger_id:string
  -> ?modal:bool
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps whether it is open ([default_open], [false] by default). *)
val component
  :  ?default_open:bool
  -> ?trigger_id:string
  -> ?modal:bool
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

val is_open : t -> bool
val set_open : t -> bool -> unit Effect.t

(** The element that toggles it (usually a [Button.view]), with the
    trigger's attributes: Radix's [<PopoverTrigger asChild>]. *)
val trigger : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The trigger's id, to name it from elsewhere (e.g. [aria-labelledby]). *)
val trigger_id : t -> string

(** The content ([role="dialog"]), shown while open, beside the trigger
    ([side] [Bottom], [side_offset] 4, [align] [Center] by default; it flips
    when there is no room). *)
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

val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val title : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val description : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
