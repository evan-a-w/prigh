(** An input where the user selects a value from within a given range. Port
    of shadcn/ui's [Slider] (Radix Slider: one thumb per value, so two make
    a range; drag a thumb or click the track; arrow keys step, Shift+arrow
    and Page Up/Down step by ten, Home and End go to the ends; thumbs that
    cross swap places).

    {[
      Slider.component ~default_value:[ 50. ] ~max:100. ~step:1.
        ~class_:"w-[60%]" graph
    ]} *)

open! Import

module Orientation : sig
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** Controlled: one thumb per value (in increasing order). [min] is 0,
    [max] 100 and [step] 1 by default; [on_change] gets the values after
    each move, sorted and on a step. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?min:float
  -> ?max:float
  -> ?step:float
  -> ?min_steps_between_thumbs:int
  -> ?orientation:Orientation.t
  -> ?disabled:bool
  -> value:float list
  -> on_change:(float list -> unit Effect.t)
  -> unit
  -> Node.t

(** Keeps the values, [default_value] ([[min; max]]) at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?min:float
  -> ?max:float
  -> ?step:float
  -> ?min_steps_between_thumbs:int
  -> ?orientation:Orientation.t
  -> ?disabled:bool
  -> ?default_value:float list
  -> local_ Bonsai.graph
  -> float list Control.t Bonsai.t

(** [update values ~index ~value ~min ~max ~step ~min_steps_between_thumbs]:
    the values after thumb [index] moves to [value] (on a step, clamped,
    sorted), and the index the thumb has then; [None] if the thumbs would be
    too close. *)
val update
  :  float list
  -> index:int
  -> value:float
  -> min:float
  -> max:float
  -> step:float
  -> min_steps_between_thumbs:int
  -> (float list * int) option
