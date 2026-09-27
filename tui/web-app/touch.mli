open! Core

(** Classifies one finger's touch sequence as a tap or a swipe, and turns a
    swipe into scroll steps as it goes. *)

type t [@@deriving sexp_of]

val start : x:float -> y:float -> t

(** Movement across less than this many pixels is still a tap. *)
val slop : float

(** [move t ~y ~step] returns the updated gesture and the number of scroll steps
    earned by the movement so far: positive when the finger moves up (revealing
    content below, i.e. scroll down), negative when it moves down. Sub-step
    remainders carry over to the next move. *)
val move : t -> x:float -> y:float -> step:float -> t * int

val finish : t -> [ `Tap | `Swipe ]
