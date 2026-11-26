open! Core

module Position = struct
  type t =
    { top : float
    ; height : float
    ; client : float
    }
  [@@deriving sexp_of]
end

let from_end ({ top; height; client } : Position.t) = height -. client -. top
let at_end p = Float.(from_end p < 40.)
let off_end p = Float.(from_end p >= 1.)

module Turn_up = struct
  (* Frames for a scroll to land: a wheel's scroll shows within a frame or
     two, a smooth one starting at the next. *)
  let frames = 4

  type t =
    { top : float
    ; frames_left : int
    }
  [@@deriving sexp_of]

  module Outcome = struct
    type nonrec t =
      | Taken
      | Waiting of t
      | Not_taken
    [@@deriving sexp_of]
  end

  let start ~top = { top; frames_left = frames }
  let again t = { t with frames_left = frames }

  let frame t ~top ~held : Outcome.t =
    if Float.(top < t.top -. 0.5)
    then Taken
    else if held
    then Waiting t
    else if t.frames_left <= 1
    then Not_taken
    else Waiting { t with frames_left = t.frames_left - 1 }
  ;;
end
