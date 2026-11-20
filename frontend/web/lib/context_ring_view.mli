open! Core
open! Import

(** The composer's context ring, beside the paperclip: how full
    the context is, amber as it nears the point where it compacts and red
    from there; it turns while the agent works. Pointed at, focused or
    tapped, it shows the session's card above it: the context against its
    window and where it compacts, the cost (the session's, the last turn's,
    the cache's hit rate), the activity, thinking, mode, model and branch,
    and a way to compact (Esc closes it). *)

(** The percentage of the window at which the context compacts:
    [compact_at_tokens] or 80% of the window, whichever is less. *)
val compacts_at : App.Model.t -> State.t -> float

(** [""], ["warm"] within 15 points of {!compacts_at}, ["hot"] from it. *)
val level : compacts_at:float -> float -> string

(** What the last turn cost: its replies' usage at the model's prices. *)
val last_turn_usd : App.Model.t -> State.t -> float option

(** The card's id, which the ring names as its description. *)
val card_id : string

val card
  :  App.Model.t
  -> State.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
