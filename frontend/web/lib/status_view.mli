open! Core
open! Import

(** The session's status: what the agent is doing, how full the context is,
    the cost, and one card of details (the tokens, the exact cost, the
    thinking level, the mode, the model, the branch) that shows over the
    composer's status chip while pointed at or focused ({!composer_status}). *)

(** ["12k"], ["1.2M"]. *)
val tokens : int -> string

(** How full the context is, in percent; [None] while the model's window is
    unknown. *)
val context_percent : State.t -> float option

(** [""], ["warm"] from 60%, ["hot"] from 85%. *)
val context_level : float -> string

(** The agent is working or a prompt is on its way: what lights the status
    chip's lamp. *)
val busy : App.Model.t -> bool

(** What the agent is doing, in a word or two: ["Ready"], ["Running
    tools"], ["Reconnecting…"]. *)
val activity_word : App.Model.t -> string

(** What the agent is doing as a status item: a dot when idle (a lamp while
    it works or the backend is out of reach) and {!activity_word}, announced
    politely as it changes ([role=status]). *)
val activity : App.Model.t -> Node.t

(** The cost to the cent; spent / budget (warm from 80%, hot once spent)
    with a budget, opening [/budget]. *)
val cost : State.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** A short thin bar filled to [percent], coloured by its level. *)
val meter : float -> Node.t

(** A ring filled to [percent] (a conic gradient: CSS draws it), coloured by
    its level, round [children]. *)
val ring : ?cls:string -> float -> Node.t list -> Node.t

(** The id of the details' card, which what shows it names as its
    description ([aria-describedby]). *)
val details_id : string

(** The details' card: context, tokens, cost, thinking, mode, model and
    branch, and a hint (a click opens [/session]). One at a time on the
    page: {!gauge} holds it. *)
val details : App.Model.t -> State.t -> Node.t

(** A button that opens the session's details ([/session]), named for how
    full the context is and described by {!details}. *)
val context_button
  :  ?cls:string
  -> State.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t list
  -> Node.t

(** [children] and, over them while pointed at or while one of them has the
    keyboard's focus, the details' card ([cls] places it, e.g. [above] or
    [below]). *)
val gauge : ?cls:string -> App.Model.t -> State.t -> Node.t list -> Node.t

(** What applies, only while it does: the goal, reminders, a resume after a
    usage limit, a Nix collection, transfers and the model's plan; each opens its command. *)
val notes : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The composer's status chip, at the right of its row by the microphone:
    the activity's lamp (its word while the agent works), the context's ring
    and percent (a click opens the session's details, [/session]) and the
    cost, under the details' card while pointed at or focused. *)
val composer_status : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
