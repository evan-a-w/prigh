open! Core
open! Import

(** The oldest [ask_user] call's dialog: its current question, the choices
    (radio buttons, or checkboxes when several may be picked) and the user's
    own answer, with buttons to move between the questions, and when the agent
    carries on without the answers. *)
val view
  :  Question_dialog.t list
  -> utc_offset:Time_ns.Span.t
  -> inject:(Question_dialog.Action.t -> unit Effect.t)
  -> Node.t
