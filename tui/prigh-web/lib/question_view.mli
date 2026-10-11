open! Core
open! Import

(** The oldest [ask_user] call's dialog: its current question, the choices
    (radio buttons, or checkboxes when several may be picked) and the user's
    own answer, with buttons to move between the questions. *)
val view
  :  Question_dialog.t list
  -> inject:(Question_dialog.Action.t -> unit Effect.t)
  -> Node.t
