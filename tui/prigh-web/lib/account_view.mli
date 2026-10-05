open! Core
open! Import

(** The account switcher: the button at the bottom of the sidebar (who we are,
    and who we act as) and the menu it opens. *)

(** [None] when there is no account to show (an open backend). *)
val trigger
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t option

(** The account menu, from its [Dialog.Picker { kind = Accounts; _ }]. *)
val menu
  :  App.Model.t
  -> Picker.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
