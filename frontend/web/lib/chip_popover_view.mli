open! Core
open! Import

(** A composer chip's menu ([Dialog.Popover]), drawn over its chip: the
    choices with the current one checked, the models filtered as typed and
    under their providers. *)
val view
  :  App.Model.t
  -> chip:Chip.t
  -> Picker.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** Under the menu and the chips, over the rest of the page: a click on it
    closes the menu. *)
val backdrop : inject:(App.Action.t -> unit Effect.t) -> Node.t
