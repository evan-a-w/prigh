open! Core
open! Import

(** A composer chip's menu ([Dialog.Popover]), drawn over its chip: the
    choices with the current one checked; the model's is
    {!Model_menu_view}. *)
val view
  :  App.Model.t
  -> chip:Chip.t
  -> Picker.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** Under the menu and the chips, over the rest of the page: a click on it
    closes the menu. *)
val backdrop : inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The transcript's verbosity as a row of choices, and the usage (the
    phone's ⋯ sheet). *)
val session_sections
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
