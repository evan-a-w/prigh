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

(** How hard the session's model thinks at a setting: a rising bar per level
    it takes, lit up to the setting's. *)
val meter : App.Model.t -> string -> Node.t

(** The session's settings as rows of choices that leave the rest open: the
    thinking level, the mode, the transcript's verbosity and the usage (the
    phone's ⋯ sheet, where the composer's row has no room for their chips). *)
val session_sections
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
