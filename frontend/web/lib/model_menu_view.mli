open! Core
open! Import

(** The model chip's menu ([Dialog.Popover] of [Chip.Model]): a rail of
    the favourites and the providers signed in; the search ("Search
    models…", across every provider) over the tab's models, each with its
    Alt+N, a star and a check on the current; the current model's
    reasoning levels; and the access mode in its foot. On a phone it is a
    sheet from the bottom with the reasoning under the list. *)
val view
  :  App.Model.t
  -> Picker.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** What the access button says for a mode: ["Full access"] (auto),
    ["Ask first"], ["Auto-edit"], ["Smart"], ["Plan"], ["Chat"]. *)
val access_label : Session_mode.t -> string
