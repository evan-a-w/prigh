open! Core
open! Import

(** What the user does to a [render_ui] component (prigh-web's
    [App.Action.Ui]). *)
type t =
  | Set of
      { key : string (** {!Ui_state.key} *)
      ; value : string
      }
  | Send of Ui_action.t (** a form, choices or checklist sent *)
[@@deriving sexp_of, equal]
