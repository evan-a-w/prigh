open! Core
open! Import

(** The id of the field menu's surface ([Command.Place_field_menu] places
    it). *)
val id : string

(** A text field's menu ({!Field_menu}), styled as the context menus
    ({!Context_menu_view}): its items with their icons and keys, the
    disabled ones dimmed; a click outside closes it. Over any dialog. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
