open! Core
open! Import

(** The current session's controls over the chat, which has no header: they
    float over its top. *)

(** Over the top of the chat: the sidebar's toggle at the left (when the
    sidebar is not docked), the status's island in the middle (with
    {!Status_style.Island}), and at the right what applies
    ({!Status_view.notes}) and the panels' toggles (with the settings when
    the sidebar is not docked); on a phone those are in a menu behind one
    button. The session's name is the sidebar's and the tab's title. *)
val chrome : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The session's name, else its description or first prompt, else
    ["New session"]. *)
val session_title : App.Model.t -> State.t -> string
