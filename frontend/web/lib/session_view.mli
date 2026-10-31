open! Core
open! Import

(** The current session at a glance: the chat's header. *)

(** Over the chat, docked sidebar or not: one slim line with the title
    (renaming it) and the branch (opening the pull requests), then what
    applies and the status ({!Status_view}: the activity, the context's
    meter, the cost) at the right, then the panels' toggles. Without the
    docked sidebar, the sidebar's toggle and the settings too. The session
    list marks which session it is. *)
val bar : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The session's name, else its description or first prompt, else
    ["New session"]. *)
val session_title : App.Model.t -> State.t -> string
