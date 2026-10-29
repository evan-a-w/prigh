open! Core
open! Import

(** The current session at a glance, in one place at a time
    ([App.Model.docked] picks which). *)

(** In the docked sidebar, above the account, two lines from the left
    edge ([Status_view]): the activity followed by what applies (but the
    plan), then the context's meter, the tokens and the cost; the branch
    (opening the pull requests) under them when there is one. The session
    list marks which session it is. *)
val card : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** With the card: the transcript filter's button and the panels' toggles
    (files, terminal, agents), floating
    in the chat's top right corner. *)
val tools : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** Over the chat otherwise (sidebar closed, or a phone): one slim line with
    the sidebar's toggle and the title, then what applies, the status, the
    panels' toggles and the settings. *)
val bar : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The session's name, else its description or first prompt, else
    ["New session"]. *)
val session_title : App.Model.t -> State.t -> string
