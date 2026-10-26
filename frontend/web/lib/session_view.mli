open! Core
open! Import

(** The current session at a glance, in one place at a time
    ([App.Model.docked] picks which). *)

(** In the docked sidebar, above the account: what applies, then the status
    ([Status_view]) on a grid: the activity beside where tools run (opening
    the host picker) and the branch (opening the pull requests), the context
    across, the tokens and the cost. The session list marks which session it
    is. *)
val card : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** With the card: the panels' toggles (files, terminal, agents), floating
    in the chat's top right corner. *)
val tools : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** Over the chat otherwise (sidebar closed, or a phone): one slim line with
    the sidebar's toggle and the title, then what applies, the status, the
    panels' toggles and the settings. *)
val bar : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
