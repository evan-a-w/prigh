open! Core
open! Import

(** The current session at a glance, in one place at a time
    ([App.Model.docked] picks which). *)

(** In the docked sidebar, above the account: the title (click to rename)
    and the panels' toggles, the cwd and branch, where tools run (opening
    the host picker), what applies, and last the status ([Status_view]). *)
val card : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** Over the chat otherwise (sidebar closed, or a phone): one slim line with
    the sidebar's toggle and the title, then what applies, the status, the
    panels' toggles and the settings. *)
val bar : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
