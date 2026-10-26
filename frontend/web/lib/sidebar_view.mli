open! Core
open! Import

(** Sessions: search, new, the sessions (current highlighted, live and
    running marked, age, repository and branch, size), each with the
    worktrees that matter to it under it (folding; changes, sync, a
    subagent's or a pull request's), context menus on both (right-click,
    Shift+F10, a long press, ⋯), the archived ones, delete; then [session],
    the current session's card; the signed-in user, help, the settings and
    sign out. *)
val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> session:Node.t
  -> Node.t

(** The gear opening the settings ([/settings]). *)
val settings : inject:(App.Action.t -> unit Effect.t) -> Node.t

(** [/home/USER/x] as [~/x]. *)
val home_relative : string -> string
