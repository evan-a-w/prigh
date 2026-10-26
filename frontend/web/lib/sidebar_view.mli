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

(** [HOME/x] as [~/x], [home] being the home of the tool host the path is
    on; without it (unknown), any [/home/USER/x]. *)
val home_relative : ?home:string -> string -> string

(** The home of the model's session's active tool host, when it said. *)
val state_home : App.Model.t -> string option

(** The home of the tool host where [s] works, when it is connected. *)
val session_home : App.Model.t -> Session_summary.t -> string option

(** Where a session works: its repository and branch, else its directory
    ([~] being [home]). *)
val place
  :  ?cls:string
  -> ?home:string
  -> Session_summary.t
  -> cwd:string
  -> Node.t
