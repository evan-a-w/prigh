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

(** {!Tilde.of_path}: [HOME/x] as [~/x], [host_home] being the home directory
    of the tool host the path is on. *)
val home_relative : ?host_home:string -> string -> string

(** The home directory of the model's session's active tool host, when it
    said. *)
val state_host_home : App.Model.t -> string option

(** The home directory of the tool host where [s] works, when it is
    connected. *)
val session_host_home : App.Model.t -> Session_summary.t -> string option

(** Where a session works: its repository and branch, else its directory
    ([~] being [host_home]). *)
val place
  :  ?cls:string
  -> ?host_home:string
  -> Session_summary.t
  -> cwd:string
  -> Node.t
