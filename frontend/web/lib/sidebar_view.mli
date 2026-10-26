open! Core
open! Import

(** Sessions: search, new, the list (current highlighted, live and running
    marked, age, cwd, size), delete; the repository's worktrees
    (each with its changes, sessions and actions); then [session], the current session's
    card; the signed-in user, help, the settings and sign out. *)
val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> session:Node.t
  -> Node.t

(** The gear opening the settings ([/settings]). *)
val settings : inject:(App.Action.t -> unit Effect.t) -> Node.t

(** [/home/USER/x] as [~/x]. *)
val home_relative : string -> string
