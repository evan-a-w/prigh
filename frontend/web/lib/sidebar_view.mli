open! Core
open! Import

(** Sessions: search, new, the tree of projects (each folding; a new
    session or worktree in it), their worktrees (changes, sync, a subagent's
    with the session it came from; a new session there, remove) and their
    sessions (current highlighted, live and running marked, age, the
    subdirectory, size), the archived ones, delete; the signed-in user and
    sign out. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** [/home/USER/x] as [~/x]. *)
val home_relative : string -> string
