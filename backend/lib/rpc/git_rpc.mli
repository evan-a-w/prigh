open! Core
open! Import

(** The review's git actions ({!Git_actions}) on the calling client's
    session's active tool host, in the worktree [dir] (relative to the
    session's directory; default: the session's):

    - [git_status {dir?}]: {!Git_actions.Status}.
    - [git_stage {dir?, paths}], [git_unstage {dir?, paths}]: the status
      after; [paths] are relative to the worktree's top directory.
    - [git_commit {dir?, message, amend?, paths?}]: {!Git_actions.Commit};
      [paths] are staged first.
    - [git_push {dir?}]: {!Git_actions.Push}, to the branch's upstream (set
      when it has none: [origin], else the only remote). *)

val methods : string list

(** [None] for other methods. *)
val dispatch
  :  Agent.t
  -> meth:string
  -> params:Json.t
  -> Json.t Or_error.t option
