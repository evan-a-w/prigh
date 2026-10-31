open! Core
open! Import

(** The [pull_request] tool: the model lists, views, creates and checks out
    pull requests of the working directory's repository through the
    backend's git-host sign-ins ({!Forges}, the user's or their namespace's),
    the way prigh-web's [/pr] dialogs do. It runs in the backend, where the
    credentials are; git (finding the repository, pushing, fetching into a
    worktree) runs on the session's tool host. Its [create] and [checkout]
    are destructive (they ask in [Ask] mode); a checkout's worktree is
    announced with [Agent_event.Worktrees_changed], so the session records
    it. *)

val name : string

(** Whether [action] changes anything ([create], [checkout]). *)
val changes : string -> bool

val create
  :  forges:Forges.t
  -> ?worktrees_dir:(unit -> string) (** the config's, when the tool runs *)
  -> unit
  -> Tool.t
