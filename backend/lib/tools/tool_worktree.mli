open! Core
open! Import

(** The [worktree] tool: the model makes, lists and removes git worktrees of
    the working directory's repository, on the session's tool host, the way
    [/worktree] does ({!Worktree}: new ones go under [worktrees_dir]). Those
    it makes or removes are announced with [Agent_event.Worktrees_changed],
    so the session records them (a subagent's as that subagent's). It never
    removes the main checkout, nor the worktree this agent or the session
    works in, nor (unless [force]) one with uncommitted changes. With
    [host] (or in a subagent working on another host) it acts on that
    host's repository, and the session does not record those worktrees. *)

val name : string

val create
  :  ?worktrees_dir:(unit -> string) (** the config's, when the tool runs *)
  -> ?session:(unit -> Session.t option)
       (** the agent's: whose cwd is kept, whose worktrees [list] marks *)
  -> ?hosts:(unit -> Tool_on_host.Host.t list)
       (** the session's connected tool hosts, for [host] *)
  -> unit
  -> Tool.t
