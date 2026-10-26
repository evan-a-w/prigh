open! Core
open! Import

(** The safety net for worktrees made or removed by hand: a [bash] command
    that runs [git worktree add] (or [remove], [move], [prune]) is wrapped
    in listings of the repository's worktrees, before and after, on the tool
    host, and what changed is announced with [Agent_event.Worktrees_changed]
    for the session to record. A new one outside [worktrees_dir] gets a note
    in the result telling the model to use the [worktree] tool. *)

(** Whether [command] runs [git worktree add] (remove, move, prune): cheap,
    on its text. *)
val applies : string -> bool

(** Runs [run] (the [bash] call, in [context]'s cwd) between the listings,
    when [applies] to its [command]; [dir] is the config's [worktrees_dir]. *)
val around
  :  Tool.Context.t
  -> dir:string
  -> arguments:Json.t
  -> (unit -> Tool_result.t)
  -> Tool_result.t
