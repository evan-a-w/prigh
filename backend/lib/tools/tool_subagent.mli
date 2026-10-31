open! Core
open! Import

(** A tool that delegates a task to a nested agent loop with its own context,
    tool set, model and turn budget. At depth 0 with [Tool.context.background] set it
    starts the agent in the background and returns its id; otherwise it blocks
    and returns the final reply. With [worktree] it works in a git worktree
    made for it on the tool host (under [worktrees_dir], default
    [~/.prigh/worktrees]), whose branch its report names. *)

val max_turns : int
val name : string

(** Whether a call's arguments ask for a worktree of its own. *)
val wants_worktree : Json.t -> bool

val create
  :  ?name_task:
       (task:string
        -> siblings:string list
        -> cancel:Cancellation.t
        -> string option)
       (** names a subagent the model gave no [name], given the names of
           the caller's other background subagents ({!Subagent_name});
           without it, or when it gives [None], the name comes from the
           task *)
  -> ?models:Model_registry.t (** what the [model] argument resolves against *)
  -> ?worktrees_dir:(unit -> string) (** the config's, when the tool runs *)
  -> ?compaction:(unit -> Compaction_settings.t)
       (** the session's, when the tool runs (default:
           {!Compaction_settings.window_only}) *)
  -> provider:Provider.t
  -> current_model:(unit -> Model.t)
  -> current_thinking:(unit -> Thinking.t)
  -> home:string
  -> unit
  -> Tool.t

(** [subagent_status] and [subagent_cancel], which act on
    the context's background subagents. *)
val control_tools : Tool.t list
