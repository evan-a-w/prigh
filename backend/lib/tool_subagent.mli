open! Core
open! Import

(** A tool that delegates a task to a nested agent loop with its own context,
    tool set, model and turn budget. At depth 0 with [Tool.context.jobs] set it
    starts the agent in the background and returns its id; otherwise it blocks
    and returns the final reply. *)

val max_turns : int

val create
  :  provider:Provider.t
  -> current_model:(unit -> Model.t)
  -> current_thinking:(unit -> Thinking.t)
  -> home:string
  -> Tool.t

(** [subagent_wait], [subagent_status] and [subagent_cancel], which act on
    the context's background jobs. *)
val control_tools : Tool.t list
