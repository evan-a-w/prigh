open! Core
open! Import

(** The [bash] tool. With [background] (at depth 0, when the context has
    background tasks) it starts a job that runs the command through the
    context's executor, i.e. on the active tool host, and returns at once. *)

val tool : Tool.t

(** [bash] without the [background] parameter, for subagents. *)
val foreground_tool : Tool.t

(** Whether this call starts a job rather than running the command. The
    executor must then run it in the backend, not forward it to a host. *)
val starts_job : Tool.Context.t -> Json.t -> bool

(** Starts a job for [command]. [run] runs it in the foreground (with
    [foreground_arguments]) and its output goes to [on_output]; the outcome's
    status is read from the result's trailer ([exited N], [killed], ...). *)
val start_job
  :  Background_tasks.t
  -> command:string
  -> run:
       (id:string
        -> cancel:Cancellation.t
        -> on_output:(string -> unit)
        -> Tool_result.t)
  -> string

(** The arguments of a background job's foreground run: [background]
    dropped, and no timeout unless one was given. *)
val foreground_arguments : Json.t -> Json.t

val started_message : id:string -> command:string -> string

(** The outcome of a finished job from its foreground result and output. *)
val job_outcome
  :  Tool_result.t
  -> output:Output_tail.t
  -> Background_tasks.Outcome.t
