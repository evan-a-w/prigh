open! Core
open! Import

(** The [bash] tool. With [background] (at depth 0, when the context has
    background tasks) it starts a job that runs the command through the
    context's executor, i.e. on the active tool host, and returns at once.
    Without it, at depth 0, a command still running after
    {!Background_tasks.auto_background} seconds becomes such a job (its
    output so far kept, its process left running) and the call returns. *)

val tool : Tool.t

(** Saves a long output in full where the model can read it (under
    [$TMPDIR/prigh-output-<uid>/], a directory only this user can enter, in
    a file only they can read; kept for 7 days) and returns its path. Raises
    if that directory is a symlink or another user's. *)
val save_output : Tool.Context.t -> string -> string

(** [bash] without the [background] parameter, for subagents. *)
val foreground_tool : Tool.t

(** Whether this call starts a job rather than running the command. *)
val starts_job : Tool.Context.t -> Json.t -> bool

(** Whether this call must run in the backend rather than be forwarded to
    the tool host: it starts a job, or it is a foreground call at depth 0
    that is moved to the background (a job) if it is still running after
    {!Background_tasks.auto_background} seconds. Either way the command
    itself then runs through the context's executor, on the host. *)
val runs_in_backend : Tool.Context.t -> Json.t -> bool

(** The job id in the result of a call that was moved to the background. *)
val moved_job_id : string -> string option

(** Starts a job for [command]. [run] runs it in the foreground (with
    [foreground_arguments]), its stdin fed from [stdin] (the task's
    {!Background_tasks.Task.input}, for [job_input]; closed at once, and
    [job_input] refused, unless [keep_stdin]), and its output goes to
    [on_output]; the outcome's status is read from the result's trailer
    ([exited N], [killed], ...). *)
val start_job
  :  ?keep_stdin:bool
  -> Background_tasks.t
  -> command:string
  -> run:
       (id:string
        -> cancel:Cancellation.t
        -> stdin:Stdin_feed.t
        -> on_output:(string -> unit)
        -> Tool_result.t)
  -> string

(** The arguments of a background job's foreground run: [background] and
    [stdin] dropped, and no timeout unless one was given. *)
val foreground_arguments : Json.t -> Json.t

val started_message : id:string -> command:string -> string

(** The outcome of a finished job from its foreground result and output. *)
val job_outcome
  :  Tool_result.t
  -> output:Output_tail.t
  -> Background_tasks.Outcome.t

module For_testing : sig
  (** [save_output] in [tmp] rather than [$TMPDIR]. *)
  val save_output : tmp:string -> call_id:string -> string -> string
end
