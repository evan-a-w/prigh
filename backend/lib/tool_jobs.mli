open! Core
open! Import

(** [job_status], [job_output], [job_wait], [job_input] and [job_kill], which
    act on the context's background shell jobs (started by [bash] with
    [background]). [job_input] writes to a job's stdin
    ({!Background_tasks.Task.input}), which reaches the process on whichever
    host runs it, and returns the output that follows. *)

val tools : Tool.t list

(** [tools] for subagents, whose [job_wait] says to block on jobs: they are
    not woken after their turn, and their jobs end with it. *)
val subagent_tools : Tool.t list

(** Lines [offset + lines] to [offset] from the end of a job's retained
    output, under a one-line header. *)
val output_text
  :  Background_tasks.Task.t
  -> now:float
  -> lines:int
  -> offset:int
  -> string
