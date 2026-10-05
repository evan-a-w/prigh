open! Core
open! Import

(** [job_status], [job_output], [job_wait] and [job_kill], which act on the
    context's background shell jobs (started by [bash] with [background]). *)

val tools : Tool.t list

(** Lines [offset + lines] to [offset] from the end of a job's retained
    output, under a one-line header. *)
val output_text
  :  Background_tasks.Task.t
  -> now:float
  -> lines:int
  -> offset:int
  -> string
