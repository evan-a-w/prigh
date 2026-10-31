open! Core
open! Import

(** [repo]: the working directory's repository on its git host, through the
    user's sign-in ({!Forges}): [view] (description, default branch, stars,
    README) and its CI: [runs], [run] (jobs and steps), [logs] (a job's
    log's end) and [rerun]. Forgejo lists Actions runs but has no API for
    jobs, logs or re-runs. Only [rerun] changes anything. *)

val name : string
val changes : string -> bool
val create : forges:Forges.t -> unit -> Tool.t
