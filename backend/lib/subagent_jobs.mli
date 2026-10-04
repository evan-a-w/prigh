open! Core
open! Import

(** The background subagents of one agent. The [subagent] tool starts them
    (at depth 0) and returns at once; they outlive the turn that started them,
    and each finished job's report is delivered to the agent exactly once:
    either through [take_undelivered] (as a message) or by [wait]/[cancel_and_wait]
    (as a tool result). *)

(** The first line of a task, at most 60 characters. *)
val short_task : string -> string

module Job : sig
  type t

  val id : t -> string
  val task : t -> string
  val result : t -> Tool_result.t option
  val delivered : t -> bool

  (** [[subagent <id> finished] <task>] (or [failed]) followed by the result
      text. *)
  val report : t -> string
end

(** What clients are told about jobs that are running or not yet delivered. *)
module Summary : sig
  type t =
    { id : string
    ; task : string
    ; running : bool
    }
  [@@deriving sexp_of]
end

module Wait_result : sig
  type t =
    { finished : Job.t list
    ; running : Job.t list (** still running when the wait ended *)
    ; timed_out : bool
    }
end

type t

(** Ids are [a<first_id>], [a<first_id + 1>], ... *)
val create : env:Env.t -> sw:Switch.t -> first_id:int -> unit -> t

(** [emit] receives the jobs' events (the agent broadcasts them and rolls up
    their usage); [on_change] is called whenever a job starts, finishes or is
    delivered, after the job's state is updated. *)
val connect
  :  t
  -> emit:(Agent_event.t -> unit)
  -> on_change:(unit -> unit)
  -> unit

(** Starts [run] in a fiber of its own and returns the job id. [run] gets the
    id, the job's own cancellation token and the event sink; its events also
    update the job's last activity. *)
val spawn
  :  t
  -> task:string
  -> run:
       (id:string
        -> cancel:Cancellation.t
        -> emit:(Agent_event.t -> unit)
        -> Tool_result.t)
  -> string

val has_running : t -> bool
val summaries : t -> Summary.t list

(** Finished jobs not delivered yet, oldest first; marks them delivered. *)
val take_undelivered : t -> Job.t list

val has_undelivered : t -> bool

(** One user message carrying the reports of [jobs]. *)
val delivery_message : Job.t list -> Message.t

(** Blocks until the jobs [ids] (default: every running or undelivered job)
    have all finished ([all]) or at least one has, until [timeout] seconds or
    until [cancel]. Finished jobs among them are marked delivered. *)
val wait
  :  t
  -> ids:string list option
  -> all:bool
  -> timeout:float option
  -> cancel:Cancellation.t
  -> Wait_result.t Or_error.t

(** Requests cancellation; the job still finishes (with a [[cancelled]]
    result) and is delivered as usual. *)
val cancel : t -> string -> unit Or_error.t

(** Cancels [id], waits for it to finish and marks it delivered. *)
val cancel_and_wait : t -> string -> cancel:Cancellation.t -> Job.t Or_error.t

(** Cancels every running job. With [discard], every job is marked delivered
    so nothing reaches the agent. *)
val cancel_all : ?discard:bool -> t -> unit

(** Blocks until no job is running. *)
val wait_all : t -> unit

(** A table of every job: id, state, elapsed seconds, task, last activity. *)
val status_text : t -> string
