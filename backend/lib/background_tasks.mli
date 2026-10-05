open! Core
open! Import

(** The background work of one agent: subagents (the [subagent] tool at depth
    0) and shell jobs ([bash] with [background]). They outlive the turn that
    started them, and each finished task's report is delivered to the agent
    exactly once: either through [take_undelivered] (as a message) or by
    [wait]/[cancel_and_wait] (as a tool result). *)

(** The first line of a task or command, at most 60 characters. *)
val short_task : string -> string

module Kind : sig
  type t =
    | Subagent (** ids [a<n>] *)
    | Job (** ids [j<n>] *)
  [@@deriving sexp_of, equal]
end

module Outcome : sig
  type t =
    { status : string (** e.g. [finished], [failed], [exited 0], [killed] *)
    ; body : string
    ; is_error : bool
    }
  [@@deriving sexp_of]
end

module Task : sig
  type t

  val id : t -> string
  val kind : t -> Kind.t

  (** The subagent's task or the job's command. *)
  val label : t -> string

  val outcome : t -> Outcome.t option
  val running : t -> bool
  val delivered : t -> bool
  val started_at : t -> float
  val finished_at : t -> float option

  (** What [run] wrote to its [on_output]. *)
  val output : t -> Output_tail.t

  (** [[<kind> <id> <status>] <label>] followed by the body. *)
  val report : t -> string
end

(** What clients are told about tasks that are running or not yet delivered. *)
module Summary : sig
  type t =
    { id : string
    ; label : string
    ; running : bool
    ; status : string option (** the outcome's, once finished *)
    }
  [@@deriving sexp_of]
end

module Wait_result : sig
  type t =
    { finished : Task.t list
    ; running : Task.t list (** still running when the wait ended *)
    ; timed_out : bool
    }
end

type t

val create
  :  env:Env.t
  -> sw:Switch.t
  -> first_subagent_id:int
  -> first_job_id:int
  -> unit
  -> t

(** [emit] receives the tasks' events (the agent broadcasts them and rolls up
    their usage); [on_change] is called whenever a task starts, finishes or is
    delivered, after its state is updated. *)
val connect
  :  t
  -> emit:(Agent_event.t -> unit)
  -> on_change:(unit -> unit)
  -> unit

(** Starts [run] in a fiber of its own and returns the task id. [run] gets the
    id, the task's own cancellation token, the event sink (whose events also
    update a subagent's last activity) and the output sink. *)
val spawn
  :  t
  -> kind:Kind.t
  -> label:string
  -> run:
       (id:string
        -> cancel:Cancellation.t
        -> emit:(Agent_event.t -> unit)
        -> on_output:(string -> unit)
        -> Outcome.t)
  -> string

(** Every task of [kind], oldest first. *)
val tasks : t -> kind:Kind.t -> Task.t list

val find : t -> kind:Kind.t -> string -> Task.t Or_error.t

(** Of [kind], or of any kind. *)
val has_running : ?kind:Kind.t -> t -> bool

val summaries : t -> kind:Kind.t -> Summary.t list

(** Finished tasks of any kind not delivered yet, oldest first; marks them
    delivered. *)
val take_undelivered : t -> Task.t list

val has_undelivered : t -> bool

(** One user message carrying the reports of [tasks]. *)
val delivery_message : Task.t list -> Message.t

(** Blocks until the tasks [ids] (default: every running or undelivered one of
    [kind]) have all finished ([all]) or at least one has, until [timeout]
    seconds or until [cancel]. Finished tasks among them are marked
    delivered. *)
val wait
  :  t
  -> kind:Kind.t
  -> ids:string list option
  -> all:bool
  -> timeout:float option
  -> cancel:Cancellation.t
  -> Wait_result.t Or_error.t

(** Requests cancellation; the task still finishes and is delivered as
    usual. *)
val cancel : t -> kind:Kind.t -> string -> unit Or_error.t

(** Cancels [id], waits for it to finish and marks it delivered. *)
val cancel_and_wait
  :  t
  -> kind:Kind.t
  -> string
  -> cancel:Cancellation.t
  -> Task.t Or_error.t

(** Cancels every running task. With [discard], every task is marked
    delivered so nothing reaches the agent. *)
val cancel_all : ?discard:bool -> t -> unit

(** Blocks until no task is running. *)
val wait_all : t -> unit

(** A table of every task of [kind]: id, state, elapsed seconds, label, and
    the last activity (subagents) or output size and last line (jobs). *)
val status_text : t -> kind:Kind.t -> string
