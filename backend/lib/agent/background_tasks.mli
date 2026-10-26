open! Core
open! Import

(** The background work of one agent: subagents (the [subagent] tool at depth
    0) and shell jobs ([bash] with [background]). They outlive the turn that
    started them, and each finished task's report is delivered to the agent
    exactly once: either through [take_undelivered]/[take_next] (as a
    message) or by [cancel_and_wait] (as a tool result). *)

(** The first line of a task or command, at most 60 characters. *)
val short_task : string -> string

(** [30s], [0.5s]. *)
val seconds_text : float -> string

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

  (** A job's stdin. *)
  val input : t -> Stdin_feed.t option

  (** A subagent's messages from the user and the main agent. *)
  val inbox : t -> Subagent_inbox.t option

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

type t

val create
  :  env:Env.t
  -> sw:Switch.t
  -> first_subagent_id:int
  -> first_job_id:int
  -> unit
  -> t

(** New subagents get ids other than [ids] (those of a reloaded
    [Subagent_log]). *)
val avoid_subagent_ids : t -> string list -> unit

(** [emit] receives the tasks' events (the agent broadcasts them and rolls up
    their usage); [on_change] is called whenever a task starts, finishes or is
    delivered, after its state is updated. [auto_background] (default: never)
    is how many seconds a foreground command may run before it is moved to
    the background ({!run_foreground}); it is asked each time, so it follows
    the configuration. [on_queue_change] is called whenever a task's
    {!Subagent_inbox} changes. *)
val connect
  :  ?auto_background:(unit -> float option)
  -> ?on_queue_change:(unit -> unit)
  -> t
  -> emit:(Agent_event.t -> unit)
  -> on_change:(unit -> unit)
  -> unit

val auto_background : t -> float option

(** Starts [run] in a fiber of its own and returns the task id. [run] gets the
    id, the task's own cancellation token, the event sink (whose events also
    update a subagent's last activity) and the output sink. [inbox] is
    closed when [run] returns, before the task counts as finished. *)
val spawn
  :  ?input:Stdin_feed.t
  -> ?inbox:Subagent_inbox.t
  -> t
  -> kind:Kind.t
  -> label:string
  -> run:
       (id:string
        -> cancel:Cancellation.t
        -> emit:(Agent_event.t -> unit)
        -> on_output:(string -> unit)
        -> Outcome.t)
  -> string

module Foreground : sig
  type 'a t =
    | Finished of 'a
    | Moved of Task.t
    (** still running after [after] seconds: it became this task (with the
        next id of its kind), which keeps running and is delivered like a
        spawned one *)
end

(** Runs [run] for the calling fiber, in a fiber of the tasks' switch, and
    waits for it up to [after] seconds. Until then its output goes to
    [on_output] (and is kept), and [cancel] cancels it (the call then waits
    for it to end). If it is still running then, it is moved to the
    background without being restarted: a task whose output so far is
    [run]'s, whose outcome is [outcome] of [run]'s result, and whose
    cancellation (for [cancel]/[job_kill]) is [run]'s. Exceptions [run]
    raises before that are raised here. *)
val run_foreground
  :  ?input:Stdin_feed.t
  -> t
  -> kind:Kind.t
  -> label:string
  -> after:float
  -> cancel:Cancellation.t
  -> on_output:(string -> unit)
  -> run:(cancel:Cancellation.t -> on_output:(string -> unit) -> 'a)
  -> outcome:('a -> output:Output_tail.t -> Outcome.t)
  -> 'a Foreground.t

(** Every task of [kind], oldest first. *)
val tasks : t -> kind:Kind.t -> Task.t list

val find : t -> kind:Kind.t -> string -> Task.t Or_error.t

(** Of [kind], or of any kind. *)
val has_running : ?kind:Kind.t -> t -> bool

(** Running tasks of any kind. *)
val running_count : t -> int

val summaries : t -> kind:Kind.t -> Summary.t list

(** Finished tasks of any kind not delivered yet, oldest first; marks them
    delivered. *)
val take_undelivered : t -> Task.t list

val has_undelivered : t -> bool

(** One user message carrying the reports of [tasks]. *)
val delivery_message : Task.t list -> Message.t

(** [take_undelivered], but when nothing is undelivered and tasks are
    running, it first blocks until one of them finishes (or [cancel], or
    [interrupt] returns). [[]] when nothing is running, on [cancel] or on
    [interrupt]. *)
val take_next
  :  ?interrupt:(unit -> unit)
  -> t
  -> cancel:Cancellation.t
  -> Task.t list

(** A running subagent's inbox, or an error saying what to do instead (it
    finished, it is unknown, it is a subagent's own). *)
val inbox_of : t -> string -> Subagent_inbox.t Or_error.t

(** Queues a message for a running subagent ({!inbox_of}). *)
val send
  :  t
  -> id:string
  -> Steer_queue.Kind.t
  -> from:Subagent_inbox.From.t
  -> string
  -> unit Or_error.t

(** The subagents with queued messages, oldest first. *)
val queues : t -> Subagent_inbox.Listing.t list

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

(** Drops every finished task (for a session replaced in place, so its
    tasks are not listed in the next one). *)
val forget_finished : t -> unit

(** Blocks until no task is running. *)
val wait_all : t -> unit

(** A table of every task of [kind]: id, state, elapsed seconds, label, and
    the last activity (subagents) or output size and last line (jobs). *)
val status_text : t -> kind:Kind.t -> string
