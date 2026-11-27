open! Core
open! Import

(** The composer engine's runner: a session's turns go to an external
    harness ({!Adapter.S}: Codex, Claude Code) whose process runs on the
    session's tool host, and what it does is recorded in the session like
    a harness run ({!Composer_recorder}). Its approval requests go through
    the session's approvals (mode, rules, smart mode, the user), its
    questions to the user, and prigh's own tools reach it over MCP
    ({!Injected_tools}).

    The process starts with the first turn (or before, {!Runner.prewarm})
    and lives on between turns;
    when it exits (or ignores an interrupt) the next turn starts another,
    continuing the harness's thread. *)

type t

(** [spawn launch] starts the harness's process on the session's tool
    host; it fails saying what to do when the harness is not installed
    there. [start]: the process's settings besides the turn's
    ([developer_instructions], [mcp_servers], [resume] to continue a thread
    the session had). Asked before each turn: when its [login] changed, the
    process stops and the next one continues the thread on the new login. [on_thread]: the harness started or moved to a thread
    (for the session to record). [on_start]: whether the harness started (its
    [start]), each time a process does. [host]: the session's tool host now (default:
    always ["local"]): when it changes, the process stops and the next turn
    starts one there, on that host's thread. [handoff_from]: the engine the
    session ran on before this runner, named in the first handoff.
    [request_timeout]: how long the harness may take to answer each request
    of its start (60s). [sw] bounds the processes' fibers. [checkpoint] runs
    {!Checkpoint.op} on the session's tool host: with it, the files are
    checkpointed around each run (in a git repository), which lets the
    session rewind them with the harness's conversation; without it the
    session cannot rewind. [catalog]: what the harness said of its login
    without a thread ({!Adapter_catalog}), for its models and limits before
    its process runs. [on_change]: the harness's models or limits changed.
    The phases of a run's start ({!Turn_timings}) go in its trace. {!Runner.prewarm} starts the process before the
    first turn.

    A turn ends when the user aborts it (Esc), also while the process
    starts, and when the process exits or stops; then whatever it was
    asking (approvals, questions, prigh's tools over MCP) is withdrawn. A
    turn that fails with an exception stops the process, so the next turn
    starts clean. The harness's events reach a turn only once it started
    it ({!Adapter.Driver}): one the harness runs of its own cannot end the
    next one early.

    Before each turn the harness is handed what it has not seen of the
    session ({!Handoff}), recorded as a [Handoff] entry; after it, the turn
    is recorded ([External_turn]) so that the next handoff starts there. *)
val create
  :  sw:Switch.t
  -> adapter:Adapter.t
  -> spawn:(Adapter.Launch.t -> Line_transport.t Or_error.t)
  -> ?spawn_start:
       (Adapter.Start.t -> Adapter.Launch.t -> Line_transport.t Or_error.t)
       (** When provided, called instead of [spawn] with both [Start.t] and
           [Launch.t] (e.g. to acquire a pooled server transport keyed by
           login/sandbox metadata). *)
  -> ?start:(cwd:string -> Adapter.Start.t)
  -> ?interrupt_grace:Time_float.Span.t
  -> ?request_timeout:Time_float.Span.t
  -> ?steer_poll:Time_float.Span.t
  -> ?on_thread:(string -> unit)
  -> ?on_start:(Adapter.Start.t -> unit Or_error.t -> unit)
  -> ?host:(unit -> string)
  -> ?host_id:string
       (** [Tool.Context.host] for approvals and injected MCP tools, when this
           runner works on another host than the session's active one (a
           subagent) *)
  -> ?home:(unit -> string option)
       (** [Tool.Context.home] for approvals and injected MCP tools *)
  -> ?depth:int (** nesting depth (default [0]; [>= 1] for a subagent) *)
  -> ?agent_id:string
       (** owning subagent's id, prefixed onto approvals and injected calls *)
  -> ?wake:(unit -> Message.t list)
       (** called when a turn finishes normally with no pending steers: any
           messages returned run as the next turn on the same live process *)
  -> ?handoff_from:Engine_choice.t
  -> ?checkpoint:(Json.t -> Tool.Result.t)
  -> ?catalog:(unit -> Adapter.Catalog.t option)
  -> ?on_change:(unit -> unit)
  -> ?on_autonomous:(unit -> unit)
       (** called when the live external harness begins a turn of its own
           between prompts (e.g. a background task woke it), so the session
           can drive and record it as an [Autonomous] run *)
  -> unit
  -> t

val runner : t -> Runner.t

(** The harness's thread, once it has one: what a later process resumes. *)
val thread_id : t -> string option

(** Stops the harness's process, if it runs; a turn it runs ends at once. *)
val close : t -> unit
