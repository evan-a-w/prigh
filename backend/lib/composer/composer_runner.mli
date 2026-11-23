open! Core
open! Import

(** The composer engine's runner: a session's turns go to an external
    harness ({!Adapter.S}: Codex, Claude Code) whose process runs on the
    session's tool host, and what it does is recorded in the session like
    a harness run ({!Composer_recorder}). Its approval requests go through
    the session's approvals (mode, rules, smart mode, the user), its
    questions to the user, and prigh's own tools reach it over MCP
    ({!Injected_tools}).

    The process starts with the first turn and lives on between turns;
    when it exits (or ignores an interrupt) the next turn starts another,
    continuing the harness's thread. *)

type t

(** [spawn launch] starts the harness's process on the session's tool
    host; it fails saying what to do when the harness is not installed
    there. [start]: the process's settings besides the turn's
    ([developer_instructions], [mcp_servers], [resume] to continue a thread
    the session had). [on_thread]: the harness started or moved to a thread
    (for the session to record). [host]: the session's tool host now (default:
    always ["local"]): when it changes, the process stops and the next turn
    starts one there, on that host's thread. [handoff_from]: the engine the
    session ran on before this runner, named in the first handoff. [sw]
    bounds the processes' fibers. [checkpoint] runs {!Checkpoint.op} on
    the session's tool host: with it, the files are checkpointed around
    each run (in a git repository), which lets the session rewind them with
    the harness's conversation; without it the session cannot rewind.

    Before each turn the harness is handed what it has not seen of the
    session ({!Handoff}), recorded as a [Handoff] entry; after it, the turn
    is recorded ([External_turn]) so that the next handoff starts there. *)
val create
  :  sw:Switch.t
  -> adapter:Adapter.t
  -> spawn:(Adapter.Launch.t -> Line_transport.t Or_error.t)
  -> ?start:(cwd:string -> Adapter.Start.t)
  -> ?interrupt_grace:Time_float.Span.t
  -> ?steer_poll:Time_float.Span.t
  -> ?on_thread:(string -> unit)
  -> ?host:(unit -> string)
  -> ?handoff_from:Engine_choice.t
  -> ?checkpoint:(Json.t -> Tool.Result.t)
  -> unit
  -> t

val runner : t -> Runner.t

(** The harness's thread, once it has one: what a later process resumes. *)
val thread_id : t -> string option

(** Stops the harness's process, if it runs. *)
val close : t -> unit
