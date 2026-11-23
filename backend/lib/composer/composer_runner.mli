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
    the session had). [sw] bounds the processes' fibers. *)
val create
  :  sw:Switch.t
  -> adapter:Adapter.t
  -> spawn:(Adapter.Launch.t -> Line_transport.t Or_error.t)
  -> ?start:(cwd:string -> Adapter.Start.t)
  -> ?interrupt_grace:Time_float.Span.t
  -> ?steer_poll:Time_float.Span.t
  -> unit
  -> t

val runner : t -> Runner.t

(** The harness's thread, once it has one: what a later process resumes. *)
val thread_id : t -> string option

(** Stops the harness's process, if it runs. *)
val close : t -> unit
