open! Core
open! Import

(** The core agentic loop: stream an assistant message, execute any tool
    calls, append the results and repeat until the assistant stops, an error
    occurs, the run is cancelled or [max_turns] is reached. *)

module Config : sig
  type t =
    { model : Model.t
    ; thinking : Thinking.t
    ; system : string option
    ; tools : Tool.t list
    ; max_turns : int option
    ; max_tokens : int option
    ; retries : int (** retries per turn for transient provider errors *)
    ; cache_key : string option (** see [Provider.Request.cache_key] *)
    }

  val default_retries : int
end

val is_retryable_error : string -> bool

(** What a call does, for confirmations: [bash]'s command, a file tool's
    path, the files an [apply_patch] touches, what [job_input] writes. *)
val tool_summary : name:string -> Json.t -> string

(** Returns the messages added to the context (prompts, assistant messages
    and tool results). [steer] is polled after each turn's tool results; any
    messages it returns are appended before the next provider request.

    Between turns, when the latest response's tokens (plus what was added
    since) cross {!Compaction.should_compact}'s threshold, the context is
    compacted with [compact] (default: {!Compaction.compact_messages}, in
    memory) and the run continues on the result; a request the provider
    rejects as too long ({!Context_overflow}) is compacted and sent again,
    keeping less the second time, before the run fails with what to do.
    Each compaction emits [Compacted]. [halt] is polled before each turn
    (also passed to tools, for subagents): [Some _] ends the run there.
    [approve] checks every tool call before it runs (default: all run);
    [ask_user] is how the [ask_user] tool reaches the user. Both are passed
    on to the tools' contexts, so [subagent] can hand [approve] down.

    A transient provider error ({!is_retryable_error}) is retried up to
    [config.retries] times, emitting [Retry] and then waiting with [sleep]
    (default: the clock; cancellation ends the wait). *)
val run
  :  env:Env.t
  -> provider:Provider.t
  -> config:Config.t
  -> cwd:string
  -> ?cancel:Cancellation.t
  -> ?depth:int
  -> ?agent_id:string
  -> ?steer:(unit -> Message.t list)
  -> ?emit:(Agent_event.t -> unit)
  -> ?approve:Tool.approver
  -> ?ask_user:
       (call_id:string
        -> timeout_seconds:int option
        -> User_question.t list
        -> User_question.Reply.t)
  -> ?execute:Tool.executor
  -> ?background:Background_tasks.t
  -> ?sleep:(float -> unit)
  -> ?compact:
       (keep_recent_tokens:int
        -> cancel:Cancellation.t
        -> Message.t list
        -> (Message.t list * Compaction.Outcome.t) Or_error.t)
  -> ?halt:(unit -> string option)
  -> context:Message.t list
  -> prompts:Message.t list
  -> unit
  -> Message.t list
