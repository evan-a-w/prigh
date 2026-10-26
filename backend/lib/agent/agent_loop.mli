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
    ; compaction : Compaction_settings.t (** see {!run} *)
    }

  val default_retries : int

  (** What the summarising request shares with the loop's requests. *)
  val compaction_prompt : t -> Compaction.Prompt.t
end

val is_retryable_error : string -> bool

(** What a call does, for confirmations: [bash]'s command, a file tool's
    path, the files an [apply_patch] touches, what [job_input] writes,
    [worktree]'s action and its branch or path. *)
val tool_summary : name:string -> Json.t -> string

(** Returns the messages added to the context (prompts, assistant messages
    and tool results). [steer] is polled after each turn's tool results; any
    messages it returns are appended before the next provider request.
    [wake] is called when the model ends its turn (no tool calls, within the
    turn limit, not cancelled): it may block, and any messages it
    returns are appended and the run goes on; [[]] ends the run.

    Between turns, with [config.compaction.mask_observations], old tool
    outputs are masked when {!Compaction_settings.worth_masking} says so for
    the latest response's tokens (plus what was added since): [mask]
    (default: {!Observation_mask.apply}, in memory) returns the masked
    context, and [Masked] says what it saved. When the tokens left still
    cross [config.compaction]'s threshold ({!Compaction_settings.threshold}),
    the context is compacted with [compact] (default:
    {!Compaction.compact_messages}, in memory), summarised by
    {!Compaction_settings.summary_model}, and the run continues on the
    result. A request the provider rejects as too long ({!Context_overflow})
    is masked, if there is anything to mask, else compacted ([overflowed]),
    and sent again, compacting at most twice and keeping less the second
    time, before the run fails with what to do. Each compaction emits
    [Compacted] (at the summariser's prices). [halt] is polled before each
    turn (also passed to tools, for subagents): [Some _] ends the run there.
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
  -> ?wake:(unit -> Message.t list)
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
       (model:Model.t
        -> keep_recent_tokens:int
        -> overflowed:bool
        -> cancel:Cancellation.t
        -> Message.t list
        -> (Message.t list * Compaction.Outcome.t) Or_error.t)
  -> ?mask:(through:int -> Message.t list -> Message.t list Or_error.t)
  -> ?halt:(unit -> string option)
  -> context:Message.t list
  -> prompts:Message.t list
  -> unit
  -> Message.t list
