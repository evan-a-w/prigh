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
    }

  val default_retries : int
end

val is_retryable_error : string -> bool

(** What a call does, for confirmations: [bash]'s command, a file tool's
    path, the files an [apply_patch] touches. *)
val tool_summary : name:string -> Json.t -> string

(** Returns the messages added to the context (prompts, assistant messages
    and tool results). [steer] is polled after each turn's tool results; any
    messages it returns are appended before the next provider request.
    [approve] checks every tool call before it runs (default: all run);
    [ask_user] is how the [ask_user] tool reaches the user. Both are passed
    on to the tools' contexts, so [subagent] can hand [approve] down. *)
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
  -> ?ask_user:(call_id:string -> User_question.t list -> User_question.Reply.t)
  -> ?execute:Tool.executor
  -> ?background:Background_tasks.t
  -> ?retry_delay:(attempt:int -> unit)
  -> context:Message.t list
  -> prompts:Message.t list
  -> unit
  -> Message.t list
