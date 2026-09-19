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

(** Returns the messages added to the context (prompts, assistant messages
    and tool results). [steer] is polled after each turn's tool results; any
    messages it returns are appended before the next provider request. *)
val run
  :  env:Env.t
  -> provider:Provider.t
  -> config:Config.t
  -> cwd:string
  -> ?cancel:Cancellation.t
  -> ?steer:(unit -> Message.t list)
  -> ?emit:(Agent_event.t -> unit)
  -> ?retry_delay:(attempt:int -> unit)
  -> context:Message.t list
  -> prompts:Message.t list
  -> unit
  -> Message.t list
