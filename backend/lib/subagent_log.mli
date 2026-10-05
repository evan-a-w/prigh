open! Core
open! Import

(** Every subagent an agent ran since it was loaded (background jobs and the
    synchronous subagents nested in them), built from the agent's events:
    status, activity and transcript, so a frontend that joins late (or comes
    back to the session) can show them. Nested subagents' ids are
    [<parent id>/<call id>]. *)

module State : sig
  type t =
    | Running
    | Complete
    | Failed
  [@@deriving sexp_of]

  val to_string : t -> string
end

module Summary : sig
  type t =
    { id : string
    ; call_id : string (** the [subagent] tool call that started it *)
    ; parent : string option
    ; task : string
    ; model : string
    ; state : State.t
    ; started_at : float
    ; updated_at : float (** last event *)
    ; ended_at : float option
    ; turns : int
    ; tool_calls : int
    ; current_tool : (string * float) option (** name, started at *)
    ; message_count : int
    ; stale : bool (** finished before the parent agent's latest run started *)
    ; result : Tool_result.t option
    }
  [@@deriving sexp_of]
end

type t

val create : unit -> t

(** Feeds one of the parent agent's own events ([Subagent_start],
    [Subagent], [Subagent_end]; a top-level [Agent_start] begins a run),
    received at [now] (seconds). *)
val record : t -> now:float -> Agent_event.t -> unit

(** Oldest first. *)
val summaries : t -> Summary.t list

(** By agent id or by the id of the tool call that started it, with its
    transcript. *)
val find : t -> string -> (Summary.t * Message.t list) option

val clear : t -> unit
