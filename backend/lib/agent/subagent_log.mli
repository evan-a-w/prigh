open! Core
open! Import

(** Every subagent an agent ran since it was loaded (background jobs and the
    synchronous subagents nested in them), built from the agent's events:
    status, activity and transcript, so a frontend that joins late (or comes
    back to the session) can show them. Nested subagents' ids are
    [<parent id>/<call id>].

    Once {!load}ed for a session, it is also appended to that session's
    [subagents.jsonl] (one JSON line per subagent start, message and end,
    and per run once the file exists; nothing per streamed delta), so that
    a later {!load} (the session coming back after eviction or a restart)
    restores it. Reads always come from memory. *)

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
    ; name : string option
      (** {!Subagent_name}; [None] in logs written before names *)
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
    transcript (each message at the time its [Message_end] was recorded). *)
val find : t -> string -> (Summary.t * Timed_message.t list) option

(** A synthetic [Subagent_end] (error result [[interrupted] <why root>], zero
    usage) for every subagent still running whose top-level ancestor [root]
    is not [alive], wrapped in its ancestors' [Subagent] envelopes like a
    real one, so that every reducer of the agent's events (this log
    included, once they are [record]ed) sees it end. Newest first, so nested
    ones end before their parents. *)
val orphan_ends
  :  t
  -> alive:(string -> bool)
  -> why:(string -> string)
  -> Agent_event.t list

(** Replaces the contents with the log saved in [dir ()] (none if [None] or
    if it has no log; lines that do not parse are skipped) and from now on
    appends to it, creating the directory on the first subagent; [dir] is
    asked on every write, [None] meaning not to write. Subagents saved as
    running are not running any more (whatever ran them is gone): they load
    as failed with an [[interrupted]] result, ended at their last recorded
    activity. Their ends are only written once {!take_interrupted} has
    them reported. *)
val load : t -> dir:(unit -> string option) -> unit

(** Whether {!load} ended background subagents of the main agent (not
    nested ones, nor synchronous ones, whose caller's call is interrupted
    too) that {!take_interrupted} has not taken yet. *)
val has_interrupted : t -> bool

(** Those background subagents, with their transcripts, oldest first; the
    ends of everything {!load} ended are written, so that later loads do not
    report them again. *)
val take_interrupted : t -> (Summary.t * Timed_message.t list) list

(** Stops writing (the backend is stopping): the subagents still running
    load as interrupted next time, rather than as cancelled by the stop. *)
val detach : t -> unit
