open! Core

(** A background subagent's queue: what the user (or the main agent) sent it
    while it works and it has not taken yet ([agent_queues] events,
    [get_pending]'s [agents]). Sent with [steer_agent {agent_id, text, mode}],
    taken back with [dequeue_agent {agent_id}] or [remove_agent_queued
    {agent_id, queue, index, text?}]. *)

type t =
  { agent_id : string
  ; steer : string list (** oldest first *)
  ; follow_up : string list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** The queue of [agent_id] in a list of them; empty when it has none. *)
val find : t list -> string -> t
