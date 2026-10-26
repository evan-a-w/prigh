open! Core
open! Import

(** The background subagents' queues ([agent_queues] events, [get_pending]'s
    [agents]): what the user sent them that they have not taken yet. *)

type t = P.Agent_queue.t list [@@deriving sexp_of]

(** One agent's queue as the main agent's is shown: steers, then
    follow-ups. *)
val messages : t -> agent_id:string -> Queued_message.t list

(** Shown at once when sent; the backend's next [agent_queues] replaces it. *)
val add : t -> agent_id:string -> Queued_message.Kind.t -> string -> t

(** Takes the latest [text] of that kind out (a send that failed). *)
val remove : t -> agent_id:string -> Queued_message.Kind.t -> string -> t
