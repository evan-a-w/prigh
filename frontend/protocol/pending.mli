open! Core

(** [get_pending]: what a session is waiting on, for a frontend that
    (re)connects or switches to it: queued texts, tool confirmations and
    [ask_user] questions. *)

module Confirm : sig
  type t =
    { call_id : string
    ; agent_id : string option (** a subagent's call *)
    ; name : string
    ; summary : string
    ; rules : string list (** what "don't ask again" allows *)
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

module Ask : sig
  type t =
    { call_id : string
    ; questions : Question.t list
    ; deadline : Time_ns.Alternate_sexp.t option
      (** when the agent stops waiting and carries on by itself *)
    }
  [@@deriving sexp_of, equal]

  val of_json : Json.t -> t Or_error.t
end

type t =
  { steer_texts : string list
  ; follow_up_texts : string list
  ; agents : Agent_queue.t list (** the subagents' queues, those not empty *)
  ; confirms : Confirm.t list
  ; questions : Ask.t list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
