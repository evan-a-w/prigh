open! Core
module P = Prigh_protocol

(** One subagent's transcript and status, plus any subagents it started. *)

type status =
  | Running
  | Done of
      { turns : int
      ; cost_usd : float
      }
  | Failed of string
[@@deriving sexp_of, equal]

type t =
  { id : string
  ; call_id : string
  ; task : string
  ; model : string
  ; status : status
  ; turns : int
  ; transcript : Transcript.t
  ; children : t list
  }
[@@deriving sexp_of]

val create
  :  call_id:string
  -> agent_id:string
  -> task:string
  -> model:string
  -> t

val find : t list -> string -> t option
val replace : t list -> string -> (t -> t) -> t list

val start
  :  t list
  -> call_id:string
  -> agent_id:string
  -> task:string
  -> model:string
  -> t list

val finish
  :  t list
  -> agent_id:string
  -> turns:int
  -> cost_usd:float
  -> result:P.Event.Subagent_result.t
  -> t list

(** Applies an event that belongs to [t] (or, when wrapped, to one of its
    children) to the right transcript. *)
val apply : t -> P.Event.t -> t

(** Applies a top-level event to the list of depth-1 agents. *)
val apply_all : t list -> P.Event.t -> t list
