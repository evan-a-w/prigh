open! Core
open! Import

(** One subagent's transcript and status, plus any subagents it started. *)

module Status : sig
  type t =
    | Running
    | Done of
        { turns : int
        ; cost_usd : float
        }
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; call_id : string
  ; task : string
  ; name : string option
  ; model : string
  ; host : string option
    (** the tool host it works on (its name), when not the session's
        active one *)
  ; engine : string option [@sexp.option]
    (** the engine/adapter key it runs on, when not the session's own *)
  ; status : Status.t
  ; turns : int
  ; transcript : Transcript.t
  ; children : t list
  }
[@@deriving sexp_of]

val create
  :  ?name:string
  -> ?host:string
  -> ?engine:string
  -> call_id:string
  -> agent_id:string
  -> task:string
  -> model:string
  -> unit
  -> t

(** Its [name], else its task on one line. *)
val title : t -> string

val find : t list -> string -> t option
val replace : t list -> string -> (t -> t) -> t list

val start
  :  ?name:string
  -> ?host:string
  -> ?engine:string
  -> t list
  -> call_id:string
  -> agent_id:string
  -> task:string
  -> model:string
  -> t list

val status_of_result
  :  turns:int
  -> cost_usd:float
  -> P.Event.Subagent_result.t
  -> Status.t

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
