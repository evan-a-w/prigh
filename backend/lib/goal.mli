open! Core
open! Import

(** A session's goal ([/goal TEXT]): the agent keeps working across runs until
    it calls [goal_complete], continuing automatically after each run that
    ends normally, up to a number of continuations. Recorded in the session
    (see [Session.goal]). *)

module Status : sig
  type t =
    | Active
    | Done of { summary : string } (** the model called [goal_complete] *)
    | Stopped of { reason : string }
    (** by an abort, an error, the budget or the continuation cap; [/goal
        resume] makes it active again *)
  [@@deriving sexp, jsonaf, equal]

  val to_string : t -> string
end

type t =
  { text : string
  ; status : Status.t
  ; continuations : int
    (** automatic continuations since the goal was set or resumed *)
  }
[@@deriving sexp, jsonaf, equal]

val create : string -> t
val is_active : t -> bool

(** The default cap on automatic continuations. *)
val default_max_continuations : int

(** The user message that starts work on a goal just set (or resumed). *)
val start_message : t -> string

(** The user message of an automatic continuation, restating the goal. *)
val continuation_message : t -> max_continuations:int -> string

(** What [/goal] shows. *)
val describe : t -> max_continuations:int -> string

val tool_name : string
