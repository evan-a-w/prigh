open! Core
open! Import

(** The agents panel's data: the session's subagents ([list_subagents], kept
    live by their events; nested ones, [<parent>/<call>], under their parent)
    and background jobs ([list_jobs]), and what the panel shows. Pure; [App]
    keeps one per session. *)

module Status : sig
  type t =
    | Running
    | Complete
    | Failed
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

module Agent : sig
  type t =
    { id : string
    ; call_id : string (** the [subagent] call that started it *)
    ; parent : string option
    ; task : string
    ; name : string option (** a few words saying what it does *)
    ; model : string
    ; status : Status.t
    ; started_at : Time_ns.t
    ; ended_at : Time_ns.t option
    ; turns : int
    ; tool_calls : int
    ; current_tool : (string * Time_ns.t) option (** name, started at *)
    ; result : Event.Subagent_result.t option
    }
  [@@deriving sexp_of]

  (** A [list_subagents] item and whether it is [stale] (finished before the
      latest run). *)
  val of_json : Json.t -> (t * bool) Or_error.t

  (** Its [name], else its task's first line. *)
  val title : t -> string

  (** Its task says no more than its {!title}: one line, the same words
      (ignoring case and trailing punctuation). *)
  val task_in_title : t -> bool
end

module Job : sig
  type t =
    { info : Job_info.t
    ; seen_at : Time_ns.t (** when [list_jobs] answered: [info.elapsed] then *)
    }
  [@@deriving sexp_of]
end

module Item : sig
  type t =
    | Agent of string
    | Job of string
  [@@deriving sexp_of, compare, equal]

  include Comparable.S_plain with type t := t
end

(** A subagent's transcript asked of the backend ([get_subagent]). *)
module Fetch : sig
  type t =
    | Asked of Time_ns.t option (** when, once the clock has said *)
    | Failed of string (** what to say *)
  [@@deriving sexp_of]
end

type t =
  { open_ : bool
  ; selected : Item.t option (** shown in full; the list otherwise *)
  ; agents : Agent.t list (** in start order *)
  ; jobs : Job.t list
  ; earlier : Item.Set.t (** finished before the latest prompt: folded away *)
  ; stopping : Item.Set.t (** asked to cancel or kill, still running *)
  ; output : (string * string) option (** a job's id and output *)
  ; polled_at : Time_ns.t option
  ; fetches : Fetch.t String.Map.t
    (** by agent id: the transcripts the panel has asked for and not got *)
  }
[@@deriving sexp_of]

val empty : t

(** Applies a top-level event: subagents starting and ending at any depth,
    their turns and tools. *)
val record : t -> now:Time_ns.t -> Event.t -> t

(** A subagent started or ended somewhere: [list_subagents] has its times. *)
val starts_or_ends : Event.t -> bool

(** A [list_subagents] reply. Stale agents seen for the first time go under
    "earlier". *)
val set_agents : t -> (Agent.t * bool) list -> t

(** A [list_jobs] reply; delivered jobs seen for the first time go under
    "earlier". *)
val set_jobs : t -> now:Time_ns.t -> Job_info.t list -> t

(** The user prompted: what has finished so far goes under "earlier". *)
val prompt_sent : t -> t

val find_agent : t -> string -> Agent.t option
val find_job : t -> string -> Job.t option
val children : t -> string option -> Agent.t list

(** [id]'s ancestors, the top-level one first, then [id] itself. *)
val lineage : t -> string -> Agent.t list

val running : t -> Item.t -> bool
val running_agents : t -> int
val running_jobs : t -> int

(** The listed (not earlier) agents as a tree, running ones first, with
    their depth; then the listed jobs. Numbered from 1 in this order for
    Alt+1..9 and [/agents N]. *)
val listed : t -> (Item.t * int) list

(** Earlier agents (as a tree) and jobs. *)
val earlier : t -> (Item.t * int) list

(** A number from [listed], an agent's or job's id, or a call id. *)
val resolve : t -> string -> Item.t option

(** The next ([delta] = 1) or previous listed item from the selected one. *)
val cycle : t -> int -> Item.t option

val agent_elapsed : Agent.t -> now:Time_ns.t -> Time_ns.Span.t
val job_elapsed : Job.t -> now:Time_ns.t -> Time_ns.Span.t

(** ["9s"], ["3m 05s"], ["1h 02m"]. *)
val format_span : Time_ns.Span.t -> string

(** Some transcript is [Asked] for. *)
val fetching : t -> bool

(** Those [Asked] for [after] or longer have [Failed]. *)
val time_out_fetches : t -> now:Time_ns.t -> after:Time_ns.Span.t -> t
