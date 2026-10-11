open! Core
open! Import

(** What both frontends show of the session's budget, goal and scheduled
    resume: status line parts, the answers of [/budget], [/goal] and
    [/resume] without arguments, and [/budget]'s argument. *)

module Level : sig
  type t =
    | Normal
    | Warn (** 80% of the budget spent *)
    | Over (** spent: no turn starts *)
  [@@deriving sexp_of, equal]
end

(** [$0.12/$5.00] when a budget is set. *)
val budget_part : P.State.t -> (string * Level.t) option

(** [goal:3/20] (continuations used, while active), [goal:done] or
    [goal:stopped]. *)
val goal_part : P.Goal.t -> string * Level.t

(** [14:05] in the frontend's time zone. *)
val clock : Time_ns.t -> utc_offset:Time_ns.Span.t -> string

(** [resumes 14:05]. *)
val resume_part : Time_ns.t -> utc_offset:Time_ns.Span.t -> string

val describe_budget : P.State.t -> string
val describe_goal : P.State.t -> string
val describe_resume : P.State.t -> utc_offset:Time_ns.Span.t -> string

(** [/budget]'s argument: dollars ([5], [$2.50]) or [off] ([None]). *)
val parse_budget : string -> (float option, string) Result.t
