open! Core
open! Import

(** The model's task plan, kept with the [update_plan] tool (Codex's): a
    short list of steps, each pending, in progress or completed, and an
    optional explanation. It is session state: the latest successful
    [update_plan] call on the session's active path, so it is persisted,
    and rewinding or forking takes it along. *)

module Status : sig
  type t =
    | Pending
    | In_progress
    | Completed
  [@@deriving sexp_of, equal, enumerate]

  (** [pending], [in_progress], [completed]. *)
  val to_string : t -> string
end

module Step : sig
  type t =
    { step : string
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

type t =
  { explanation : string option
  ; steps : Step.t list (** never empty *)
  }
[@@deriving sexp_of, equal]

(** [update_plan]'s arguments. An empty [plan] clears it ([None]); more than
    one step in progress, unknown statuses and empty steps are errors that
    say what to fix. *)
val of_arguments : Json.t -> t option Or_error.t

(** [{explanation, steps: [{step, status}]}] *)
val to_json : t -> Json.t

(** The plan as of the last successful [update_plan] call in [messages]. *)
val latest : Message.t list -> t option

(** [update_plan]. Only the main agent has it ({!Tools.for_context} drops it
    for subagents). *)
val tool : Tool.t

val tool_name : string
