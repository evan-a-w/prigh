open! Core
open! Import

(** The session's state as a subagent's chat shows it: the subagent's model,
    reasoning, tool host, folder, branch, activity, context and cost in the
    session's place, so that the chrome around its chat (the composer's
    chips, the line under it, the context ring) is the subagent's. The
    session's modes, plan, goal, budget, reminders and its own subagents and
    jobs belong to the main agent: none of them here. *)

val of_agent
  :  State.t
  -> models:Llm.t list
  -> Agents.Agent.t
  -> transcript:Chat.t option
  -> cost_usd:float option
  -> State.t

(** The model a subagent's id names, preferring [current]'s provider; a
    stand-in named after the id when no listed model has it. *)
val model : models:Llm.t list -> current:Llm.t -> string -> Llm.t

(** What a subagent keeps from its start: the backend changes none of them
    for a running subagent. *)
module Fixed : sig
  type t =
    | Model
    | Host
    | Folder
    | Branch
  [@@deriving sexp_of, equal]

  (** Why it cannot change, and what to do instead: [title] is the
      subagent's, [parent] what Esc goes back to. *)
  val explain : t -> title:string -> parent:string -> string
end
