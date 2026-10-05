open! Core
open! Import

(** The transcript of one agent: messages in order, with each tool call's live
    state (streamed output, result, the subagent it started). Built from
    [get_messages] and kept up to date by events. *)

type t [@@deriving sexp_of]

module Subagent : sig
  type chat := t

  type t =
    { agent_id : string
    ; task : string
    ; model : string
    ; chat : chat (** its own transcript, from its events *)
    ; turns : int
    ; cost_usd : float option (** once it has finished *)
    ; result : Event.Subagent_result.t option
    }
  [@@deriving sexp_of]
end

module Tool : sig
  type t =
    { call : Tool_call.t
    ; output : string (** streamed so far *)
    ; result : Message.Tool_result.t option
    ; subagent : Subagent.t option
    }
  [@@deriving sexp_of]
end

module Entry : sig
  type t =
    | User of Message.User.t
    | Assistant of
        { message : Message.Assistant.t
        ; streaming : bool
        }
    | Notice of string
    | Compaction of string (** the summary that replaced older messages *)
  [@@deriving sexp_of]
end

val empty : t
val of_messages : Message.t list -> t

(** Oldest first. *)
val entries : t -> Entry.t list

val tool : t -> string -> Tool.t option

(** Whether the agent is working: between its start and end events. A tool
    call without a result while it is not will never get one. *)
val running : t -> bool

val apply : t -> Event.t -> t

(** The ids of [subagent] calls without a transcript: [get_messages] only
    has their reports, the backend keeps the rest ([get_subagent]). *)
val subagents_to_load : t -> string list

(** Fills in a [subagent] call's transcript, unless events already did. *)
val set_subagent : t -> call_id:string -> Subagent.t -> t

val add_notice : t -> string -> t
