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
    ; forked : bool (** it started from its caller's conversation *)
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
    ; output : string
      (** streamed so far: the last [live_limit] bytes or so, from a line
              start *)
    ; lines : int (** the newlines in all the output streamed *)
    ; missed : bool
      (** some of its output streamed while the page did not follow (another
          session on show, the connection lost, opened as it ran) *)
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
    | Shell of Tool_call.t (** a [!command] the user ran *)
    | Compaction of string (** the summary that replaced older messages *)
  [@@deriving sexp_of]
end

(** How much of a running call's output is kept, in bytes (at most twice
    this). *)
val live_limit : int

(** How many ['\n'] [s] has. *)
val newlines : string -> int

val empty : t

(** [running]: the agent is still working, so tool calls without a result
    are still to get one. *)
val of_messages : ?running:bool -> Message.t list -> t

(** The calls still running stream output that this page does not see
    (until it follows the session again): [Tool.missed]. *)
val missed_output : t -> t

(** Oldest first. *)
val entries : t -> Entry.t list

(** The number of the oldest of [entries]: they are numbered in order, and
    keep their numbers as messages are added after them or [prepend]ed
    before them. *)
val first : t -> int

(** Adds older messages (a page before those loaded) before the entries. *)
val prepend : t -> Message.t list -> t

(** [fresh] (the messages from the first one [old] has to the newest, fetched
    again after reconnecting) with [old]'s entries and tool calls wherever
    they are the same, so that their views are kept (and [old] itself when
    nothing changed); numbered as [old]. A message still streaming in [old]
    stays while [fresh] is running, and an output [old] has whole stays whole
    where [fresh] has it cut. *)
val reconcile : old:t -> t -> t

val tool : t -> string -> Tool.t option

(** Whether the agent is working: between its start and end events. A tool
    call without a result while it is not will never get one. *)
val running : t -> bool

(** What the agent is doing, from its events ([Idle] unless [running]). *)
val activity : t -> Prigh_ui.Activity.t

(** As the backend says (its state's [running]): a transcript fetched or
    cached while the agent was not known to be working. [t] itself when that
    is what it says. *)
val set_running : t -> bool -> t

val apply : t -> Event.t -> t

(** Gives the messages of [message_start] and [message_end] events (also a
    subagent's) without a time, from a backend that does not send them, the
    time the page received them. *)
val received : Event.t -> at:Time_ns.t -> Event.t

(** The ids of [subagent] calls without a transcript: [get_messages] only
    has their reports, the backend keeps the rest ([get_subagent]). *)
val subagents_to_load : t -> string list

(** Fills in a [subagent] call's transcript, unless events already did. *)
val set_subagent : t -> call_id:string -> Subagent.t -> t

(** A subagent by its agent id, at any depth, also one kept detached. *)
val find_subagent : t -> string -> Subagent.t option

(** A [get_subagent] reply: [set_subagent] in the transcript of the subagent
    [parent] (at any depth), or in [t] itself. While that call is not loaded
    (in an older page, or its parent not fetched yet) the subagent is kept
    detached: found, and kept up to date by its events, and put on its call
    once the call is loaded; or, unless [detach], dropped. *)
val set_nested_subagent
  :  t
  -> parent:string option
  -> call_id:string
  -> detach:bool
  -> Subagent.t
  -> t

(** A tool call of the transcript, or of the transcript of the subagent
    [agent_id] (at any depth). *)
val find_tool : t -> agent_id:string option -> call_id:string -> Tool.t option

(** Gives a call's result its whole [text], once fetched in place of what was
    [elided]. *)
val set_output : t -> agent_id:string option -> call_id:string -> string -> t
