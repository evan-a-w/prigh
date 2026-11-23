open! Core
open! Import

(** How a composer session starts its external harness's process. *)
module Start = struct
  type t =
    { cwd : string
    ; model : string option (** [None]: the harness's default *)
    ; thinking : Thinking.t
    ; approval : Approval_mode.t
    ; plan : bool
    ; resume : string option (** the harness's thread to continue *)
    ; fork : bool (** continue [resume] in a new thread *)
    ; at : Session.Fork_point.t option
      (** continue [resume] only up to this turn: its fork ([fork]) is
          made there, or (an agent that rolls back by resuming at a
          message: {!Rollback.Resume_at}) it is rolled back to it *)
    ; developer_instructions : string option
      (** appended to the harness's own system prompt *)
    ; mcp_servers : (string * Json.t) list
      (** stdio MCP servers to give it: name and [{command, args, env}] *)
    }
  [@@deriving sexp_of]

  let default ~cwd =
    { cwd
    ; model = None
    ; thinking = Thinking.default
    ; approval = Ask
    ; plan = false
    ; resume = None
    ; fork = false
    ; at = None
    ; developer_instructions = None
    ; mcp_servers = []
    }
  ;;
end

(** The settings a turn runs with: the session's, which may have changed
    since the process started. *)
module Settings = struct
  type t =
    { model : string option
    ; thinking : Thinking.t
    ; approval : Approval_mode.t
    ; plan : bool
    }
  [@@deriving sexp_of, equal]

  let of_start (s : Start.t) =
    { model = s.model
    ; thinking = s.thinking
    ; approval = s.approval
    ; plan = s.plan
    }
  ;;
end

(** The process to run, on the session's tool host. *)
module Launch = struct
  type t =
    { argv : string list
    ; env_set : (string * string) list
    ; env_unset : string list
    ; cwd : string
    ; mcp : bool
      (** with an MCP side channel, through which prigh's tools reach a
          harness that has no MCP of its own in its protocol *)
    }
  [@@deriving sexp_of]
end

module Input = struct
  type t =
    { text : string
    ; images : Image.t list
    }
  [@@deriving sexp_of]
end

(** How a harness takes its conversation back to an earlier turn. *)
module Rollback = struct
  type t =
    | Unsupported
    | Revert (** asked of the running process ({!S.revert}) *)
    | Resume_at
      (** its next process continues the thread at a turn's end
          ([Start.at], with {!S.mark}) *)
  [@@deriving sexp_of, equal]
end

(** An external harness (Codex, Claude Code, ...) driven over its process's
    protocol. After {!S.start}, the harness's events arrive on
    {!S.next_event}, in order; each turn {!S.send} starts ends with exactly
    one [Turn_end] (also when the process exits, or does not answer an
    interrupt within the grace period). *)
module type S = sig
  type t

  (** ["codex"], ["claude"]: the prefix of the models the session records. *)
  val id : string

  (** ["Codex"], ["Claude Code"]. *)
  val display_name : string

  (** What it can do before its process says more ({!current_capabilities}). *)
  val capabilities : Capabilities.t

  (** What the running harness can do: an ACP agent says, at [initialize]
      and when the session opens. *)
  val current_capabilities : t -> Capabilities.t

  (** Whether the harness asks the user questions itself (else prigh's
      [ask_user] tool is offered to it). *)
  val native_questions : bool

  val launch : Start.t -> Launch.t

  (** Talks to the process [transport] runs: the handshake, then the thread
      (new, resumed or forked). Fails, saying why, when the process does not
      answer as expected or exits. [interrupt_grace]: how long an
      interrupted turn may take to end before it is ended for it (10s).
      [injected]: prigh's tools, for a harness that carries MCP in-band
      (Claude Code); the others reach them through [Start.mcp_servers]. *)
  val start
    :  sw:Switch.t
    -> ?interrupt_grace:Time_float.Span.t
    -> ?injected:Injected_tools.t
    -> Line_transport.t
    -> Start.t
    -> t Or_error.t

  val next_event : t -> Adapter_event.t

  (** Starts a turn with [input]. Fails when one is running or the process
      exited. *)
  val send : t -> Settings.t -> Input.t -> unit Or_error.t

  (** Adds [input] to the running turn, when the harness can. *)
  val steer : t -> Input.t -> unit Or_error.t

  (** Asks the running turn to stop: it ends [Interrupted]. *)
  val interrupt : t -> unit

  val reply_approval
    :  t
    -> request_id:string
    -> Adapter_event.Approval_reply.t
    -> unit

  val answer : t -> request_id:string -> User_question.Reply.t -> unit

  val rollback : Rollback.t

  (** Whether it forks a thread at a turn itself (else a fork is a new
      thread, handed the session). *)
  val fork_native : bool

  (** Drops the turns from [before_turn_id] on from the thread (not the
      files: those are prigh's to restore). Only for {!Rollback.Revert}. *)
  val revert : t -> before_turn_id:string -> unit Or_error.t

  (** After a turn: where its conversation ends, for {!Rollback.Resume_at}
      (Claude Code: the id of its last message). *)
  val mark : t -> string option

  (** The thread to resume later. *)
  val thread_id : t -> string option

  val in_turn : t -> bool

  (** Whether its process still runs. *)
  val alive : t -> bool

  (** Stops the process. *)
  val stop : t -> unit
end
