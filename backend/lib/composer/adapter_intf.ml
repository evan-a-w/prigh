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
    { model = s.model; thinking = s.thinking; approval = s.approval; plan = s.plan }
  ;;
end

(** The process to run, on the session's tool host. *)
module Launch = struct
  type t =
    { argv : string list
    ; env_set : (string * string) list
    ; env_unset : string list
    ; cwd : string
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

  val capabilities : Capabilities.t
  val launch : Start.t -> Launch.t

  (** Talks to the process [transport] runs: the handshake, then the thread
      (new, resumed or forked). Fails, saying why, when the process does not
      answer as expected or exits. [interrupt_grace]: how long an
      interrupted turn may take to end before it is ended for it (10s). *)
  val start
    :  sw:Switch.t
    -> ?interrupt_grace:Time_float.Span.t
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

  (** The thread to resume later. *)
  val thread_id : t -> string option

  val in_turn : t -> bool

  (** Whether its process still runs. *)
  val alive : t -> bool

  (** Stops the process. *)
  val stop : t -> unit
end
