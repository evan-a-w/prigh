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
    ; cost_total : float option
      (** the running cost total [resume] reported last: the harnesses that
          report a session's cost as a running total carry it on when the
          thread is continued, so only what comes after it is new spend *)
    ; developer_instructions : string option
      (** appended to the harness's own system prompt *)
    ; mcp_servers : (string * Json.t) list
      (** stdio MCP servers to give it: name and [{command, args, env}] *)
    ; login : Cli_credential.t option
      (** one of prigh's logins to run on, for a harness that can take one
          ({!Cli_login}); [None]: its own *)
    ; login_refused : (string option[@sexp.opaque])
      (** the token of [login] the previous process was refused with: this
          one wants a newer one ({!S.renewal}) *)
    ; no_sandbox : string option
      (** the harness's own sandbox cannot run on the tool host: where and
          why (e.g. ["laptop (no user namespaces)"]). It runs without it,
          still asking prigh before what the session's mode asks about. *)
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
    ; cost_total = None
    ; developer_instructions = None
    ; mcp_servers = []
    ; login = None
    ; login_refused = None
    ; no_sandbox = None
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
  (** A secret for the process, kept out of its environment where its host
      can: there the commands it runs would inherit it. *)
  module Secret = struct
    type t =
      { fd_env : string
        (** the variable naming the descriptor it is read from, on a host
            that can hand it so ({!Host_process}'s [secret]) *)
      ; env : string (** the variable carrying it, on another host *)
      ; value : string [@sexp.opaque]
      }
    [@@deriving sexp_of]
  end

  type t =
    { argv : string list
    ; env_set : (string * string) list
    ; env_unset : string list
    ; cwd : string
    ; mcp : bool
      (** with an MCP side channel, through which prigh's tools reach a
          harness that has no MCP of its own in its protocol *)
    ; secret : Secret.t option
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

(** What a harness says of its login without a thread: its models (its
    default first, as {!External_model.default_id}) and, for a subscription
    login, how much of its usage limits it has used. *)
module Catalog = struct
  type t =
    { models : Model.t list
    ; limits : Subscription_usage.Status.t option
    }
  [@@deriving sexp_of]
end

(** Why a harness on one of prigh's logins wants its process replaced
    before the next turn, its thread continued ({!S.renewal}): for a harness
    that takes the login only when it starts (Claude Code). *)
module Login_renewal = struct
  type t =
    | Expiring (** the token it was given expires soon *)
    | Refused of string (** it was refused with this token *)
  [@@deriving sexp_of, equal]
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

  (** Whether its process is started when a session opens on it, before the
      first turn ({!Runner.prewarm}): for a harness slow to start whose
      running process says all its catalog would (ACP agents; Codex and
      Claude Code say their limits only when asked or after a turn). *)
  val starts_early : bool

  val launch : Start.t -> Launch.t

  (** Talks to the process [transport] runs: the handshake, then the thread
      (new, resumed or forked). Fails, saying why, when the process does not
      answer as expected, in time ([request_timeout] for each request, 60s),
      or exits. Cancelling the calling fiber abandons it. [interrupt_grace]: how long an
      interrupted turn may take to end before it is ended for it (10s).
      [injected]: prigh's tools, for a harness that carries MCP in-band
      (Claude Code); the others reach them through [Start.mcp_servers]. *)
  val start
    :  sw:Switch.t
    -> ?interrupt_grace:Time_float.Span.t
    -> ?request_timeout:Time_float.Span.t
    -> ?injected:Injected_tools.t
    -> Line_transport.t
    -> Start.t
    -> t Or_error.t

  val next_event : t -> Adapter_event.t

  (** Starts a turn with [input]. Fails when one is running or the process
      exited. *)
  val send : t -> Settings.t -> Input.t -> unit Or_error.t

  (** The models the running harness offers, once it said ([] before):
      its default first, as {!External_model.default_id}. *)
  val models : t -> Model.t list

  (** How much of its login's usage limits it said were used, last ([None]
      before it said, or for a login without limits). *)
  val limits : t -> Subscription_usage.Status.t option

  (** [f] is called whenever {!models} or {!limits} change. *)
  val on_change : t -> (unit -> unit) -> unit

  (** [f] is called when the harness starts a turn of its own between
      prompts (e.g. woken by a background task), so the session can drive
      and record it as a run. *)
  val on_autonomous : t -> (unit -> unit) -> unit

  (** Runs the process [transport] runs only to ask its {!Catalog}: the
      handshake, its models and its login's limits, opening no thread
      (nothing is recorded with the harness); the caller stops the
      process. Fails for a harness that cannot say without a thread. *)
  val catalog
    :  sw:Switch.t
    -> ?request_timeout:Time_float.Span.t
    -> Line_transport.t
    -> Start.t
    -> Catalog.t Or_error.t

  (** Compacts the thread's context ([/compact]): runs as a turn, ending
      with [Turn_end]. *)
  val compact : t -> Settings.t -> instructions:string option -> unit Or_error.t

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

  (** The running cost total the harness reported last for its thread, to
      seed {!Start.cost_total} when the thread is continued; [None] for one
      that reports no running total. *)
  val cost_total : t -> float option

  val in_turn : t -> bool

  (** Whether its process still runs. *)
  val alive : t -> bool

  (** The login of prigh's its process runs on ([Start.login], unless it
      could not take it); [None]: its own. *)
  val login : t -> Cli_credential.t option

  (** Whether its process should be replaced before the next turn: see
      {!Login_renewal}. A harness that renews its login itself: [None]. *)
  val renewal : t -> Login_renewal.t option

  (** Stops the process. *)
  val stop : t -> unit
end
