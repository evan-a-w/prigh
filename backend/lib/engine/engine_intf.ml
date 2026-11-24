open! Core
open! Import

(** What the agent (the engine-independent core of a session) gives an
    engine's runner for a run: where it works, how it reports, and the
    session's approvals, questions, tools and queues. *)
module Services = struct
  type t =
    { env : Env.t
    ; cwd : string
    ; session : Session.t (** the session the run started in *)
    ; emit : Agent_event.t -> unit
    ; notice : string -> unit
    ; approve : Tool.approver
    ; ask_user :
        call_id:string
        -> timeout_seconds:int option
        -> User_question.t list
        -> User_question.Reply.t
    ; ui : Ui_host.t
    ; tools : Tool.t list
      (** the session's tools, as the harness engine offers them *)
    ; execute : Tool.executor (** prigh's tools, on the session's host *)
    ; background : Background_tasks.t
    ; steer : unit -> Message.t list
      (** what arrived for the running turn: steering messages, deliveries,
        mail, reminders and mode notes *)
    ; halt : unit -> string option
      (** [Some reason]: stop before the next turn *)
    ; add_spend : usage:Usage.t -> cost_usd:float -> unit
      (** records spend the session's messages do not price: what an
          external harness reported its replies cost *)
    }
end

module Turn = struct
  module Kind = struct
    type t =
      | Prompt (** answer the prompts *)
      | Compact of { instructions : string option }
      (** compact the context ([/compact]); there are no prompts *)
  end

  module Input = struct
    type t =
      { context : Message.t list (** the session's context before [prompts] *)
      ; prompts : Message.t list (** already in the session *)
      ; chat : bool (** chat mode: no tools *)
      ; thinking : Thinking.t
      ; approval : Approval_mode.t
      ; plan : bool (** plan mode *)
      ; model : string option
        (** the external harness's model to use: [None] for its default (or,
            in the harness, the session's) *)
      ; kind : Kind.t
      }
  end
end

(** A session's engine for its runs. *)
module type Runner = sig
  type t

  val engine : Engine_id.t
  val capabilities : t -> Capabilities.t

  (** The models the session can switch to, when they are the runner's (an
      external harness's) rather than prigh's. *)
  val models : t -> Model.t list option

  (** The latest context size the runner saw reported: tokens, window. *)
  val context : t -> int option * int option

  (** Runs the agent on [input] until it ends its turn, is cancelled or
      halted, emitting what happens through the services; returns the
      messages it added after the prompts. *)
  val run_turn
    :  t
    -> Services.t
    -> cancel:Cancellation.t
    -> Turn.Input.t
    -> Message.t list
end

module type Runner_api = sig
  module type S = Runner

  (** A runner of any engine. *)
  type t

  (** [close]: stops what the runner keeps running between turns (an
      external harness's process). [rewind]: what the engine must undo outside the session (an external
      harness's conversation and the files it changed) before the session
      is rewound to the entry [to_]; an error leaves both as they were.
      Without it, rewinding is the session's alone. [limits]: how much of
      its login's usage limits an external harness said were used. *)
  val create
    :  ?close:(unit -> unit)
    -> ?rewind:(Session.t -> to_:string -> unit Or_error.t)
    -> ?limits:(unit -> Subscription_usage.Status.t option)
    -> (module S with type t = 'a)
    -> 'a
    -> t

  val close : t -> unit

  (** Undoes, before the session is rewound to [to_], what the engine
      must outside it (see [create]). *)
  val rewind : t -> Session.t -> to_:string -> unit Or_error.t

  val engine : t -> Engine_id.t
  val capabilities : t -> Capabilities.t
  val models : t -> Model.t list option
  val context : t -> int option * int option

  (** See [create]: [None] for the harness. *)
  val limits : t -> Subscription_usage.Status.t option

  val run_turn
    :  t
    -> Services.t
    -> cancel:Cancellation.t
    -> Turn.Input.t
    -> Message.t list
end
