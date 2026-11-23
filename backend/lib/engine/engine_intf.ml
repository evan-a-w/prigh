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
  module Input = struct
    type t =
      { context : Message.t list (** the session's context before [prompts] *)
      ; prompts : Message.t list (** already in the session *)
      ; chat : bool (** chat mode: no tools *)
      ; thinking : Thinking.t
      ; approval : Approval_mode.t
      ; plan : bool (** plan mode *)
      }
  end
end

(** A session's engine for its runs. *)
module type Runner = sig
  type t

  val engine : Engine_id.t
  val capabilities : t -> Capabilities.t

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
      external harness's process). *)
  val create : ?close:(unit -> unit) -> (module S with type t = 'a) -> 'a -> t

  val close : t -> unit
  val engine : t -> Engine_id.t
  val capabilities : t -> Capabilities.t

  val run_turn
    :  t
    -> Services.t
    -> cancel:Cancellation.t
    -> Turn.Input.t
    -> Message.t list
end
