open! Core
open! Import

(** A tool that delegates a task to a nested agent with its own context,
    tool set, model, engine and turn budget. At depth 0 with
    [Tool.context.background] set it starts the agent in the background and
    returns its id; otherwise it blocks and returns the final reply.

    By default a subagent runs on the same engine and tool host as its
    caller. With [worktree] it works in a git worktree made for it on the
    tool host (under [worktrees_dir], default [~/.prigh/worktrees]), whose
    branch its report names. With [host] it works on that tool host: every
    call of its tools (or its external agent CLI) runs there
    ([Tool.Context.host]), from that host's home (or [cwd] there), its
    worktree too; [computer] (the active host's screen) is dropped. With
    [engine] it runs on that deployed engine or adapter ([harness],
    [codex], [claude], [acp:NAME], ...). A subagent's own subagents stay on
    its host and engine unless they name another. *)

val max_turns : int
val name : string
val continue_name : string

(** Whether a call's arguments ask for a worktree of its own. *)
val wants_worktree : Json.t -> bool

(** The [engine] argument a call names, trimmed ([None] when omitted or
    empty). *)
val wanted_engine : Json.t -> string option

(** How [subagent] runs on the backend's deployed engines (the composer's
    external agent CLIs as well as the harness). *)
module External : sig
  type run_result =
    { messages : Message.t list
    ; cost_usd : float
    }

  type t =
    { deployed : Engines.t
    ; current : unit -> Engine_choice.t
    ; harness_model : unit -> Model.t
      (** prigh's own harness model, used when a composer session starts a
          harness subagent without naming a model *)
    ; external_models : host:string -> Adapter_id.t -> Model.t list option
      (** the models [adapter] listed on [host], if known *)
    ; parent_messages : unit -> Message.t list
      (** the main session's messages, for [fork] when the caller runs on
          the composer *)
    ; approval_mode : unit -> Approval_mode.t
    ; plan_mode : unit -> bool
    ; add_spend : usage:Usage.t -> cost_usd:float -> unit
      (** records spend reported by an external harness on the parent
          session *)
    ; run :
        adapter:Adapter_id.t
        -> host_id:string
        -> host_name:string
        -> cwd:string
        -> model:string option
        -> thinking:Thinking.t
        -> approval:Approval_mode.t
        -> plan:bool
        -> developer_instructions:string option
        -> resume:string option
        -> handoff_from:Engine_choice.t option
        -> session:Session.t
        -> on_thread:(string -> unit)
        -> depth:int
        -> agent_id:string
        -> home:(unit -> string option)
        -> tools:Tool.t list
        -> context:Tool.Context.t
        -> emit:(Agent_event.t -> unit)
        -> steer:(unit -> Message.t list)
        -> await_steer:(unit -> unit)
        -> wake:(unit -> Message.t list)
        -> cancel:Cancellation.t
        -> prompts:Message.t list
        -> run_result
    }
end

val create
  :  ?name_task:
       (task:string
        -> siblings:string list
        -> cancel:Cancellation.t
        -> string option)
       (** names a subagent the model gave no [name], given the names of
           the caller's other background subagents ({!Subagent_name});
           without it, or when it gives [None], the name comes from the
           task *)
  -> ?models:Model_registry.t (** what the [model] argument resolves against *)
  -> ?worktrees_dir:(unit -> string) (** the config's, when the tool runs *)
  -> ?compaction:(unit -> Compaction_settings.t)
       (** the session's, when the tool runs (default:
           {!Compaction_settings.window_only}) *)
  -> ?user_info:(unit -> string option)
       (** {!User_info}'s text when the tool runs, for the system prompt of
           a subagent that has its own (default: none) *)
  -> ?custom_instructions:(unit -> Custom_instruction.t list)
       (** the user's custom instructions when the tool runs, likewise
           (default: none) *)
  -> ?hosts:(unit -> Tool_on_host.Host.t list)
       (** the session's connected tool hosts, for [host] (default: none) *)
  -> ?engines:(unit -> External.t option)
       (** deployed engines and how to run external adapters (default: the
           harness alone) *)
  -> provider:Provider.t
  -> current_model:(unit -> Model.t)
  -> current_thinking:(unit -> Thinking.t)
  -> home:string
  -> unit
  -> Tool.t

(** [subagent_status], [subagent_cancel], [subagent_message] and
    [subagent_continue], which act on the context's background subagents. *)
val control_tools : Tool.t list
