open! Core
open! Import

(** Runs a subagent on one of the composer's external agent CLIs (Codex,
    Claude Code, ACP agents) on any connected tool host, using
    {!Composer_runner} over a temporary in-memory {!Session.t}. Each run
    stops its CLI process when done, recording its thread id so a later
    continuation resumes it (or falls back to a handoff). *)

val create
  :  engines:Engines.t
  -> current:(unit -> Engine_choice.t)
  -> harness_model:(unit -> Model.t)
  -> external_models:(host:string -> Adapter_id.t -> Model.t list option)
  -> parent_messages:(unit -> Message.t list)
  -> approval_mode:(unit -> Approval_mode.t)
  -> plan_mode:(unit -> bool)
  -> add_spend:(usage:Usage.t -> cost_usd:float -> unit)
  -> notice:(string -> unit)
  -> spawn:
       (sw:Switch.t
        -> host:string
        -> host_name:string
        -> adapter:Adapter_id.t
        -> Adapter.Launch.t
        -> Line_transport.t Or_error.t)
  -> ?on_start:
       (host:string
        -> adapter:Adapter_id.t
        -> Adapter.Start.t
        -> unit Or_error.t
        -> unit)
  -> ?login:(adapter:string -> Cli_credential.t option)
  -> ?no_sandbox:
       (host:string
        -> host_name:string
        -> cwd:string
        -> Adapter_id.t
        -> string option)
  -> ?catalog:(host:string -> Adapter_id.t -> Adapter.Catalog.t option)
  -> ?request_timeout:Time_float.Span.t
  -> ?interrupt_grace:Time_float.Span.t
  -> unit
  -> Tool_subagent.External.t
