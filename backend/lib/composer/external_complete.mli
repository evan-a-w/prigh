open! Core
open! Import

(** One-shot, tool-less text completion through an external harness adapter
    (ACP, Codex, Claude Code). Each adapter runs an ephemeral session/thread
    with no MCP servers or tools, auto-denies any unexpected approval or
    question request, streams text deltas, and cleans up on finish, timeout or
    cancellation. *)

val default_timeout : Time_float.Span.t

(** Picks a fast/cheap model id from [models] (preferring flash-lite, flash,
    haiku, mini, nano/lite/fast, ignoring placeholders); [None] leaves the
    adapter on its default. *)
val pick_fast_model : Model.t list -> string option

(** Instruction reminding agentic harnesses not to invoke tools. *)
val no_tools_note : string

(** Appends {!no_tools_note} to [system] for harnesses with a system-prompt
    channel (Codex, Claude Code). *)
val system_with_no_tools : string -> string

(** Combines [system], {!no_tools_note} and [prompt] into one user message for
    harnesses without a separate system-prompt channel (ACP). *)
val combine_prompt : system:string -> prompt:string -> string

(** Runs [f ()], failing with ["cancelled"] or a timeout error if [cancel] or
    [timeout] fires first. *)
val with_cancel_timeout
  :  cancel:Cancellation.t
  -> timeout:Time_float.Span.t
  -> (unit -> 'a Or_error.t)
  -> 'a Or_error.t

(** Drives a single turn of a started external adapter to completion,
    streaming text to [on_delta], auto-denying approvals and declining
    questions, and calling [interrupt] if [cancel] or [timeout] fires. *)
val drive
  :  ?timeout:Time_float.Span.t
  -> ?on_delta:(string -> unit)
  -> cancel:Cancellation.t
  -> next_event:(unit -> Adapter_event.t)
  -> interrupt:(unit -> unit)
  -> reply_approval:
       (request_id:string -> Adapter_event.Approval_reply.t -> unit)
  -> answer:(request_id:string -> User_question.Reply.t -> unit)
  -> model:string option
  -> unit
  -> Adapter.Completion.t Or_error.t

(** Spawns [adapter] in lightweight completion mode ({!Adapter.S.launch_complete}),
    runs one completion ({!Adapter.S.complete}), and guarantees the transport
    and reader fibers are stopped on exit. *)
val run
  :  adapter:Adapter.t
  -> spawn:(Adapter.Launch.t -> Line_transport.t Or_error.t)
  -> ?login:Cli_credential.t
  -> ?no_sandbox:string
  -> ?catalog_models:Model.t list
  -> ?timeout:Time_float.Span.t
  -> ?request_timeout:Time_float.Span.t
  -> ?on_delta:(string -> unit)
  -> cancel:Cancellation.t
  -> cwd:string
  -> model:string option
  -> system:string
  -> prompt:string
  -> unit
  -> Adapter.Completion.t Or_error.t
