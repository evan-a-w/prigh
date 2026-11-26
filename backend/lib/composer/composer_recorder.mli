open! Core
open! Import

(** Turns what a composer adapter reports ({!Adapter_event.t}) into what
    the harness engine produces for the same run: the [Agent_event.t]s its
    loop emits (assistant messages streamed with [Message_start]/
    [Message_update]/[Message_end], tool calls with [Tool_start]/
    [Tool_output]/[Tool_end] and their results, [Turn_start]/[Turn_end],
    subagents' nested events) and the messages it adds. So the session log
    of a composer run reads like a harness run's: tool calls are recorded as
    the prigh tools they amount to ({!Canonical_tool}), and the existing
    renderers, approval rules, search, outline and plan tracking apply.

    An assistant message holds what the harness says up to the tool calls
    it makes next: it ends (and its calls start) at the first event after
    its calls that is not another call. Plans ([Plan]) are recorded as
    [update_plan] calls. Usage is attached to the next assistant message
    that ends; reported cost is summed ({!cost_usd}) for the runner to
    record as spend. Approvals and questions are the runner's; it tells the
    recorder how approvals went ({!note_approval}) so a denied call's
    result says so. *)

type t

(** Starts a run: emits [Agent_start] and the [prompts] (already in the
    session) as the harness does. Assistant messages are recorded with the
    model [ADAPTER/MODEL] ([default] until the harness names it). *)
val create
  :  adapter:string
  -> model:string option
  -> cwd:string
  -> emit:(Agent_event.t -> unit)
  -> notice:(string -> unit)
  -> prompts:Message.t list
  -> t

val feed : t -> Adapter_event.t -> unit

(** The harness was handed a turn: emits [Agent_waiting], and again
    whenever it goes silent within the turn (its calls ended, a steer was
    passed on, before a message starts), until the turn ends. *)
val waiting : t -> unit

(** The harness's process is starting (or resuming its thread): emits
    [Agent_waiting] with [starting]. *)
val starting : t -> unit

(** A message the user sent while the harness worked (a steer the runner
    passed on): recorded and emitted at once when no call is open, else
    after the open calls end (by the end of the turn at the latest), so
    calls stay next to their results. *)
val user_message : t -> Message.t -> unit

(** How the user answered the approval of the call [tool_id]. *)
val note_approval
  :  t
  -> tool_id:string
  -> Adapter_event.Approval_reply.t
  -> unit

(** Ends the run: closes what is still open (as interrupted when the
    harness never ended its turn), emits [Agent_end] and returns the
    messages the run added, prompts first (as [Agent_loop.run] does). *)
val finish : t -> Message.t list

(** How the harness ended its last turn, if it did. *)
val stop : t -> Adapter_event.Stop.t option

val thread_id : t -> string option
val model : t -> string

(** The cost the harness reported for the run so far, in USD. *)
val cost_usd : t -> float

(** The latest context size the harness reported: tokens, window. *)
val context : t -> int option * int option

(** The assistant message's error text for a failed turn: what
    [Usage_limit.unavailable] recognises for a usage limit (with the reset
    time), the hint for missing credentials or installs. *)
val error_text : Adapter_event.Error_kind.t -> message:string -> string

(** Shows the call [tool_id] as started (ending the assistant message that
    made it), for an approval of it: its prigh form, and the [Subagent]
    call it runs under. [None] when no such call was seen. *)
val call_started
  :  t
  -> tool_id:string
  -> (Canonical_tool.t * string option) option
