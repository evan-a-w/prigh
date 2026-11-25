open! Core
open! Import

(** The harness engine's runner: prigh's own loop ({!Agent_loop}) on
    [provider], with the model, tools and system prompt [config] gives at
    the start of each run. Compaction and observation masking act on the
    session the run started in. *)
val create
  :  provider:Provider.t
  -> config:(cancel:Cancellation.t -> Agent_loop.Config.t)
  -> Runner.t

(** Masks [session]'s messages up to its [through]th, recording it. *)
val mask_session : Session.t -> through:int -> Message.t list Or_error.t
