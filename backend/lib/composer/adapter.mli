open! Core
open! Import

(** The external harnesses a composer session drives: their interface
    ({!S}), and the machinery each one's process shares ({!Driver}). *)

include module type of struct
  include Adapter_intf
end

(** An adapter of any harness. *)
type t = (module S)

(** What every adapter does with its process: reads its lines in a fiber
    and hands them to the codec, queues the events, matches answers to the
    requests waiting for them, and settles the running turn when the
    process exits or ignores an interrupt. ['r] is the codec's answers.

    {b Turns.} Only the events of the turn prigh started ({!begin_turn}
    until its [Turn_end]) reach {!next_event}, with those that belong to no
    turn ([Thread_started], [Usage], [Notice], [Compacted], [Retrying],
    [Request_withdrawn]). A harness also runs turns of its own (Codex's
    goal continuations, Claude Code's after a background task): their
    events are kept from {!next_event}, their requests refused
    ({!on_orphan_request}), and their end becomes a [Notice] saying what
    they said. The next turn waits for such a turn to end ({!await_idle}),
    so a [Turn_end] on {!next_event} always ends the turn prigh started,
    and a turn ending early (an exception, an interrupt the harness
    ignored) cannot shift later turns onto the wrong events. *)
module Driver : sig
  type 'r t

  (** [name]: the harness's, for messages. [request_timeout]: how long
      {!request} waits for an answer by default (60s). *)
  val create
    :  sw:Switch.t
    -> name:string
    -> ?interrupt_grace:Time_float.Span.t
    -> ?request_timeout:Time_float.Span.t
    -> Line_transport.t
    -> 'r t

  (** Starts reading: [handle_line] gets each line, [on_exit] the process's
      exit (before the driver ends a turn still running, as [Crashed]). *)
  val run
    :  'r t
    -> handle_line:(string -> unit)
    -> on_exit:(Line_transport.Exit.t -> unit)
    -> unit

  (** Writes [line] to the process; nothing once it is stopping. *)
  val write : _ t -> string -> unit

  (** Takes an event from the codec ({!Driver} above says where it goes). *)
  val push : _ t -> Adapter_event.t -> unit

  (** How to refuse an approval or a question the harness asks in a turn of
      its own, which nobody would answer (the default leaves it). *)
  val on_orphan_request : _ t -> (Adapter_event.t -> unit) -> unit

  val next_event : _ t -> Adapter_event.t

  (** Gives an answer to the first request waiting for one like it. *)
  val answer : 'r t -> 'r -> unit

  (** Writes [line] and waits for an answer [matches] accepts, at most
      [timeout] (default: the driver's). Fails when the process exits or
      stops first, or does not answer in time. Cancelling the calling fiber
      withdraws the request. *)
  val request
    :  ?timeout:Time_float.Span.t
    -> 'r t
    -> string
    -> matches:('r -> bool)
    -> 'r Or_error.t

  (** Waits until the harness runs no turn of its own (saying so on
      {!next_event} when it has to wait): before a turn is started. *)
  val await_idle : _ t -> unit

  (** A turn starts: {!in_turn} until its [Turn_end]. *)
  val begin_turn : _ t -> unit

  val in_turn : _ t -> bool

  (** After an interrupt: if the turn has not ended within the grace
      period, ends it ([Interrupted]) and stops the process, which a later
      turn starts again. *)
  val watch_interrupt : _ t -> unit

  (** Whether the process runs and is not being stopped. *)
  val alive : _ t -> bool

  (** Stops the process: a turn it runs ends at once ([Crashed]). *)
  val stop : _ t -> unit

  (** Why the harness did not start, saying what to do: when its process
      exited, its exit code and the first lines of its stderr, then [hint]'s
      advice for that exit, else to check it in Settings → Engines; [e] when
      it runs but did not answer. *)
  val start_failed
    :  ?hint:(Line_transport.Exit.t -> string option)
    -> _ t
    -> Error.t
    -> Error.t

  (** Why the process ended, for errors: its exit code and stderr. *)
  val exit_message
    :  ?tail:bool (** with the end of its stderr (default: true) *)
    -> display_name:string
    -> Line_transport.Exit.t
    -> string
end
