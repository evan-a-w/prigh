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
    process exits or ignores an interrupt. ['r] is the codec's answers. *)
module Driver : sig
  type 'r t

  val create
    :  sw:Switch.t
    -> ?interrupt_grace:Time_float.Span.t
    -> Line_transport.t
    -> 'r t

  (** Starts reading: [handle_line] gets each line, [on_exit] the process's
      exit (before the driver ends a turn still running, as [Crashed]). *)
  val run
    :  'r t
    -> handle_line:(string -> unit)
    -> on_exit:(Line_transport.Exit.t -> unit)
    -> unit

  val write : _ t -> string -> unit

  (** Queues an event; a [Turn_end] ends the turn. *)
  val push : _ t -> Adapter_event.t -> unit

  val next_event : _ t -> Adapter_event.t

  (** Gives an answer to the first request waiting for one like it. *)
  val answer : 'r t -> 'r -> unit

  (** Writes [line] and waits for an answer [matches] accepts. Fails when
      the process exits first. *)
  val request : 'r t -> string -> matches:('r -> bool) -> 'r Or_error.t

  (** A turn starts: {!in_turn} until its [Turn_end]. *)
  val begin_turn : _ t -> unit

  val in_turn : _ t -> bool

  (** After an interrupt: if the turn has not ended within the grace
      period, ends it ([Interrupted]) and stops the process, which a later
      turn starts again. *)
  val watch_interrupt : _ t -> unit

  val alive : _ t -> bool
  val stop : _ t -> unit

  (** Why the process ended, for errors: its exit code and stderr. *)
  val exit_message
    :  ?tail:bool (** with the end of its stderr (default: true) *)
    -> display_name:string
    -> Line_transport.Exit.t
    -> string
end
