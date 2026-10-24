open! Core
open! Import

(** A reminder the harness sends the main agent (never a subagent): when it
    ends its turn ([/remind stop TEXT]), e.g. not to stop before the work is
    done, or every so many turns ([/remind every N TEXT]), e.g. to clean up
    disk space. A session's reminders are recorded in it (see
    [Session.reminders]). *)

module Trigger : sig
  type t =
    | On_stop of { max_repeats : int }
    (** when the agent ends its turn with nothing left to wait for, at most
        [max_repeats] times in a row before the user speaks again *)
    | Every_turns of int (** every this many turns of the main agent *)
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { name : string (** [r1], [r2], ...: unique within the session *)
  ; text : string
  ; trigger : Trigger.t
  }
[@@deriving sexp, jsonaf, equal]

val default_max_repeats : int

(** A reminder named after the highest [rN] in [taken] (every name the
    session ever used), so that names are never reused. Fails on an empty
    text or a count below 1, saying what to type instead. *)
val create : taken:string list -> text:string -> Trigger.t -> t Or_error.t

(** [r2: TEXT's first line]. *)
val label : t -> string

(** What the model gets, e.g. [\[prigh reminder: r2, every 40 turns\] TEXT];
    [repeat] counts the times an on-stop reminder fired in a row. *)
val message_text : t -> repeat:int option -> string

(** The main agent's progress that reminders depend on, for one session. *)
module Tracker : sig
  type reminder := t
  type t

  val create : unit -> t

  (** Starts over, as for a new session. *)
  val reset : t -> unit

  (** On-stop reminders fire again up to their [max_repeats]. *)
  val user_spoke : t -> unit

  (** A turn of the main agent ended. *)
  val turn_ended : t -> unit

  (** The times on-stop reminders fired in a row since the user spoke. *)
  val stops : t -> int

  (** The periodic reminders due now, which count as fired. A reminder not
      seen before counts its turns from now. *)
  val due : t -> reminder list -> reminder list

  module Stop : sig
    type t =
      { fire : (reminder * int) list
        (** the on-stop reminders to send, each with its repeat (from 1) *)
      ; used_up : reminder list
        (** the ones that fired [max_repeats] times in a row and now stay
          quiet until the user speaks; each is reported once *)
      }
    [@@deriving sexp_of]
  end

  (** The agent ended its turn with nothing to wait for. *)
  val stopped : t -> reminder list -> Stop.t
end
