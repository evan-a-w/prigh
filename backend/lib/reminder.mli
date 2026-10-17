open! Core
open! Import

(** A reminder the harness sends the main agent (never a subagent), from the
    config's [reminders]: when it ends its turn ([on: stop]), e.g. not to
    stop before the work is done, or every so many turns ([every_turns]),
    e.g. to clean up disk space. *)

module Trigger : sig
  type t =
    | On_stop of { max_repeats : int }
    (** when the agent ends its turn with nothing left to wait for, at most
        [max_repeats] times in a row before the user speaks again *)
    | Every_turns of int (** every this many turns of the main agent *)
  [@@deriving sexp_of, equal]
end

type t =
  { name : string option
  ; text : string
  ; trigger : Trigger.t
  }
[@@deriving sexp_of, equal]

val default_max_repeats : int

(** [{"name", "text", "on": "stop", "max_repeats"}] or [{"name", "text",
    "every_turns"}]; errors say what is wrong without saying where. *)
val of_json : Json.t -> t Or_error.t

val to_json : t -> Json.t

(** The name, else the first line of the text. *)
val label : t -> string

(** What the model gets, e.g. [\[prigh reminder: disk, every 40 turns\] TEXT];
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

  (** The periodic reminders due now, which count as fired. [reminders] is
      the config's list: reminders are told apart by their place in it. *)
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
