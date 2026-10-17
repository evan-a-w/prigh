open! Core

(** One of the session's reminders ([/remind]), as the state reports it. *)

module Trigger : sig
  type t =
    | On_stop of
        { max_repeats : int
        ; repeats : int (** the times it fired in a row since the user spoke *)
        } (** when the agent ends its turn *)
    | Every_turns of int
  [@@deriving sexp_of, equal]

  (** [on stop 1/3], [every turn], [every 5 turns]. *)
  val describe : t -> string
end

type t =
  { name : string (** [r1], [r2], ...: unique within the session *)
  ; text : string
  ; trigger : Trigger.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
val find : t list -> string -> t option

(** The text's first non-blank line, for lists. *)
val first_line : t -> string
