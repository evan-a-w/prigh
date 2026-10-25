open! Core

(** A [get_subagent] reply: one subagent's summary and transcript. *)
type t =
  { id : string
  ; call_id : string (** the [subagent] call that started it *)
  ; parent : string option
  ; task : string
  ; model : string
  ; turns : int
  ; result : Event.Subagent_result.t option (** [None] while it runs *)
  ; messages : Message.t list
  }
[@@deriving sexp_of]

val of_json : Json.t -> t Or_error.t
