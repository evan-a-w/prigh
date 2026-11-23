open! Core

(** The engine a session runs on, from [state]: absent fields (every
    harness session, and backends from before engines) are the harness,
    which can do everything. *)

module History_op : sig
  type t =
    | Unsupported
    | Native
    | By_handoff (** emulated by forking with a handoff summary *)
  [@@deriving sexp_of, equal]
end

module Compact : sig
  type t =
    | Unsupported
    | Native
    | Auto_only
  [@@deriving sexp_of, equal]
end

module Steer : sig
  type t =
    | Between_turns
    | In_run
  [@@deriving sexp_of, equal]
end

module Subagents : sig
  type t =
    | Unsupported
    | Observe
    | Full
  [@@deriving sexp_of, equal]
end

(** What the session's engine supports (the backend's [Capabilities]). *)
module Capabilities : sig
  type t =
    { fork : History_op.t
    ; rewind : History_op.t
    ; context_edit : bool
    ; compact : Compact.t
    ; steer : Steer.t
    ; plan_mode : bool
    ; btw : bool
    ; subagents : Subagents.t
    ; model_fallback : bool
    ; images : bool
    ; ask_user : bool
    }
  [@@deriving sexp_of, equal]

  val harness : t
end

type t =
  { engine : string (** [harness] or [composer] *)
  ; adapter : string option
    (** the composer's: [codex], [claude], [prigh], [acp:NAME] *)
  ; capabilities : Capabilities.t
  ; unavailable : string option
    (** why the session cannot run on this backend (it can still be read),
        naming what to do *)
  }
[@@deriving sexp_of, equal]

val harness : t
val is_harness : t -> bool

(** From a [state] object's [engine], [adapter], [capabilities] and
    [unavailable]. *)
val of_state_json : Json.t -> t Or_error.t
