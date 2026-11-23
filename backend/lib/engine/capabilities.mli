open! Core
open! Import

(** What an engine can do with a session, declared honestly: a feature it
    does not support is off, never emulated, so the session's controls can
    be gated on it. *)

module History_op : sig
  type t =
    | Unsupported
    | Native
    | By_handoff
    (** a new thread on the external agent, started with a summary of the
        session so far *)
  [@@deriving sexp_of, equal]
end

module Compact : sig
  type t =
    | Unsupported
    | Native (** on request ([/compact]) and automatically *)
    | Auto_only (** the engine compacts by itself; [/compact] is refused *)
  [@@deriving sexp_of, equal]
end

module Steer : sig
  type t =
    | Between_turns (** messages wait for the end of the run *)
    | In_run (** delivered at the next boundary of the running turn *)
  [@@deriving sexp_of, equal]
end

module Subagents : sig
  type t =
    | Unsupported
    | Observe (** shown as they run, not steerable *)
    | Full
  [@@deriving sexp_of, equal]
end

type t =
  { fork : History_op.t
  ; rewind : History_op.t
  ; context_edit : bool (** trim and delete what the model sees *)
  ; compact : Compact.t
  ; steer : Steer.t
  ; plan_mode : bool
  ; btw : bool
  ; subagents : Subagents.t
  ; model_fallback : bool (** hand over to [fallback_models] *)
  ; images : bool
  ; ask_user : bool
  }
[@@deriving sexp_of]

(** prigh's own harness: everything. *)
val harness : t
