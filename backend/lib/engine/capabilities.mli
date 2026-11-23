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

(** A session whose engine does not run here: nothing but reading it. *)
val none : t

(** What a request can need of its session's engine. *)
module Feature : sig
  type t =
    | Fork (** fork or clone the session *)
    | Rewind
    | Context_edit (** trim, delete and their undos *)
    | Compact (** [/compact] on request *)
    | Downsize_images
    | Btw
    | Steer_subagent (** steer a subagent or edit its queue *)
    | Plan_mode
  [@@deriving sexp_of, equal, enumerate]

  (** As the user would name it: [fork], [rewind], [trim], ... *)
  val name : t -> string
end

val supports : t -> Feature.t -> bool

(** The feature an RPC method needs, if it needs one. [set_mode] needs
    [Plan_mode] only when it turns plan mode on, which the method alone does
    not say: see [Engine_gate]. *)
val method_feature : string -> Feature.t option

(** [Ok ()] when the engine supports the feature; else an error saying what
    to do instead. [who] names the sessions, e.g. ["Codex"]. *)
val check : t -> Feature.t -> who:string -> unit Or_error.t
