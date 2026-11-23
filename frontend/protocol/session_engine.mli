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
    ; rewind_note : string option
      (** why it cannot rewind, for a reason of the session's *)
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

(** How sessions of an adapter are named: [codex] is "Codex", [claude]
    "Claude Code", [acp:NAME] "NAME (ACP)". *)
val adapter_label : string -> string

(** "Harness", "Composer". *)
val engine_label : string -> string

(** Who the session's messages name: its adapter's label, else its
    engine's. *)
val who : t -> string

(** The model as the status line and the model chip show it: harness
    sessions the model alone, others "Codex · MODEL". *)
val model_label : t -> model:string -> string

(** What a request can need of the session's engine (the backend's
    [Capabilities.Feature]). *)
module Feature : sig
  type t =
    | Fork
    | Rewind
    | Context_edit (** trim, delete and their undos *)
    | Compact (** [/compact] on request *)
    | Downsize_images
    | Btw
    | Steer_subagent
    | Plan_mode
  [@@deriving sexp_of, equal, enumerate]
end

val supports : t -> Feature.t -> bool

(** Why the session's engine cannot do it, and what to do instead, in the
    backend's words. *)
val refusal : t -> Feature.t -> string

(** The feature a request needs, if any ([set_mode] needs [Plan_mode] when
    it turns plan mode on). *)
val method_feature
  :  method_:string
  -> params:(string * Json.t) list
  -> Feature.t option

(** Why the session would refuse the request: a feature its engine lacks,
    or, for what starts a run, the session being [unavailable]; [None] to
    send it. *)
val refuse : t -> method_:string -> params:(string * Json.t) list -> string option
