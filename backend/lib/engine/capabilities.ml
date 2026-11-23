open! Core
open! Import

module History_op = struct
  type t =
    | Unsupported
    | Native
    | By_handoff
  [@@deriving sexp_of, equal]
end

module Compact = struct
  type t =
    | Unsupported
    | Native
    | Auto_only
  [@@deriving sexp_of, equal]
end

module Steer = struct
  type t =
    | Between_turns
    | In_run
  [@@deriving sexp_of, equal]
end

module Subagents = struct
  type t =
    | Unsupported
    | Observe
    | Full
  [@@deriving sexp_of, equal]
end

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
[@@deriving sexp_of]

let harness =
  { fork = Native
  ; rewind = Native
  ; context_edit = true
  ; compact = Native
  ; steer = In_run
  ; plan_mode = true
  ; btw = true
  ; subagents = Full
  ; model_fallback = true
  ; images = true
  ; ask_user = true
  }
;;
