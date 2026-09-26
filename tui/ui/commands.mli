open! Core

(** The slash-command table. *)

module Argument : sig
  type t =
    | Model
    | Thinking
    | Verbosity
    | Confirm
    | Login
    | Logout
    | Sessions
    | Path
    | Directory
  [@@deriving sexp_of, equal]
end

module Spec : sig
  type t =
    { name : string
    ; args : string
    ; help : string
    ; argument : Argument.t option
    }
  [@@deriving sexp_of, equal]
end

val all : Spec.t list
val find : string -> Spec.t option

(** [/thinking] levels in Ctrl+T cycling order. *)
val thinking_levels : string list

module Parsed : sig
  type t =
    { name : string
    ; args : string list
    ; rest : string
    }
  [@@deriving sexp_of]
end

(** [None] unless the input starts with [/]. *)
val parse : string -> Parsed.t option

val closest : string -> Spec.t option
val help : Content.t
