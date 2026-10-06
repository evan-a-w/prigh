open! Core

(** The slash-command table. *)

module Argument : sig
  type t =
    | Model
    | Models (** several, completed word by word ([off] first) *)
    | Thinking
    | Verbosity
    | Confirm
    | Login
    | Logout
    | Sessions
    | Path
    | Directory
    | Default_directory
    (** a directory on the backend's host when it runs tools, else on the
        active tool host *)
    | Skill (** [/skill:NAME]: the name is part of the command *)
    | Mcp
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

(** [/name args], as [/help] shows it. *)
val usage : Spec.t -> string

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

(** The prefixes a prompt can start with ([!command], [@path], ...). *)
val input_help : Content.t
