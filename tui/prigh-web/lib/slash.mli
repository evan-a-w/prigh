open! Core

(** prigh-web's slash commands. *)

module Argument : sig
  (** What the completion popup offers after the command name. *)
  type t =
    | Model
    | Thinking
    | Login
    | Logout
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

(** The command with the nearest name (within two edits, or a prefix). *)
val closest : string -> Spec.t option

module Parsed : sig
  type t =
    { name : string
    ; rest : string (** the arguments, stripped *)
    }
  [@@deriving sexp_of, equal]
end

(** [None] unless [text] is one line starting with [/] (so pasting a path
    like [/etc/hosts] with a question still sends a prompt). *)
val parse : string -> Parsed.t option
