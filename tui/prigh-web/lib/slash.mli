open! Core

(** prigh-web's slash commands. *)

module Argument : sig
  (** What the completion popup offers after the command name. *)
  type t =
    | Model
    | Models (** model keys, one per word, then [off] *)
    | Thinking
    | Login
    | Logout
    | Directory
    | Backend_directory (** directories on the backend's host *)
    | Verbosity
    | Confirm
    | Session (** saved sessions' paths *)
    | Path (** [list_paths] *)
    | Host
    | User (** the users a superuser can act as *)
    | Skill (** right after [/skill:], from [list_skills] *)
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

  (** [/name args]; names ending in [:] (like [skill:]) take their argument
      without a space. *)
  val usage : t -> string
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
