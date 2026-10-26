open! Core

(** The slash-command table. *)

module Argument : sig
  type t =
    | Model
    | Models (** several, completed word by word ([off] first) *)
    | Thinking
    | Verbosity
    | Confirm
    | Mode
    | Plan
    | Notify (** [off], [bell] or [desktop] *)
    | Login
    | Logout
    | Sessions
    | Session_list (** several, completed word by word *)
    | Path
    | Directory
    | Default_directory
    (** a directory on the backend's host when it runs tools, else on the
        active tool host *)
    | Skill (** [/skill:NAME]: the name is part of the command *)
    | Mcp
    | Budget (** [off], or dollars *)
    | Goal (** [off], [resume], or the goal *)
    | Resume (** [now] or [cancel] *)
    | Snippet (** a snippet name, [add] or [rm] *)
    | Remind (** a [/remind] subcommand, or a reminder name after [rm] *)
    | Worktree
    (** a [/worktree] subcommand, then a worktree, flag or base branch *)
    | Server (** [/connect]: [local], the current or the saved address *)
    | Pr (** [/pr]'s subcommands *)
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
