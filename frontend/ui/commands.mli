open! Core

(** The slash-command table. *)

module Argument : sig
  type t =
    | Model
    | Models (** several, completed word by word ([off] first) *)
    | Thinking
    | Verbosity
    | Filter (** [/filter]: [all] and the kinds *)
    | Confirm
    | Mode
    | Plan
    | Notify (** [off], [bell] or [desktop] *)
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
    | Budget (** [off], or dollars *)
    | Goal (** [off], [resume], or the goal *)
    | Resume (** [now] or [cancel] *)
    | Trim (** [after], [undo], [show] or [hide] *)
    | Delete (** [undo], or several sessions, completed word by word *)
    | Snippet (** a snippet name, [add] or [rm] *)
    | Remind (** a [/remind] subcommand, or a reminder name after [rm] *)
    | Worktree
    (** a [/worktree] subcommand, then a worktree, flag or base branch *)
    | Review (** [/review]: a worktree or [--base], then a base branch *)
    | Server (** [/connect]: [local], the current or the saved address *)
    | Pr (** [/pr]'s subcommands *)
    | Commit (** [/commit]'s flags, until the message *)
    | Agents (** [/agents]' subcommands, then a subagent *)
    | Compaction (** [at] or [model], then [off] or a model *)
    | Naming (** [model], then [off] or a model *)
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

(** The commands the user has: [superuser]'s include those only superusers
    have ([/nix-gc]), which to everyone else do not exist. *)
val all : superuser:bool -> Spec.t list

val find : superuser:bool -> string -> Spec.t option

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

val closest : superuser:bool -> string -> Spec.t option
val help : superuser:bool -> Content.t

(** The prefixes a prompt can start with ([!command], [@path], ...). *)
val input_help : Content.t
