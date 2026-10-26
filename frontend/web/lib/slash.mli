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
    | Theme (** [original], [dark] or [light] *)
    | Accent (** the presets' names, or a colour *)
    | Confirm
    | Mode
    | Plan
    | Notify (** [on] or [off] *)
    | Session (** saved sessions' paths *)
    | Path (** [list_paths] *)
    | Host
    | User (** the users a superuser can act as *)
    | Skill (** right after [/skill:], from [list_skills] *)
    | Budget (** [off], or dollars *)
    | Goal (** [off], [resume], or the goal *)
    | Resume (** [now] or [cancel] *)
    | Snippet (** a snippet's name, [add] or [rm] *)
    | Remind (** a [/remind] subcommand, or a reminder's name after [rm] *)
    | Pr (** [/pr]'s subcommands *)
    | Worktree
    (** a [/worktree] subcommand, then a worktree, flag or base branch *)
    | Review (** [/review]: a worktree or [--base], then a base branch *)
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

(** The commands the user has: [superuser]'s include those only superusers
    have ([/nix-gc]), which to everyone else do not exist. *)
val all : superuser:bool -> Spec.t list

val find : superuser:bool -> string -> Spec.t option

(** The command with the nearest name (within two edits, or a prefix). *)
val closest : superuser:bool -> string -> Spec.t option

module Parsed : sig
  type t =
    { name : string
    ; rest : string (** the arguments, stripped *)
    }
  [@@deriving sexp_of, equal]
end

(** [None] unless [text] is one line starting with [/] (so pasting a path
    like [/etc/hosts] with a question still sends a prompt), or
    [/snippet add], [/remind stop] and [/remind every], whose text may span
    lines. *)
val parse : string -> Parsed.t option
