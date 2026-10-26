open! Core
open! Import

(** [/commit]'s arguments, shared by the terminal and browser frontends:
    [/commit [-a] [--amend] -m MESSAGE] commits what is staged in the
    session's worktree ([-a]: every change to tracked files first, like
    [git commit -a]; [-am MESSAGE] for both), [/commit [-a] [--amend]
    --generate] has the model draft the message from the staged changes
    instead, and [/commit --amend] alone amends keeping the message.

    The message is everything after [-m], verbatim (new lines too), or a
    quoted string: in double quotes, with backslash escaping a double quote
    or a backslash, or in single quotes, as typed. *)

module Message : sig
  type t =
    | Text of string (** empty: [--amend] keeps the commit's message *)
    | Generate
  [@@deriving sexp_of, equal]
end

type t =
  { stage_tracked : bool (** [-a] *)
  ; amend : bool
  ; message : Message.t
  }
[@@deriving sexp_of, equal]

val usage : string

(** [rest] is all that follows [/commit]; errors say what to type. *)
val parse : string -> (t, string) Result.t

(** [message] as a double-quoted string that {!parse} reads back. *)
val quote : string -> string

(** The [/commit -m] (with [--amend]) that commits [message], quoted. *)
val command : amend:bool -> string -> string

(** What [rest] (all after [/commit ]) completes, like
    {!Remind_command.complete}: the flags, until the message starts. *)
val complete : string -> (int * (string * string) list) option

(** [committed 1a2b3c4 Fix the parser on main], for the transcript. *)
val committed : amend:bool -> P.Git_status.Commit.t -> string

(** [pushed main to origin]. *)
val pushed : P.Git_status.Push.t -> string
