open! Core
open! Import

(** A worktree's changes on a tool host, for [/diff] and [/review]: [git
    diff] against [HEAD] (or, with a base, against its merge base with
    [HEAD]: what was committed since as well; with [direct], against the
    base itself) plus the untracked files as new files, optionally limited
    to one path. The worktree is the one of the session's directory, or
    [dir] (any worktree of any repository on the host). With [default_base],
    a linked worktree (not the repository's main checkout) is compared with
    its base when none is given: the branch it was made from as
    {!Worktree.create} recorded it, else its remote's default branch
    ([origin/HEAD]), else [main] or [master].

    With [rev], a branch (or any commit) of that repository is reviewed
    without checking it out: [git diff base...rev] ([base rev] when
    [direct]), its base found as a linked worktree's is; it has no
    uncommitted or untracked changes, so [HEAD] is no base for it.

    Runs on the host as the pseudo-tool [op]; [show_op] reads a file of a
    commit for a review of a [rev]. *)

module Commit : sig
  type t =
    { sha : string
    ; subject : string (** its message's first line *)
    }
  [@@deriving sexp, jsonaf, equal]
end

module Result : sig
  type t =
    { root : string (** the repository's top directory *)
    ; against : string
      (** what the working tree is compared with: [HEAD], a merge base's
        commit, or [empty tree] before the first commit *)
    ; diff : string (** paths relative to [root] *)
    ; truncated : bool (** cut at [max_bytes] *)
    ; branch : string option (** checked out; [None]: detached *)
    ; base : string
      (** what [against] was made from: [HEAD], or the base's name *)
    ; main : bool (** the repository's main checkout, not a linked worktree *)
    ; branches : string list
      (** the repository's local branches, to pick another base from *)
    ; remote_branches : string list (** [origin/main], ... *)
    ; tags : string list (** newest first *)
    ; commits : Commit.t list
      (** the last few on what is reviewed ([HEAD], or [rev]), newest
          first *)
    ; rev : string option
      (** the branch reviewed without a checkout; [root] is then the
          repository's worktree it was read in *)
    ; direct : bool (** [against] is the base itself, not a merge base *)
    }
  [@@deriving sexp, jsonaf, equal]

  (** Files changed, lines added and removed. *)
  val stats : t -> int * int * int
end

val op : string
val max_bytes : int

(** The pseudo-tool's arguments. *)
val args
  :  ?path:string
  -> ?base:string
  -> ?dir:string
  -> ?rev:string
  -> ?direct:bool
  -> ?default_base:bool
  -> unit
  -> Jsonaf.t

(** On the host: [Ok] carries the [Result.t] as JSON; errors say what to do
    (not a repository, git missing, an unknown base or [rev], with the
    closest names). *)
val run
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Jsonaf.t
  -> Tool_result.t

(** {2 A file of a commit} *)

module Shown : sig
  type t =
    { text : string (** at most 256 KiB, cut at a line *)
    ; truncated : bool
    }
  [@@deriving sexp, jsonaf, equal]
end

val show_op : string

(** [show_op]'s arguments: [path] (relative to the repository's top) as it
    is in [rev], in the repository of [dir] (default: the session's). *)
val show_args : ?dir:string -> rev:string -> path:string -> unit -> Jsonaf.t

(** On the host: [Ok] carries a [Shown.t] as JSON; not a text file, no such
    file or an unknown [rev] are errors. *)
val show
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Jsonaf.t
  -> Tool_result.t

val shown_of_tool_result : Tool_result.t -> Shown.t Or_error.t

(** {2 The repository's names, for completion} *)

module Refs : sig
  type t =
    { branches : string list (** local *)
    ; remote_branches : string list
    ; tags : string list (** newest first *)
    }
  [@@deriving sexp, jsonaf, equal]
end

val refs_op : string
val refs_args : ?dir:string -> unit -> Jsonaf.t

(** On the host: [Ok] carries the [Refs.t] of [dir]'s repository as JSON. *)
val list_refs
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> arguments:Jsonaf.t
  -> Tool_result.t

val refs_of_tool_result : Tool_result.t -> Refs.t Or_error.t

(** Whether an error of [run] says the base is not a revision. *)
val is_unknown_revision : string -> bool

val of_tool_result : Tool_result.t -> Result.t Or_error.t
