open! Core
open! Import

(** A repository's git worktrees on a tool host, and where prigh puts the ones
    it creates: [<dir>/<project>/<branch-slug>], where [dir] is the config's
    [worktrees_dir] (default [~/.prigh/worktrees], on the tool host),
    [project] the main worktree's directory name and [branch-slug] the branch
    with [/] and anything unusual turned into [-]. A project directory
    records the repository it belongs to in a [.prigh-repo] file (its git
    common directory), so two repositories with the same name get
    [<project>] and [<project>-2]; a slug already taken gets [-2], [-3], ...

    Every operation runs git on the host, in the repository of [cwd] (any of
    its worktrees). The pseudo-tools [list_op], [create_op] and [remove_op]
    run them on a tool host ({!run}); {!Agent} calls them on the active
    one. *)

type t =
  { path : string
  ; branch : string option (** [None]: detached HEAD *)
  ; head : string option (** the commit; [None] before the first one *)
  ; main : bool (** the repository's main worktree, never removed *)
  ; managed : bool (** under the project directory, i.e. made by prigh *)
  ; locked : bool
  ; prunable : bool (** its directory is gone *)
  ; changes : int option
    (** files with uncommitted changes (untracked ones included); [None]
        when unknown (prunable) *)
  ; upstream : string option (** the branch's upstream, e.g. [origin/main] *)
  ; ahead : int (** commits not in [upstream] *)
  ; behind : int (** commits of [upstream] not in the branch *)
  }
[@@deriving sexp, jsonaf, equal]

(** Whether [dir] is [t]'s directory or inside it. *)
val contains : t -> dir:string -> bool

type worktree := t

module Listing : sig
  type t =
    { root : string (** the main worktree *)
    ; project_dir : string (** where new worktrees go *)
    ; worktrees : worktree list (** the main one first *)
    }
  [@@deriving sexp, jsonaf, equal]
end

module Created : sig
  type t =
    { worktree : worktree
    ; new_branch : bool (** the branch was created, from the base *)
    ; root : string (** the main worktree *)
    ; cwd : string
      (** the [cwd] it was made from, carried over: the same subdirectory of
          the new worktree when it has it, else its top *)
    }
  [@@deriving sexp, jsonaf, equal]
end

(** [branch] made fit for a directory name: [feat/x] is [feat-x]. *)
val slug : string -> string

(** The [~] of [dir] is the host's home. *)
val list
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> dir:string
  -> Listing.t Or_error.t

(** Checks [branch] out in a new worktree at the standard location: an
    existing local branch as it is (no [base] then); otherwise a new branch
    from [base] (default: [HEAD] of [cwd]), or, without a base, tracking the
    one remote branch of that name if there is exactly one. Fails when the
    branch is checked out in another worktree already. With [fresh], a
    branch that exists or is checked out is not reused: [branch-2],
    [branch-3], ... is made instead. *)
val create
  :  ?fresh:bool
  -> env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> dir:string
  -> branch:string
  -> base:string option
  -> unit
  -> Created.t Or_error.t

(** Removes the worktree at [path] (relative to [cwd] or absolute) and
    returns it as it was; its branch stays. Refuses the main worktree, and,
    unless [force], one with uncommitted changes or locked. A worktree whose
    directory is gone is pruned. *)
val remove
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> dir:string
  -> path:string
  -> force:bool
  -> t Or_error.t

(** {2 On a tool host} *)

val list_op : string
val create_op : string
val remove_op : string
val is_op : string -> bool
val list_args : dir:string -> Jsonaf.t

val create_args
  :  ?fresh:bool
  -> dir:string
  -> branch:string
  -> base:string option
  -> unit
  -> Jsonaf.t

val remove_args : dir:string -> path:string -> force:bool -> Jsonaf.t

(** Runs one of the ops; [Ok] carries the result as JSON. *)
val run
  :  env:Env.t
  -> cancel:Cancellation.t
  -> cwd:string
  -> name:string
  -> arguments:Jsonaf.t
  -> Tool_result.t

(** [create_op] as an [on_host] tool, for [Tool.execute_via]: the
    subagent tool makes worktrees with it on the session's tool host. *)
val create_tool : Tool.t

val listing_of_result : Tool_result.t -> Listing.t Or_error.t
val created_of_result : Tool_result.t -> Created.t Or_error.t
val removed_of_result : Tool_result.t -> t Or_error.t
