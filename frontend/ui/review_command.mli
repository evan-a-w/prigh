open! Core
open! Import

(** [/review]'s arguments, shared by the terminal and browser frontends.
    [/review [WORKTREE|BRANCH] [--base REV] [--direct]] reviews a worktree
    of the repository (the review page in prigh-web, the files' approvals in
    the terminal): the session's own without one, else the one a path,
    branch or directory name names (as [/worktree] lists them), or a branch
    that is not checked out (read-only, by its commits), against [REV]
    (default: its base, see [git_diff]): since their merge base, or the
    base itself with [--direct]. Anything else ([/review main focus on
    errors]) has the model review the changes, as before. A single word
    without flags that is no worktree is the model's base: a branch to
    review needs [--base] or [--direct]. *)

module Action : sig
  type t =
    | Show of
        { target : P.Git_diff.Target.t
        ; base : string option
        ; direct : bool
        } (** review it now *)
    | Named of
        { name : string
        ; base : string option
        ; direct : bool
        ; model : string option
          (** when no worktree is called [name]: the model's review of
              these arguments; [None]: an error naming the closest *)
        } (** a worktree by its branch or name: look it up first *)
    | Model of string (** the model reviews the changes; the arguments *)
    | Error of string
  [@@deriving sexp_of, equal]
end

val parse : string -> Action.t

(** What a [Named] comes to with the repository's worktrees: [Show] its
    path, the model's review, or [Show] the branch of that name, not checked
    out (the backend says when there is none, with the closest). *)
val resolve
  :  P.Worktree.Listing.t
  -> name:string
  -> base:string option
  -> direct:bool
  -> model:string option
  -> Action.t

(** The command that reviews [target] (default: the session's worktree)
    against [base]: the slash form of the review action, for pickers and
    menus ([/review PATH --base REV --direct]). *)
val command
  :  ?target:P.Git_diff.Target.t
  -> ?base:string
  -> ?direct:bool
  -> unit
  -> string

(** What [rest] (all after [/review ]) completes, like
    {!Worktree_command.complete}: the worktrees' targets, the branches not
    checked out, [--base] and [--direct]; then [HEAD], the branches, remote
    branches and tags after [--base]. *)
val complete
  :  worktrees:P.Worktree.t list
  -> refs:P.Git_diff.Refs.t
  -> string
  -> (int * (string * string) list) option
