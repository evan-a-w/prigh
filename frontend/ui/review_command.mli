open! Core
open! Import

(** [/review]'s arguments, shared by the terminal and browser frontends.
    [/review [WORKTREE] [--base REV]] reviews a worktree of the repository
    (the review page in prigh-web, the files' approvals in the terminal):
    the session's own without one, else the one a path, branch or directory
    name names (as [/worktree] lists them), against [REV] (default: its
    base, see [git_diff]). Anything else ([/review main focus on errors])
    has the model review the changes, as before. *)

module Action : sig
  type t =
    | Show of
        { dir : string option (** a worktree's path: the session's *)
        ; base : string option
        } (** review it now *)
    | Named of
        { name : string
        ; base : string option
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
    path, the model's review, or an error naming the closest. *)
val resolve
  :  P.Worktree.Listing.t
  -> name:string
  -> base:string option
  -> model:string option
  -> Action.t

(** The command that reviews the worktree at [dir] (default: the session's)
    against [base]: the slash form of the review action, for pickers and
    menus ([/review PATH --base REV]). *)
val command : ?dir:string -> ?base:string -> unit -> string

(** What [rest] (all after [/review ]) completes, like
    {!Worktree_command.complete}: the worktrees' targets and [--base], then
    [branches] after [--base]. *)
val complete
  :  worktrees:P.Worktree.t list
  -> branches:string list
  -> string
  -> (int * (string * string) list) option
