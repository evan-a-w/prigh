open! Core
open! Import

(** What the frontends' session lists show of where a session works: its
    repository (an attribute of the session, not a folder of sessions) and
    the worktrees that matter to it ([Session_summary.worktrees]), shown
    under it. A worktree two sessions use is under both. *)

module Node : sig
  type t =
    { worktree : P.Session_worktree.t
    ; listed : P.Worktree.t option
      (** as its repository's listing has it: changes, upstream, lock *)
    ; here : bool (** the session's cwd is in it *)
    }
  [@@deriving sexp_of, equal]

  val label : t -> string
  val agent : t -> bool
end

(** [prigh] for [~/src/prigh]. *)
val repo_name : P.Repo.t -> string

(** [prigh ⎇ main]: its repository and the branch of its cwd's checkout;
    [None] outside a repository (or unknown). *)
val place : P.Session_summary.t -> string option

(** [from main]: where a worktree switch took it from ({!P.Session_home}),
    while it is set. *)
val home : ?tilde:(string -> string) -> P.Session_home.t option -> string option

(** Its worktrees, by when they first mattered. [listings]: repositories'
    worktrees as [list_worktrees] gives them; one its repository's listing
    does not have (removed outside prigh) is left out. [cwd]: where the
    session is now, when known better than its summary (the current one's
    state). *)
val of_session
  :  ?listings:P.Worktree.Listing.t list
  -> ?cwd:string
  -> P.Session_summary.t
  -> Node.t list

(** The repositories whose listings [of_session] would use for [sessions]:
    those with worktrees under a session. *)
val roots : P.Session_summary.t list -> string list

(** One line for a picker's detail: [⎇ feat/x · ⎇ agent/fix (subagent)]. *)
val summary : Node.t list -> string
