open! Core
open! Import

(** Sessions as a tree of projects, for the frontends' session lists. A
    project is a git repository, named by its main checkout ([repo.root] of
    its sessions), or a plain folder (the cwd of sessions outside any
    repository, or whose repository is unknown). Sessions in the main
    checkout sit directly under their project; each other worktree of the
    repository is a node under it, with its sessions: the worktrees the
    sessions work in, those made for subagents
    ([Session_summary.agent_worktrees]) and, for the repository of a
    [list_worktrees] listing, all of its worktrees.

    Projects come in the order of their first session in the list given
    (the backend's: the most recent first), worktrees likewise, each
    session in its list's order. *)

module Worktree_node : sig
  type t =
    { path : string
    ; branch : string option
    ; sessions : P.Session_summary.t list
    ; agent : (P.Agent_worktree.t * P.Session_summary.t) option
      (** made for a subagent of that session *)
    ; listed : P.Worktree.t option
      (** as the listing has it: changes, upstream, lock *)
    }
  [@@deriving sexp_of, equal]

  (** The branch, else the directory's name. *)
  val label : t -> string
end

module Project : sig
  type t =
    { root : string (** the main checkout, or the folder *)
    ; git : bool
    ; branch : string option (** the main checkout's *)
    ; sessions : P.Session_summary.t list (** in the main checkout *)
    ; worktrees : Worktree_node.t list
    ; listed : P.Worktree.t option (** the main checkout, as listed *)
    }
  [@@deriving sexp_of, equal]

  (** The directory's name: [~/src/prigh] is [prigh]. *)
  val name : t -> string

  (** Every session under it, the main checkout's first. *)
  val all_sessions : t -> P.Session_summary.t list
end

type t = Project.t list [@@deriving sexp_of, equal]

(** The project (its [root]) and the worktree (its path, [None] for the main
    checkout or a folder) a session works in. *)
val place : P.Session_summary.t -> string * string option

(** [owners]: sessions not shown (archived ones) whose subagents' worktrees
    still count. [listing]: the worktrees of the current session's
    repository; its project shows even with no session. *)
val build
  :  ?listing:P.Worktree.Listing.t
  -> ?owners:P.Session_summary.t list
  -> P.Session_summary.t list
  -> t

(** The sessions in the order the tree shows them, leaving out those under a
    [collapsed] project or worktree (by [root] or path). *)
val sessions : ?collapsed:String.Set.t -> t -> P.Session_summary.t list

(** The project whose root is [root], and the worktree at [path] in it. *)
val find_worktree : t -> string -> (Project.t * Worktree_node.t) option

val find_project : t -> string -> Project.t option

(** What the user calls the place: [prigh], [prigh ⎇ feat/x]. *)
val place_label : t -> P.Session_summary.t -> string
