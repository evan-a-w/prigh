open! Core
open! Import

(** The review page's two pickers, anchored menus over its "Reviewing …
    against …" line: what to review (the repository's worktrees, then its
    branches, local and remote, that are not checked out: those are
    reviewed read-only), and what to compare it with ([HEAD]: the
    uncommitted changes only; the branches, remote branches, tags and the
    last commits on what is reviewed; or any revision typed, which the
    backend checks). Grouped while nothing is typed, best match first
    otherwise. *)

module Which : sig
  type t =
    | Target (** what is reviewed *)
    | Base (** what it is compared with *)
  [@@deriving sexp_of, equal]
end

module Choice : sig
  type t =
    | Review of Git_diff.Target.t
    | Base of string
  [@@deriving sexp_of, equal]
end

module Item : sig
  type t =
    { choice : Choice.t
    ; group : string (** the heading it is under *)
    ; label : string
    ; detail : string
    ; marked : bool (** what the page shows now *)
    }
  [@@deriving sexp_of, equal]

  (** For the view's elements and clicks: unique in a picker. *)
  val id : t -> string
end

type t [@@deriving sexp_of, equal]

val which : t -> Which.t
val query : t -> string

(** The worktrees a [targets] picker was made from. *)
val worktrees : t -> Worktree.t list

(** What the page reviews, of the repository's [worktrees] (prunable ones
    left out) and the branches [diff] lists; [reviewed] is marked. *)
val targets
  :  worktrees:Worktree.t list
  -> reviewed:Git_diff.Target.t
  -> Git_diff.t
  -> t

(** What to compare [diff]'s worktree or branch with; its base is marked. *)
val bases : Git_diff.t -> t

(** The items matching the query, best first (grouped, in order, when it is
    empty), and for [Base] a last one comparing with the query as typed
    unless an item is called that. *)
val visible : t -> Item.t list

val selected : t -> int
val selected_item : t -> Item.t option
val set_query : t -> string -> t

(** Moves the highlight by [delta], clamped. *)
val move : t -> int -> t

val find : t -> id:string -> Item.t option

(** [fresh]'s choices (the diff was read again) with [t]'s query and
    highlight. *)
val refresh : t -> fresh:t -> t
