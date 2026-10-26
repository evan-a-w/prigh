open! Core
open! Import

(** The worktrees of the repositories that sessions' worktrees belong to
    ([list_worktrees] with their [cwd]), for what the sidebar shows of them:
    changes, ahead and behind, and which are gone (one its repository no
    longer lists is not shown). Each is asked for once, and again once
    [stale]; those on show stay until their new listing comes. *)

type t [@@deriving sexp_of]

val empty : t

(** Of [roots], those not asked for yet. *)
val wanted : t -> string list -> string list

val requested : t -> string list -> t
val loaded : t -> root:string -> Worktree.Listing.t -> t

(** Not listed (not a repository any more, its host gone): not asked
    again until [stale]. *)
val failed : t -> root:string -> t

val stale : t -> t
val listings : t -> Worktree.Listing.t list
