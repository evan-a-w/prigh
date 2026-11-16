open! Core
open! Import

(** The worktrees of the repositories that sessions' worktrees belong to
    ([list_worktrees] with their [cwd]), by repository root, for what the
    sidebar shows of them: changes, ahead and behind, and which are gone
    (one its repository no longer lists is not shown).

    It belongs to the user, not to the session they are in: switching
    sessions keeps it, so a session's worktrees stay under it when the user
    leaves it. The open session's own listing ({!Worktrees}) goes in too
    ([loaded]), so its repository is up to date when the user leaves it.
    Each repository is asked for once, and again once [stale]; a listing on
    show stays until its new one comes. *)

type t [@@deriving sexp_of]

val empty : t

(** Of [roots], those not asked for yet. *)
val wanted : t -> string list -> string list

val requested : t -> string list -> t

(** A listing of [root], from its own request or the open session's
    listing: not asked for again until [stale]. *)
val loaded : t -> root:string -> Worktree.Listing.t -> t

(** The answer to [requested]: taken unless [root] was not asked for since
    [empty] (a reply for the user we were). *)
val answered : t -> root:string -> Worktree.Listing.t -> t

(** Not listed (not a repository any more, its host gone): not asked
    again until [stale]. *)
val failed : t -> root:string -> t

val stale : t -> t

(** Only the repositories of [roots]: those the sessions listed work in.
    Keeps it bounded by the session list. *)
val keep : t -> roots:String.Set.t -> t

val listings : t -> Worktree.Listing.t list

(** The repositories whose listing came before the last [stale] (its new one
    on its way, or not asked for): a worktree it lacks may be newer than
    it, so it still shows. *)
val unsettled : t -> string list
