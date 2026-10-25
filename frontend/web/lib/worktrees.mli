open! Core
open! Import

(** The worktrees of the repository the session works in ([list_worktrees]),
    for the sidebar's section, [/worktree]'s completion and the dialogs:
    listed once per place (the session, its tool host and directory), again
    when [stale]. Another directory of the same session keeps the listing on
    show until the new one comes; another session starts with none. *)

type t [@@deriving sexp_of]

val empty : t

(** Another session or user: nothing shows; replies to earlier requests are
    ignored. *)
val reset : t -> t

(** The session, its tool host and directory; [None] before the state is
    known. *)
val place : State.t option -> string option

(** Neither listed nor asked for where [state] is (or [stale]): the place to
    list. *)
val wanted : t -> State.t option -> string option

(** Asked for at [place] (of [state]); the request's number, which its
    reply passes to [loaded]. *)
val requested : t -> State.t -> t * int

(** A listing of [place]: taken when it is the place shown and, with [seq],
    from the newest request. *)
val loaded : t -> place:string -> ?seq:int -> Worktree.Listing.t -> t

(** The newest request failed: not a repository, say. Nothing shows. *)
val failed : t -> seq:int -> t

(** To be listed again: something changed (created, removed, a turn ended),
    or the user asked. [quiet]: a check in the background, which [loading]
    does not show. *)
val stale : ?quiet:bool -> t -> t

(** Sessions moved (or came or went): [stale] when a listing shows and none
    is on its way. *)
val sessions_moved : t -> t

(** What shows. *)
val listing : t -> Worktree.Listing.t option

(** A request is on its way, not [quiet]. *)
val loading : t -> bool

(** The worktree being opened in this session, from the sidebar or a
    command, until the backend has moved it. *)
val entering : t -> string option

val set_entering : t -> string option -> t

(** By path, in what shows. *)
val find : t -> string -> Worktree.t option

(** The confirmation's button that removes [w] whatever it holds:
    uncommitted changes, a lock. *)
val force_label : Worktree.t -> string
