open! Core
open! Import

(** What moving a session's cwd does to its home ({!Session.home}): a
    worktree switch into another worktree records where it came from, unless
    it already has a home (switching on from worktree A to B keeps the
    first); switching into the home's worktree, or a [/cd] to the home or
    out of the repository's worktrees, forgets it. *)

module Place : sig
  (** A cwd, where it is. *)
  type t =
    { cwd : string
    ; host : Session.Host.t option
    ; repo : Git_location.t option option
      (** [None]: unknown (a host that cannot tell); [Some None]: not in a
          repository *)
    }
  [@@deriving sexp_of]

  val home : t -> Session.Home.t
end

module Change : sig
  type t =
    | Keep
    | Set of Session.Home.t
    | Clear
  [@@deriving sexp_of, equal]
end

(** A worktree switch ([/worktree open], [/worktree new], [/pr checkout],
    the sidebar's Switch session here) from [from] to [to_]. *)
val switch : Session.Home.t option -> from:Place.t -> to_:Place.t -> Change.t

(** Any other change of directory ([/cd], [set_cwd], [/host]) from [from]:
    within its checkout, or into another worktree of the repository, keeps
    the home. *)
val cd : Session.Home.t option -> from:Place.t -> to_:Place.t -> Change.t
