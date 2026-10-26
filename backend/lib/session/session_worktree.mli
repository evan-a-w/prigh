open! Core
open! Import

(** A git worktree that matters to a session ({!Session.worktrees}): one it
    made, moved into, checked a pull request out in, or gave a subagent.
    Worktrees live by repository on disk ([<worktrees_dir>/<project>/...]);
    sessions only keep track of theirs, until they are removed. *)

module Source : sig
  (** How it came to matter to the session: the first way only. *)
  type t =
    | Created (** [/worktree new], the New worktree dialog *)
    | Entered (** the session's cwd moved into it *)
    | Pull_request of { number : int } (** [/pr checkout] *)
    | Agent of
        { agent : string (** the subagent's id in the session *)
        ; task : string
        } (** made for a subagent ({!Agent_worktree}) *)
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { path : string (** its top directory *)
  ; branch : string option (** [None]: detached, or unknown *)
  ; root : string (** the repository's main checkout ({!Git_location}) *)
  ; source : Source.t
  }
[@@deriving sexp, jsonaf, equal]

val of_agent : Agent_worktree.t -> t

(** Where the cwd is: a worktree other than the main checkout. *)
val of_location : Git_location.t -> t option

(** Whether [dir] is [path] or inside it. *)
val contains : path:string -> dir:string -> bool
