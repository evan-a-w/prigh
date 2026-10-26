open! Core

(** A git worktree that matters to a session ([list_sessions]'
    [worktrees]): one it made, moved into, checked a pull request out in or
    gave a subagent, until it is removed. On disk worktrees go by
    repository; the session only keeps track of its own. *)

module Source : sig
  (** How it first came to matter. *)
  type t =
    | Created (** [/worktree new], New worktree *)
    | Entered (** the session's cwd moved into it *)
    | Pull_request of int (** [/pr checkout N] *)
    | Agent of
        { agent : string (** the subagent's id in its session *)
        ; task : string
        } (** the [subagent] tool's [worktree] *)
  [@@deriving sexp_of, equal]
end

type t =
  { path : string
  ; branch : string option
  ; root : string (** the repository's main checkout *)
  ; source : Source.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** The branch, else the directory's name. *)
val label : t -> string

(** Whether [dir] is [path] or inside it. *)
val contains : t -> dir:string -> bool
