open! Core
open! Import

(** Where a directory is in git, found by reading files only (no git
    process), so that it is cheap enough to do on every change of directory:
    the repository it belongs to (its main checkout, which names the
    project), the worktree it is in and that worktree's branch. *)

type t =
  { root : string
    (** the repository's main checkout (for a bare repository, its git
        directory): the same for all of its worktrees *)
  ; worktree : string (** the top of the checkout the directory is in *)
  ; branch : string option
    (** [NAME] for [refs/heads/NAME]; a detached HEAD is the first 8
        characters of the commit; [None] when unreadable *)
  ; main : bool (** [worktree] is the main checkout *)
  }
[@@deriving sexp, jsonaf, equal]

(** Walks up from [cwd] to the nearest [.git]: a directory (a main checkout)
    or a file pointing at a linked worktree's git directory, whose
    [commondir] leads to the repository's. [None] outside a repository. *)
val find : cwd:string -> t option

(** The branch of a [HEAD] file's contents. *)
val parse_head : string -> string option

(** {2 On a tool host} *)

(** [$git_location {}]: {!find} for the cwd, as JSON ([null] outside a
    repository). *)
val op : string

val run : cwd:string -> Tool_result.t
val of_result : Tool_result.t -> t option Or_error.t
