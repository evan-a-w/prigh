open! Core
open! Import

(** Every file under a directory, for the editor's Go to file: in a git
    repository the tracked and untracked-but-not-ignored ones ([git ls-files],
    less those deleted from the work tree), elsewhere a walk that skips dot
    directories, [_build], [_opam] and [node_modules] and follows no symbolic
    links. Paths are relative to the directory, sorted; at most [limit]
    (a million by default). *)

type t =
  { files : string list
  ; truncated : bool (** stopped at [limit] files *)
  }
[@@deriving sexp_of]

val limit : int
val list : ?limit:int -> env:Env.t -> root:string -> unit -> t

(** The walk alone, as used outside a git repository. *)
val walk : ?limit:int -> root:string -> unit -> t

(** [{"root": ROOT, "files": "a\nb", "truncated": false}]: one string, not
    an array, as it can hold a few hundred thousand paths. *)
val to_json : root:string -> t -> Json.t
