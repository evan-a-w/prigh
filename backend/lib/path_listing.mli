open! Core
open! Import

(** Paths under [root] matching [prefix] (case-insensitive substring), relative
    to [root], directories with a trailing slash, at most 200. Uses [fd] when
    available and falls back to a bounded recursive [readdir]. Version-control
    and build directories are skipped. *)
val list : env:Env.t -> root:string -> prefix:string -> string list

(** The fallback used when [fd] is unavailable. *)
val readdir : root:string -> prefix:string -> string list
