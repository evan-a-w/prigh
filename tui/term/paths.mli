open! Core
open! Async
module P = Prigh_protocol

(** Paths under [cwd] (the process cwd when [None]) matching [prefix]
    (case-insensitive substring), relative to [cwd], directories with a trailing
    slash, at most 200. Uses [fd] when available and falls back to a bounded
    recursive [readdir]. Version-control and build directories are skipped. *)
val list : cwd:string option -> prefix:string -> P.Json.t Deferred.t
