open! Core
open! Async
module P = Prigh_protocol

(** Relative paths under the cwd matching [prefix] (case-insensitive substring),
    directories with a trailing slash, at most 200. Uses [fd] when available and
    falls back to a bounded recursive [readdir]. *)
val list : prefix:string -> P.Json.t Deferred.t
