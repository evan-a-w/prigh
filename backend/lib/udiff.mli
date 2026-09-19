open! Core
open! Import

(** Unified diff of two texts, split into lines. *)

(** Returns the unified diff, or the empty string if [before] and [after] have
    the same lines. The result starts with one [--- a/path] / [+++ b/path]
    header pair followed by any number of [@@] hunks with three lines of
    context. [path] is used verbatim in the header, so callers usually pass a
    path relative to the working directory. *)
val hunks : path:string -> before:string -> after:string -> string
