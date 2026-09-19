open! Core
open! Import

(** Current git branch for [cwd], found by walking up to the nearest [.git]
    directory and reading its [HEAD]. A symbolic ref [ref: refs/heads/NAME]
    yields [NAME]; a detached HEAD yields the first 8 characters of the
    commit hash. *)
val find : cwd:string -> string option

val parse_head : string -> string option
