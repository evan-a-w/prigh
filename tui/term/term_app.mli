open! Core
open! Async

(** Runs the TUI against a spawned backend until the user quits. *)
val run : backend:string -> args:string list -> unit Deferred.Or_error.t
