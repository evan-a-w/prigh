open! Core
open! Async
open Prigh_client

(** Spawns [prog args] and talks JSON lines over its stdin/stdout. *)
val spawn
  :  ?env:Process.env
  -> prog:string
  -> args:string list
  -> unit
  -> Transport.t Deferred.Or_error.t
