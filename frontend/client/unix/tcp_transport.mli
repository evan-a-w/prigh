open! Core
open! Async
open Prigh_client

(** Connects to a backend started with [prigh serve -listen HOST:PORT] and talks
    JSON lines over the socket. *)
val connect : host:string -> port:int -> Transport.t Deferred.Or_error.t
