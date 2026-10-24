open! Core
open! Async_kernel

(** A browser WebSocket as a [Transport.t]: one text message per line. Fails
    when the socket cannot be opened. *)
val connect : url:string -> Prigh_client.Transport.t Deferred.Or_error.t
