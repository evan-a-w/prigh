open! Core
open! Async_kernel

(** A browser WebSocket as a [Transport.t]: one text message per line. Fails
    when the socket cannot be opened (or does not open within [timeout],
    closing the connecting socket). *)
val connect
  :  ?timeout:Time_ns.Span.t
  -> url:string
  -> unit
  -> Prigh_client.Transport.t Deferred.Or_error.t
