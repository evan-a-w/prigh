open! Core
open! Async

(** A byte stream to a server: a TCP connection, or {!Tls_channel} over
    one. *)
type t =
  { reader : Reader.t
  ; write : string -> unit (** does nothing once closed *)
  ; close : unit -> unit (** after flushing what was written *)
  }

(** With TCP keepalive on. *)
val tcp : host:string -> port:int -> t Deferred.Or_error.t

val of_reader_writer : Reader.t -> Writer.t -> t
