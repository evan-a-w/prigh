open! Core
open! Async

(** A bidirectional line channel to the backend. Stdio to a spawned process
    today; a websocket later. *)
type t =
  { send_line : string -> unit
  ; lines : string Pipe.Reader.t
  ; stderr_lines : string Pipe.Reader.t
  ; close : unit -> unit
  ; closed : unit Deferred.t
  }

(** In-memory pair for tests: what the client sends appears on [server_lines] of
    the returned [Backend.t]; what [Backend.send] emits appears on [lines]. *)
module In_memory : sig
  module Backend : sig
    type t

    val requests : t -> string Pipe.Reader.t
    val send : t -> string -> unit
    val close : t -> unit
  end

  val create : unit -> t * Backend.t
end
