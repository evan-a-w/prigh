open! Core
open! Import

(** A minimal RFC 6455 implementation: enough for the browser frontend to
    exchange the JSON-lines protocol as text messages and terminal bytes as
    binary ones. *)

(** [Sec-WebSocket-Accept] for a [Sec-WebSocket-Key]. *)
val accept_key : string -> string

module Opcode : sig
  type t =
    | Continuation
    | Text
    | Binary
    | Close
    | Ping
    | Pong
  [@@deriving sexp_of, equal]
end

module Frame : sig
  type t =
    { fin : bool
    ; opcode : Opcode.t
    ; payload : string
    }
  [@@deriving sexp_of, equal]
end

(** Clients must mask ([mask] is the 4-byte key); servers must not. *)
val encode : ?mask:string -> Frame.t -> string

exception Protocol_error of string

(** Raises [End_of_file] at end of input and [Protocol_error] on malformed
    frames or ones above 64 MiB. *)
val read_frame : Eio.Buf_read.t -> Frame.t

module Message : sig
  type t =
    | Text of string
    | Binary of string
    | Close of int option
    | Ping of string
    | Pong of string
  [@@deriving sexp_of, equal]
end

(** Reassembles fragments and returns control messages as they come. *)
module Message_reader : sig
  type t

  val create : Eio.Buf_read.t -> t

  (** Raises [End_of_file] and [Protocol_error] like [read_frame]. *)
  val next : t -> Message.t
end

(** An open connection (after the HTTP handshake). *)
type t

val create
  :  ?role:[ `Server | `Client ]
  -> reader:Eio.Buf_read.t
  -> flow:_ Eio.Flow.sink
  -> unit
  -> t

(** The next data message; answers pings. [None] once the peer closed, the
    connection dropped or a protocol error was answered with a close frame. *)
val read : t -> [ `Text of string | `Binary of string ] option

(** [read], skipping binary messages. *)
val read_text : t -> string option

(** The client's side of the opening handshake over [flow] (a [GET] of
    [target] from [host]), then [create ~role:`Client]. [Error] says what the
    server answered instead of switching protocols. Raises what reading and
    writing [flow] raise. *)
val connect
  :  reader:Eio.Buf_read.t
  -> flow:_ Eio.Flow.sink
  -> host:string
  -> target:string
  -> (t, string) Result.t

val send_text : t -> string -> unit
val send_binary : t -> string -> unit
val close : ?code:int -> t -> unit
val is_closed : t -> bool

(** Watches over a connection until the peer looks gone, then marks it
    closed, calls [shutdown] (which should shut the socket down, so that a
    read or a write stuck on it ends) and returns. Pings the peer every
    [interval] seconds while nothing is being written (browsers answer pings
    by themselves, also from a suspended page); the peer is gone when
    nothing came from it for [timeout] seconds since the last write ended,
    or when one write has been stuck for [write_timeout] seconds (it is not
    reading). Defaults: 15, 45, 300. Once the connection is closed
    otherwise, waits to be cancelled. *)
val keepalive
  :  t
  -> clock:_ Eio.Time.clock
  -> shutdown:(unit -> unit)
  -> ?interval:float
  -> ?timeout:float
  -> ?write_timeout:float
  -> unit
  -> unit
