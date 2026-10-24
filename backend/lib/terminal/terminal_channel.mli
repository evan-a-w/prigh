open! Core
open! Import

(** A bidirectional stream of terminal frames: a browser's WebSocket, or the
    same frames relayed through the RPC connection of a tool host. *)

module Frame : sig
  type t =
    [ `Binary of string
    | `Text of string
    ]
  [@@deriving sexp_of, equal]

  (** [kind] ("binary" or "text") and [data] (base64 for binary). *)
  val to_fields : t -> (string * Json.t) list

  (** Reads [kind] and [data] from a JSON object. *)
  val of_json : Json.t -> t Or_error.t
end

type t

val of_websocket : Websocket.t -> t

(** Sends are dropped once the channel is closed. *)
val send : t -> Frame.t -> unit

val send_text : t -> string -> unit

(** [None] once the peer has gone. *)
val read : t -> Frame.t option

val is_closed : t -> bool

(** A channel whose incoming frames are pushed by the caller. *)
module Fed : sig
  type channel := t
  type t

  val create : send:(Frame.t -> unit) -> t
  val channel : t -> channel
  val push : t -> Frame.t -> unit

  (** Ends the input: reads return [None] once the pushed frames are
      consumed, and sends are dropped. *)
  val close : t -> unit
end
