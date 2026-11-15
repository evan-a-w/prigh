open! Core

(** A [message_update]: a piece of a streamed assistant message. The backend
    numbers the pieces of each stream from 1 ([seq]); one update may carry
    several, merged ([from] to [seq]). It sends the whole message so far
    ([partial]) instead of expecting the client to have the pieces before:
    when the client did not follow the stream from its start, or asked for
    it with [resync]. *)

type t =
  { stream : int (** the streamed message's id *)
  ; from : int (** the first piece this update carries *)
  ; seq : int (** the last piece this update carries *)
  ; block : int (** the index in the content of the block [delta] changes *)
  ; delta : Delta.t
  ; partial : Message.Assistant.t option
    (** the whole message through [seq]: [delta] is in it already *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** Where a client is in a stream: the last piece it has. *)
module Position : sig
  type t =
    { stream : int
    ; seq : int
    }
  [@@deriving sexp_of, equal]
end

module Applied : sig
  type t =
    | Updated of Position.t * Message.Assistant.t
    | Unchanged (** it had this piece already *)
    | Gap
    (** pieces are missing (or the delta does not fit the message): ask the
        backend to [resync] *)
  [@@deriving sexp_of]
end

(** Applies [t] to [message], the stream so far, which is at [at]; an update
    starting a stream ([from] 1) starts from an empty message (keeping
    [message]'s other fields). *)
val apply : t -> at:Position.t option -> Message.Assistant.t -> Applied.t
