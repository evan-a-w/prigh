open! Core

(** Incremental parser for text/event-stream. Feed bytes as they arrive; each
    complete event (terminated by a blank line) is returned. Comment lines and
    unknown fields are ignored. [data] is the newline-joined data lines. *)

module Event : sig
  type t =
    { event : string option
    ; data : string
    ; id : string option
    }
  [@@deriving sexp_of]
end

type t

val create : unit -> t
val feed : t -> string -> Event.t list

(** Flush a trailing event that was not terminated by a blank line. *)
val finish : t -> Event.t option
