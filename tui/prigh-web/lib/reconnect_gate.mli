open! Core

(** Which of the page's reconnect attempts may still go ahead. An attempt is
    scheduled with its generation and a delay; by the time it is due, a newer
    one may have started (the network came back, the user said retry now) and
    perhaps connected and moved on to another session: the old one must not
    connect again, or its [hello] would take the backend back to the session
    it was scheduled for. *)

type t [@@deriving sexp_of]

val create : unit -> t

(** An attempt of [generation] is scheduled. *)
val schedule : t -> generation:int -> unit

(** An attempt of [generation] is due: whether it goes ahead, i.e. no newer
    one was scheduled since. It goes ahead also when the socket is open: an
    older attempt may have opened it and had its [hello] answered too late
    to count, and only a [hello] of this generation brings the page back. *)
val due : t -> generation:int -> bool
