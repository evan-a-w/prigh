open! Core

(** Connections lost before they brought the session back. A connection
    that keeps dropping while the session loads (a backend that cannot serve
    it in time, a network too slow to carry it) must not be retried at once
    forever: each such loss waits longer before the next attempt, and after
    {!give_up_after} of them in a row the page stops and says so. A
    connection that brought the session back starts the count over. *)

type t =
  { lost : int (** connections in a row lost before the session came back *)
  ; settling : bool (** this connection has not brought the session back *)
  ; resyncing : bool (** its messages are on their way after reconnecting *)
  ; rejoined : bool (** this connection is a reconnection *)
  }
[@@deriving sexp_of, equal]

(** The page's first connection: settling too. *)
val initial : t

val give_up_after : int

(** Connected again; [resyncing] when the session's messages were asked for
    again. *)
val rejoined : t -> resyncing:bool -> t

(** The messages asked for again arrived (or failed). *)
val resynced : t -> t

(** The session is back: the count starts over. *)
val settled : t -> t

(** The connection went. *)
val lost : t -> t

(** Too many connections in a row went before the session came back. *)
val gave_up : t -> bool

(** The user asked to try again: one more attempt, after which a loss gives
    up again. *)
val retry : t -> t
