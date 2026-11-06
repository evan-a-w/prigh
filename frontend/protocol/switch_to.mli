open! Core

(** Where deleting the session the client is in moves it first: the
    [switch_to] of [delete_sessions], [delete_session] and
    [delete_archived]. *)
type t =
  | Session of string (** a path or id, not among those deleted *)
  | New_session (** a new empty session, as [new_session] *)
[@@deriving sexp_of, equal]

val params : t option -> (string * Json.t) list
