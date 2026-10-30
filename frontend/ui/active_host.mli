open! Core
open! Import

(** Where the session's tools run, as the frontends show it: by the host's
    name (its hostname, or [-name]), never its id unless nothing better is
    known, also while that host is disconnected. *)

type t =
  | Connected of P.Host.t
  | Disconnected of
      { id : string
      ; name : string (** as last seen, else [id] *)
      } (** the session waits for it to reconnect *)
  | Unset
  (** no host: none adopted yet, or the backend's, which is disabled (it
      never disconnects) *)
[@@deriving sexp_of, equal]

val of_state : P.State.t -> t

(** What to do about a disconnected host, for hints. *)
val reconnect_hint : name:string -> string
