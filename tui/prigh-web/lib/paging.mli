open! Core
open! Import

(** How much of the session's transcript is loaded: the newest page first
    ([get_messages_page]), older pages as the user scrolls up. *)

type t =
  { older : int (** messages before those loaded *)
  ; before : string option (** what to ask for the page before with *)
  ; loading : bool (** that page is on its way *)
  }
[@@deriving sexp_of, equal]

(** Everything is loaded. *)
val all : t

val of_page : Message_page.t -> t

(** How many messages a page has. *)
val size : int

(** The cursor of the page before, when there is one to load and none on its
    way. *)
val next : t -> string option

(** Above the transcript while older messages are not loaded: how many, and
    a button that loads them ([load]). The page also loads them as the user
    scrolls near the top ([data-older] is the marker it looks for). *)
val view : t -> load:unit Effect.t -> Node.t
