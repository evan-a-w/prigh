open! Core

(** The provider failed transiently and the reply is asked for again: the
    [retry] event. A [Message_start] begins the new attempt. *)
type t =
  { attempt : int (** the attempt about to be made, from 2 *)
  ; max_attempts : int
  ; error : string (** what failed the attempt before *)
  ; delay_ms : int (** until it is made *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
