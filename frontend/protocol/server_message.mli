open! Core

(** One line from the backend. *)

module Response : sig
  type t =
    { id : int option
    ; result : (Json.t, string) Result.t
    }
  [@@deriving sexp_of]
end

type t =
  | Response of Response.t
  | Event of Event.t
[@@deriving sexp_of]

val of_json : Json.t -> t Or_error.t
val of_line : string -> t Or_error.t
