open! Core

(** The result of [hello]: our client id and, when the backend has token
    namespaces, the one we logged in to (the user name). *)
type t =
  { client_id : string
  ; namespace : string option
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
