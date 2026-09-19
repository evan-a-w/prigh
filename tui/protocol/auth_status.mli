open! Core

module Method : sig
  type t =
    { method_ : string
    ; label : string
    }
  [@@deriving sexp_of, equal]
end

module Configured : sig
  type t =
    { method_ : string
    ; source : string
    }
  [@@deriving sexp_of, equal]
end

type t =
  { provider : string
  ; name : string
  ; methods : Method.t list
  ; configured : Configured.t option
  ; expires_ms : int option
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
