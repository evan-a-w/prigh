open! Core

module Cost : sig
  type t =
    { input : float
    ; output : float
    ; cache_read : float
    }
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; provider : string
  ; key : string
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; cost : Cost.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
