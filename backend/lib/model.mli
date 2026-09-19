open! Core

(** Per-million-token prices in USD. *)
module Cost : sig
  type t =
    { input : float
    ; output : float
    ; cache_read : float
    }
  [@@deriving sexp_of]
end

type t =
  { id : string
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; cost : Cost.t
  }
[@@deriving sexp_of]

val all : t list
val default : t
val find : string -> t option
val cost_usd : t -> Usage.t -> float
