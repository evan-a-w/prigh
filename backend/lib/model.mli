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
  ; provider : Provider_id.t
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; cost : Cost.t
  }
[@@deriving sexp_of]

val all : t list
val default : t
val default_for : Provider_id.t -> t

(** ["provider/id"]; the same id can exist under several providers. *)
val key : t -> string

(** Accepts a bare id (first provider that has it) or a [key]. *)
val find : string -> t option

val cost_usd : t -> Usage.t -> float
