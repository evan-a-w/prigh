open! Core

(** The user message with which the backend hands a conversation over to the
    next of [fallback_models] when a model's usage runs out:
    [\[prigh: FROM cannot continue (ERROR), so TO takes over this conversation
    from here. ...\]]. *)

type t =
  { from : string
  ; to_ : string
  ; error : string
  }
[@@deriving sexp_of, equal]

val parse : string -> t option

(** [↪ handed over from FROM to TO (ERROR)] *)
val summary : t -> string
