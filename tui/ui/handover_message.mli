open! Core

(** When a model's usage runs out, the backend switches to the next of
    [fallback_models] and starts a run with the user message
    [\[prigh: FROM cannot continue (ERROR), so TO takes over this conversation
    from here. Carry on with the task where it left off.\]]. *)

type t =
  { from : string
  ; to_ : string
  ; error : string
  }
[@@deriving sexp_of, equal]

(** [None] unless [text] is a hand-over message. *)
val parse : string -> t option

(** [↪ handed over from FROM to TO], without the error. *)
val summary : t -> string
