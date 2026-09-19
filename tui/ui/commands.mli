open! Core

(** The slash-command table. *)

module Spec : sig
  type t =
    { name : string
    ; args : string
    ; help : string
    }
  [@@deriving sexp_of]
end

val all : Spec.t list
val find : string -> Spec.t option

module Parsed : sig
  type t =
    { name : string
    ; args : string list
    ; rest : string
    }
  [@@deriving sexp_of]
end

(** [None] unless the input starts with [/]. *)
val parse : string -> Parsed.t option

module Completion : sig
  type t =
    | Unique of string (** the completed input, ending in a space *)
    | Common_prefix of string (** longer than the input *)
    | Candidates of Spec.t list (** several, none longer in common *)
    | Nothing
  [@@deriving sexp_of]
end

val complete : string -> Completion.t
val closest : string -> Spec.t option
val help : Content.t
