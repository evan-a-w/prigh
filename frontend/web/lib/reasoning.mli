open! Core
open! Import

(** The model menu's Reasoning column and the model chip's level: the
    thinking settings a model takes, as words. *)

module Row : sig
  type t =
    { id : string (** what [set_thinking] takes *)
    ; label : string
    ; default : bool (** what the provider picks when sent none *)
    ; current : bool
    }
  [@@deriving sexp_of, equal]
end

(** ["xhigh"] is ["Extra High"], ["default"] ["Default"]. *)
val level_label : string -> string

(** Off (when it can be), then its levels, lowest first. The level the
    provider picks by itself is marked [default] and sets ["default"]; when
    the model does not say which that is, ["default"] is a row of its own. *)
val rows : Llm.t -> current:string -> Row.t list

(** The level the chip shows ([None]: the model does not think): the
    provider's own level for ["default"] when known. *)
val chip_label : Llm.t -> current:string -> string option
