open! Core
open Prigh_protocol

(** Resolves what the user typed after [/model] against the catalog: exact
    key/id/name, then unique case-insensitive prefix or word match, otherwise
    "did you mean" suggestions. *)

type t =
  | Found of Model.t
  | Ambiguous of Model.t list
  | Not_found of Model.t list (** closest suggestions *)
[@@deriving sexp_of]

val resolve : Model.t list -> string -> t
