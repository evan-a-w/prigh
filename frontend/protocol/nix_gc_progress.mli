open! Core

(** A [nix_gc_progress] event: how far the collection we asked for got. *)
type t =
  { paths_deleted : int
  ; generations_deleted : int
  ; message : string option (** Nix's latest line worth showing *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
