open! Core
open! Import

(** A [nix_gc_progress] event: the counts so far and the latest message. *)
type t =
  { paths_deleted : int
  ; generations_deleted : int
  ; message : string option (** a line of Nix's that is not [deleting ...] *)
  }
[@@deriving sexp_of]

val to_json : t -> Json.t
