open! Core

(** A Nix garbage collection that finished: [nix_store_gc]'s reply, and
    {!Nix_store_status}'s last one. *)
type t =
  { at : Time_ns.t
  ; by : string
    (** a superuser's name, [schedule] (the container's automatic
        collection) or [console] ([prigh-docker nix-gc]) *)
  ; generations : Nix_generations.t option (** what it was asked to delete *)
  ; generations_deleted : int
  ; paths_deleted : int
  ; freed_bytes : Int64.t
  ; duration : Time_ns.Span.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
