open! Core

(** [nix_store_status]'s reply (superusers only): the store's size, its
    file system's space, the last collection, one running and the automatic
    schedule. *)

module Running : sig
  type t =
    { by : string (** as {!Nix_gc_report.by} *)
    ; since : Time_ns.t
    ; generations : Nix_generations.t option
    }
  [@@deriving sexp_of, equal]
end

module Schedule : sig
  (** The container's automatic collection. *)
  type t =
    { interval : Time_ns.Span.t
    ; keep_days : int option
      (** the profile generations older than this are deleted; [None]:
          they are kept *)
    ; next_at : Time_ns.t option (** [None]: off *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { store_bytes : int
  ; store_paths : int
  ; fs_total_bytes : int (** of [/nix]'s file system *)
  ; fs_free_bytes : int
  ; last_gc : Nix_gc_report.t option (** [None]: never *)
  ; running : Running.t option
  ; schedule : Schedule.t option (** [None]: not in the container *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
