open! Core
open! Import

(** What [nix_store_status] answers. *)

module Running : sig
  (** A garbage collection under way. *)
  type t =
    { by : string (** a superuser, or [schedule] *)
    ; since : float
    ; generations : Nix_generations.t option
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t

  (** The helper's [key=value] lines. *)
  val of_fields : string -> t Or_error.t
end

module Schedule : sig
  (** The container's automatic garbage collection. *)
  type t =
    { interval_seconds : int
    ; keep_days : int option (** profile generations kept; [None]: all *)
    ; next_at : float option
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
  val of_fields : string -> t Or_error.t
end

module Store : sig
  (** The store and the filesystem it is on. *)
  type t =
    { bytes : int
    ; paths : int
    ; fs_total_bytes : int
    ; fs_free_bytes : int
    }
  [@@deriving sexp_of]
end

type t =
  { store : Store.t
  ; last_gc : Nix_gc_report.t option
  ; running : Running.t option
  ; schedule : Schedule.t option
  }
[@@deriving sexp_of]

val to_json : t -> Json.t
