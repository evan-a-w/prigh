open! Core
open! Import

(** A finished garbage collection: what [nix_store_gc] answers and
    [nix_store_status] shows as the last one. *)
type t =
  { at : float (** when it finished, Unix time *)
  ; by : string (** the superuser who asked, or [schedule] *)
  ; generations : Nix_generations.t option (** deleted first *)
  ; generations_deleted : int
  ; paths_deleted : int
  ; freed_bytes : int
  ; seconds : float (** how long it took *)
  }
[@@deriving sexp_of]

val to_json : t -> Json.t

(** The [key=value] lines the container helper writes (see
    [docker/prigh-docker]). *)
val of_fields : string -> t Or_error.t

(** [key=value] lines into an association list; other lines are skipped. *)
val fields : string -> (string * string) list
