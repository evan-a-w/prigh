open! Core
open! Import

(** The Nix store for superusers, in Settings and [/nix-gc]: what
    [nix_store_status] said, and the garbage collection we asked for while it
    runs. The status belongs to the user (switching starts over); the
    collection to the connection. *)

module Status : sig
  type t =
    | Unknown (** not asked *)
    | Loading
    | Loaded of Nix_store_status.t
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { status : Status.t
  ; collecting : Nix_gc_progress.t option (** ours, while it runs *)
  ; outcome : (Nix_gc_report.t, string) Result.t option
    (** how our last collection went *)
  }
[@@deriving sexp_of, equal]

val empty : t

val loading : t -> t
val status : t -> (Nix_store_status.t, string) Result.t -> t

(** A collection asked for: no progress yet. *)
val start : t -> t

(** Progress of another collection than ours is ignored. *)
val progress : t -> Nix_gc_progress.t -> t

val finished : t -> (Nix_gc_report.t, string) Result.t -> t
