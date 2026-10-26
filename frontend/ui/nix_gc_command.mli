open! Core
open! Import

(** [/nix-gc], shared by the terminal and browser frontends: [/nix-gc] shows
    the Nix store ([nix_store_status]), [/nix-gc run [DAYS|all]] collects its
    garbage ([nix_store_gc]), first deleting every user's profile generations
    older than DAYS days ([all]: all but the current ones). Only superusers
    may: the backend refuses others and says whom to ask. *)

module Action : sig
  type t =
    | Status
    | Collect of P.Nix_generations.t option
    | Error of string (** what was wrong, with the usage *)
  [@@deriving sexp_of, equal]
end

val usage : string
val parse : string -> Action.t

(** What to confirm before collecting: [Some] when profile generations are
    deleted, which users cannot roll back to afterwards. *)
val warning : P.Nix_generations.t option -> string option

(** The acknowledgement once a collection is asked for. *)
val starting : P.Nix_generations.t option -> string

(** ["deleting profile generations older than 14 days"]; [None]: none. *)
val describe_generations : P.Nix_generations.t option -> string option

(** ["1.5 GiB"] *)
val bytes : int -> string

(** ["3 min"], ["2 h"], ["4 days"] *)
val span : Time_ns.Span.t -> string

(** [schedule]: the container's automatic collection; [console]:
    [prigh-docker nix-gc]; otherwise the superuser. *)
val describe_by : string -> string

(** The store, its file system, the last and the running collection and the
    schedule, one line each, then how to collect. *)
val status_lines
  :  utc_offset:Time_ns.Span.t
  -> P.Nix_store_status.t
  -> string list

(** ["2026-10-26 14:02"] *)
val time : Time_ns.t -> utc_offset:Time_ns.Span.t -> string

(** [nix-gc: 12 paths, 1 generation deleted]: while one runs, for status
    lines. *)
val progress : P.Nix_gc_progress.t -> string

(** What a finished collection did. *)
val report : P.Nix_gc_report.t -> string
