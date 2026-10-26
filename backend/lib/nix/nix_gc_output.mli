open! Core
open! Import

(** What Nix prints while deleting profile generations and collecting
    garbage ([nix-env --delete-generations], [nix store gc -v],
    [nix-collect-garbage -v]). *)

module Line : sig
  type t =
    | Deleting of string (** a store path *)
    | Removing_generation (** of a profile *)
    | Summary of
        { paths : int
        ; bytes : int
        } (** [N store paths deleted, X MiB freed] *)
    | Other of string
  [@@deriving sexp_of]

  val classify : string -> t
end

(** Counts from the lines seen so far. *)
module Tally : sig
  type t =
    { mutable paths_deleted : int
    ; mutable generations_deleted : int
    ; mutable freed_bytes : int option (** from the summary *)
    }
  [@@deriving sexp_of]

  val create : unit -> t
  val add : t -> Line.t -> unit
end

(** Splits chunks of output into lines (without their newline). *)
module Splitter : sig
  type t

  val create : (string -> unit) -> t
  val feed : t -> string -> unit

  (** Passes on what is left after the last newline. *)
  val flush : t -> unit
end

(** ["1.5 GiB"]: [Some 1610612736]. *)
val parse_size : string -> int option
