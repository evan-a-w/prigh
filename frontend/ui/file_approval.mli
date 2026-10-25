open! Core
open! Import

(** Whether the user approved a changed file in the review ([P.Review]'s
    approvals), keyed by its {!Diff_files.File.hash}: a file that changed
    after it was approved is no longer approved. *)

module Status : sig
  type t =
    | Approved
    | Changed (** approved, but it changed since *)
    | Unapproved
  [@@deriving sexp_of, equal]

  (** [approved], [changed since approved], [not approved]. *)
  val to_string : t -> string
end

val status : P.Review.t -> Diff_files.File.t -> Status.t

(** How many of [files] are approved. *)
val approved : P.Review.t -> Diff_files.File.t list -> int

(** The terminal's [/review]: each changed file with its status and its
    comments' threads, after a summary. *)
val render : P.Git_diff.t -> P.Review.t -> Content.t
