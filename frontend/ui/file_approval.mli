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

(** What changed in a file since the last action on it, for lists: [+3 −1
    since approved] ([changed since approved] when it has no counts, binary
    or too large); [None] when nothing did. *)
val since_label : P.Review_since.File.t -> string option

(** A file's status with what changed since the last action:
    [changed since approved (+3 −1)], [not approved, +2 −0 since
    commented]. *)
val describe : Status.t -> P.Review_since.File.t option -> string

(** The terminal's [/review]: each changed file with its status (and what
    changed since the last action on it, from [since]) and its comments'
    threads, after a summary. *)
val render : ?since:P.Review_since.t -> P.Git_diff.t -> P.Review.t -> Content.t
