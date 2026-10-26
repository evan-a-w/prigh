open! Core
open! Import

(** A [git diff] (as [/diff] gets it) split into files, for both frontends:
    the terminal prints it in the transcript, prigh-web shows each file's
    hunks in its diff view. *)

module File : sig
  module Status : sig
    type t =
      | Added
      | Deleted
      | Modified
      | Binary
    [@@deriving sexp_of, equal]
  end

  type t =
    { path : string
    ; status : Status.t
    ; added : int
    ; removed : int
    ; hunks : string list (** from the first [@@] line *)
    ; hash : string
      (** of the file's whole section of the diff (MD5, hex): it changes
          whenever the file's content does, so approvals are keyed by it
          ({!File_approval}) *)
    }
  [@@deriving sexp_of, equal]
end

val parse : string -> File.t list

(** What the worktree is compared with: [HEAD], a base's merge base as
    [main (merge base 1234abcd)], a commit shortened to 8 characters, the
    empty tree as [nothing (no commits yet)]. *)
val against : P.Git_diff.t -> string

(** The worktree reviewed: [feat/x (/path/to/it)], or its path when
    detached. *)
val where : P.Git_diff.t -> string

(** [3 files changed, +12 −4], or [no changes]. *)
val summary : File.t list -> string

(** The terminal's rendering: a header with the summary, then each file's
    name and counts and its hunks, coloured. *)
val render : P.Git_diff.t -> Content.t
