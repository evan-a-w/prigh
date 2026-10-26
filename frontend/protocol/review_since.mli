open! Core

(** [review_since]'s reply: for each file of a worktree with review actions
    (or the one asked for), what changed in it since the last action (or the
    one asked for, by its [index] in {!Review.history}). *)

module File : sig
  type t =
    { path : string
    ; index : int
    ; snapshot : Review.Snapshot.t (** the action compared with *)
    ; current : Review.Snapshot.Content.t (** the file now *)
    ; changed : bool
    ; added : int
    ; removed : int
    ; diff : string
      (** the snapshot to now, as [git diff] writes one file; [""] when
          unchanged *)
    ; note : string option (** why a changed file has no diff *)
    }
  [@@deriving sexp_of, equal]
end

type t = File.t list [@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
