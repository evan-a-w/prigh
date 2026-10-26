open! Core

(** [pr_view]'s reply (and [pr_create]'s, without checks or diff). *)

type t =
  { repo : Forge_repo.t
  ; pr : Pull_request.t
  ; checks : Pr_checks.t option (** [None] when they could not be read *)
  ; diff : string (** unified diff of the whole pull request; [""] if none *)
  ; truncated : bool (** the diff was cut *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** The diff as a {!Git_diff.t}, for the frontends' diff views. *)
val git_diff : t -> Git_diff.t
