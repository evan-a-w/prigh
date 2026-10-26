open! Core

(** [pr_defaults]'s reply: what a new pull request from the current branch
    starts with, for the create dialog. *)

type t =
  { repo : Forge_repo.t
  ; head : string (** the current branch *)
  ; base : string (** the repository's default branch *)
  ; title : string (** the only commit's subject, else from the branch *)
  ; body : string (** the commit's body, or a list of the commits *)
  ; commits : string list (** subjects of the commits on [head] not on [base] *)
  ; pushed : bool
    (** [head] is on the remote with all its commits; otherwise
        [pr_create {push: true}] pushes it first *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
