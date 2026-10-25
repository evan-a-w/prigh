open! Core

(** [pr_list]'s reply. *)

type t =
  { repo : Forge_repo.t
  ; prs : Pull_request.t list (** newest first *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
