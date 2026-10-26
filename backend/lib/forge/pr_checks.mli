open! Core
open! Import

(** CI results for a commit: GitHub's check runs and commit statuses, or
    Forgejo's commit statuses, reduced to [success], [failure], [pending]
    and [skipped]. *)

module Item : sig
  type t =
    { name : string
    ; state : string
    ; url : string option
    }
  [@@deriving sexp_of]
end

type t =
  { state : string (** overall; [none] without any checks *)
  ; items : Item.t list
  }
[@@deriving sexp_of]

(** From [GET /commits/SHA/check-runs]. *)
val of_github_check_runs : Json.t -> Item.t list

(** From [GET /commits/SHA/status] (GitHub's and Forgejo's). *)
val of_statuses : Json.t -> Item.t list

val of_items : Item.t list -> t
val to_json : t -> Json.t
