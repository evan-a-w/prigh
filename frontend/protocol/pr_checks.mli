open! Core

(** The CI status of a pull request's head commit: GitHub's check runs, or
    Forgejo's commit statuses. *)

module Item : sig
  type t =
    { name : string
    ; state : string (** [success], [failure], [pending] or [skipped] *)
    ; url : string option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { state : string
    (** overall: [success], [failure], [pending], or [none] (no checks) *)
  ; items : Item.t list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
