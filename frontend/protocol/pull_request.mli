open! Core

(** A pull request as [pr_list], [pr_view] and [pr_create] show it. *)

module State : sig
  type t =
    | Open
    | Closed
    | Merged
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

type t =
  { number : int
  ; title : string
  ; state : State.t
  ; draft : bool
  ; author : string
  ; head : string (** the branch it merges *)
  ; base : string (** the branch it merges into *)
  ; url : string (** its web page *)
  ; body : string
  ; created_at : string (** ISO 8601 *)
  ; updated_at : string
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
