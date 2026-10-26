open! Core
open! Import

(** A pull request, from GitHub's or Forgejo's JSON (which agree on most
    fields). *)

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
  ; head : string (** the branch, [owner:branch] from another repository *)
  ; head_sha : string
  ; base : string
  ; url : string
  ; body : string
  ; created_at : string
  ; updated_at : string
  }
[@@deriving sexp_of]

val of_json : Forge_kind.t -> Json.t -> t Or_error.t

(** The wire form ([head_sha] is not part of it). *)
val to_json : t -> Json.t

(** Forgejo marks drafts with a [WIP:] title prefix. *)
val forgejo_draft_title : string -> string
