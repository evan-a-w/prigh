open! Core

(** The session's goal ([/goal]), as the state reports it. *)

module Status : sig
  type t =
    | Active
    | Done
    | Stopped
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

type t =
  { text : string
  ; status : Status.t
  ; continuations : int (** automatic continuations used *)
  ; max_continuations : int
  ; summary : string option (** the model's, once done *)
  ; reason : string option (** why it stopped *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
