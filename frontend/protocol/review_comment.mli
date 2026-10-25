open! Core

(** A review comment on lines of a changed file, with its thread (the
    backend's [Review_comment], as [get_review] and the [review] event send
    it). *)

module Author : sig
  type t =
    | User
    | Agent
  [@@deriving sexp_of, equal]
end

module Edit : sig
  type t =
    { old_text : string
    ; new_text : string
    }
  [@@deriving sexp_of, equal]
end

module Proposal : sig
  module State : sig
    type t =
      | Pending
      | Accepted
      | Rejected
      | Failed of string (** accepted, but it no longer applied *)
    [@@deriving sexp_of, equal]
  end

  type t =
    { edits : Edit.t list
    ; state : State.t
    }
  [@@deriving sexp_of, equal]
end

module Post : sig
  type t =
    { author : Author.t
    ; text : string
    ; at : Time_ns.t option
    ; proposal : Proposal.t option
    ; delivered : bool (** the agent has seen it *)
    }
  [@@deriving sexp_of, equal]
end

module Status : sig
  type t =
    | Open
    | Resolved
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

type t =
  { id : string
  ; root : string (** the repository *)
  ; path : string (** relative to [root] *)
  ; first : int (** lines when commented on, 1-based, inclusive *)
  ; last : int
  ; anchor : string (** their text then *)
  ; status : Status.t
  ; thread : Post.t list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** A post of the user's the agent has not seen. *)
val unsent : t -> bool

(** A proposal waiting for the user. *)
val pending : t -> bool
