open! Core

(** A review comment on lines of a changed file (or on text selected in
    them), with its thread (the
    backend's [Review_comment], as [get_review] and the [review] event send
    it). *)

module Side : sig
  (** The file as it is ([New]), or the lines the change removed ([Old]),
      numbered as in the file before it. *)
  type t =
    | Old
    | New
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

module Columns : sig
  (** Where a selection starts in its first line and ends in its last: byte
      offsets into them, the end exclusive. *)
  type t =
    { start_col : int
    ; end_col : int
    }
  [@@deriving sexp_of, equal]
end

module Range : sig
  (** Lines when commented on, 1-based, inclusive; with [columns], the text
      selected in them. *)
  type t =
    { first : int
    ; last : int
    ; side : Side.t
    ; columns : Columns.t option (** [None]: the whole lines *)
    }
  [@@deriving sexp_of, equal]

  (** Whole lines of the file as it is. *)
  val lines : first:int -> last:int -> t

  (** [line 3], [lines 3–5], [old line 3]. *)
  val label : t -> string

  (** The selected text of [anchor] (the lines' text): all of it without
      columns. *)
  val selected : t -> anchor:string -> string

  (** [start_col], [end_col] and [side], as [review_comment] takes them. *)
  val params : t -> (string * Json.t) list
end

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
  ; range : Range.t
  ; anchor : string (** the lines' text then *)
  ; status : Status.t
  ; thread : Post.t list
  }
[@@deriving sexp_of, equal]

(** Older backends send neither [side] nor columns: whole lines of the file
    as it is. *)
val of_json : Json.t -> t Or_error.t

(** The selected text. *)
val selected : t -> string

(** A post of the user's the agent has not seen. *)
val unsent : t -> bool

(** A proposal waiting for the user. *)
val pending : t -> bool
