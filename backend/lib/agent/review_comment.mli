open! Core
open! Import

(** A review comment the user left on lines of a file (or on text selected
    in them), with its thread: the
    user's posts and the agent's replies ([reply_to_comment]), which may
    propose a change for the user to accept or reject. Recorded in the
    session ({!Review_state}). *)

module Side : sig
  (** Which text the lines are of: the file as it is ([New]), or what the
      change removed ([Old]), numbered as in the file before it. *)
  type t =
    | Old
    | New
  [@@deriving sexp, jsonaf, equal]

  (** [old], [new]. *)
  val to_string : t -> string

  val of_string : string -> t Or_error.t
end

module Columns : sig
  (** A selection within the lines: where it starts in the first line and
      ends in the last, as byte offsets into them (the end exclusive). *)
  type t =
    { start_col : int
    ; end_col : int
    }
  [@@deriving sexp, jsonaf, equal]
end

module Range : sig
  (** Lines of the file as it was when commented on, 1-based, inclusive;
      with [columns], the text selected in them. Comments saved before
      selections have neither [side] (they are [New]) nor [columns]. *)
  type t =
    { first : int
    ; last : int
    ; side : Side.t
    ; columns : Columns.t option
    }
  [@@deriving sexp, jsonaf, equal]

  (** Whole lines of the file as it is. *)
  val lines : first:int -> last:int -> t

  (** [12] or [12-14], [old 12] for removed lines. *)
  val to_string : t -> string

  (** The selected text of [anchor] (the lines' text): all of it without
      [columns]. *)
  val selected : t -> anchor:string -> string

  (** Whether it is a range of lines, and its columns fit [anchor]; the
      error says what is wrong. *)
  val validate : t -> anchor:string -> unit Or_error.t
end

module Author : sig
  type t =
    | User
    | Agent
  [@@deriving sexp, jsonaf, equal]
end

module Edit : sig
  (** Like the [edit] tool's: [old_text] occurs exactly once in the file. *)
  type t =
    { old_text : string
    ; new_text : string
    }
  [@@deriving sexp, jsonaf, equal]
end

module Proposal : sig
  module State : sig
    type t =
      | Pending
      | Accepted
      | Rejected
      | Failed of string (** accepted, but the edits no longer applied *)
    [@@deriving sexp, jsonaf, equal]

    (** [pending], [accepted], [rejected], [failed]. *)
    val to_string : t -> string
  end

  (** A change the agent proposes in a reply, applied to the comment's file
      when the user accepts it. *)
  type t =
    { edits : Edit.t list
    ; state : State.t
    }
  [@@deriving sexp, jsonaf, equal]
end

module Post : sig
  type t =
    { author : Author.t
    ; text : string
    ; at : float (** seconds since the epoch *)
    ; proposal : Proposal.t option (** only in the agent's *)
    ; delivered : bool
      (** the agent has seen it: its own, or a user's sent with
          {!Review_state.submit} *)
    }
  [@@deriving sexp, jsonaf, equal]
end

module Status : sig
  type t =
    | Open
    | Resolved
  [@@deriving sexp, jsonaf, equal]

  val to_string : t -> string
  val of_string : string -> t Or_error.t
end

type t =
  { id : string (** [c1], [c2], ...: never reused in a session *)
  ; root : string (** the repository's top directory, as [git_diff] says *)
  ; path : string (** relative to [root], as [git diff] names it *)
  ; range : Range.t
  ; anchor : string
    (** the lines' text when commented on, so a frontend can find them again
        after the file changed around them *)
  ; status : Status.t
  ; thread : Post.t list (** oldest first; the first is the comment *)
  }
[@@deriving sexp, jsonaf, equal]

(** Whether a post of the user's has not been sent to the agent yet. *)
val unsent : t -> bool

(** [root/path]. *)
val file : t -> string

(** The comment as the agent reads it: id, file, lines (and where the
    selection is in them, as 1-based character columns), the commented code
    with line numbers (a selection marked [⟦like this⟧], and quoted whole),
    and the thread, the posts it has not seen marked [new]. *)
val to_prompt : t -> string
