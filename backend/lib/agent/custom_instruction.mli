open! Core
open! Import

(** One of the user's custom instructions as the model sees it (without
    when it was saved), with how the list is shown to the model: in the
    system prompt of new sessions, and as a note listing what changed for
    sessions that were told an earlier list ({!Custom_instructions} keeps
    them). *)

type t =
  { id : string (** [ci1], [ci2], ... *)
  ; title : string option
  ; text : string
  }
[@@deriving sexp, jsonaf, equal]

(** The system prompt's block, [None] without instructions:
    {v
Custom instructions (the user's standing instructions; follow them):
[ci1] Use Australian spelling.
[ci3] (Maths) Keep a single memory note on my maths ability up to date.
    v}
    A text's further lines are indented under its first. *)
val section : t list -> string option

module Change : sig
  type instruction := t

  type t =
    | Added of instruction
    | Edited of instruction (** its new title and text *)
    | Removed of instruction (** as it was *)
  [@@deriving sexp_of]
end

(** What turned [told] into [now], by id: removed, then edited and added in
    [now]'s order. *)
val changes : told:t list -> t list -> Change.t list

(** The note for a model that was told [told], [None] when nothing changed:
    the changes, or the whole list when most of it changed (or that there
    are none now). *)
val note : told:t list -> t list -> string option

(** [told] after this session's own change to [id] ([None]: removed), so
    that it is not told back to it: the note is replaced in place, or added
    at the end. *)
val apply : t list -> id:string -> t option -> t list
