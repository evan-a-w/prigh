open! Core
open! Import

(** Jumping around a long transcript: its landmarks (the user's messages,
    compactions, replies that failed) from what is loaded ([Chat]) and, for
    the pages not loaded yet, from the session's outline ([get_outline]);
    and a jump to a message not loaded, which loads the pages down to it
    first. *)

module Target : sig
  type t =
    | Entry of int (** a loaded entry, by its number ([Chat.first]) *)
    | Message of int
    (** a message not loaded, by its index ({!Outline.Point.index}) *)
    | Trimmed
    (** the divider in place of the messages trimmed from the context, at
        the transcript's start *)
  [@@deriving sexp_of, equal]

  (** For a picker item's id, and back. *)
  val to_string : t -> string

  val of_string : string -> t option
end

module Kind : sig
  type t =
    | User
    | Compaction
    | Error
    | Trimmed (** what is trimmed from the context ([Target.Trimmed]) *)
  [@@deriving sexp_of, equal]
end

module Point : sig
  type t =
    { target : Target.t
    ; kind : Kind.t
    ; text : string
      (** its first line; empty for a compaction; for [Trimmed], how much *)
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]
end

(** A user message's text as the landmarks show it: its first line, a
    skill's invocation as typed. *)
val headline : string -> string

(** Whether the entry is one of [points] (its view is marked
    [data-entry=N] for the page to find it). *)
val landmark : Chat.Entry.t -> bool

type t =
  { outline : (string * Outline.t) option
    (** the session's (by path), for the pages not loaded *)
  ; fetching : string option (** the outline of this path is on its way *)
  ; pending : Target.t option
    (** the message ([Message index]) or the transcript's start ([Trimmed])
        being loaded to jump to, once its page is in *)
  }
[@@deriving sexp_of]

val empty : t

(** Oldest first: what is trimmed from the context ([trim]), the outline's
    points before the first message loaded ([paging.older]), then those of
    [chat] but those [deleting] (entries hidden while deleted). *)
val points
  :  ?deleting:Int.Set.t
  -> t
  -> path:string option
  -> paging:Paging.t
  -> trim:Trim.t option
  -> Chat.t
  -> Point.t list

(** The entry that the message [index] became, once the page [page] was
    added before the others ([Chat.prepend]) and the transcript starts at
    entry [first]; [None] when the page does not have it. *)
val entry_of_index : page:Message_page.t -> first:int -> int -> int option

(** Whether the context can be trimmed before the point: one of the user's
    messages but the first in the context. *)
val trimmable : paging:Paging.t -> Chat.t -> Point.t -> bool

(** Which of the user's messages in [points] the target is, counting from
    the newest (0), as {!Trim.Point}s are matched to them; and its point. *)
val from_end : Point.t list -> Target.t -> (int * Point.t) option

(** The jump list: the user's messages, newest first, each [Target.to_string]
    as its id; what is trimmed, last. *)
val picker : Point.t list -> times:Message_time.t -> Picker.t
