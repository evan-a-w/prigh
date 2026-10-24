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
  [@@deriving sexp_of, equal]

  (** For a picker item's id, and back. *)
  val to_string : t -> string

  val of_string : string -> t option
end

module Point : sig
  type t =
    { target : Target.t
    ; kind : Outline.Kind.t
    ; text : string (** its first line; empty for a compaction *)
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]
end

(** Whether the entry is one of [points] (its view is marked
    [data-entry=N] for the page to find it). *)
val landmark : Chat.Entry.t -> bool

type t =
  { outline : (string * Outline.t) option
    (** the session's (by path), for the pages not loaded *)
  ; fetching : string option (** the outline of this path is on its way *)
  ; pending : int option
    (** the message index being loaded to jump to, once its page is in *)
  }
[@@deriving sexp_of]

val empty : t

(** Oldest first: the outline's points before the first message loaded
    ([paging.older]), then those of [chat]. *)
val points
  :  t
  -> path:string option
  -> paging:Paging.t
  -> Chat.t
  -> Point.t list

(** The entry that the message [index] became, once the page [page] was
    added before the others ([Chat.prepend]) and the transcript starts at
    entry [first]; [None] when the page does not have it. *)
val entry_of_index : page:Message_page.t -> first:int -> int -> int option

(** The jump list: the user's messages, newest first, each [Target.to_string]
    as its id. *)
val picker : Point.t list -> times:Message_time.t -> Picker.t
