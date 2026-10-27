open! Core
open! Import

(** Where the transcript's entries are in the session's context: the index
    of each entry's message as [get_messages_page] counts them
    ({!Message_page.older}, {!Message_point.index}), from the loaded pages
    (messages and entries go one for one, but tool results, which join
    their calls' entries). For the actions on a message (delete, trim after
    it), checked against [get_message_points] when used, and for the
    dividers of what is trimmed after a message. *)

module Span : sig
  type t =
    { first : int (** the entry's message *)
    ; next : int (** the message after it and its calls' results *)
    }
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

(** Of [chat], whose first loaded entry is message [paging.older]; nothing
    when the transcript has a compaction made since it was loaded (what
    came before it is not in the context). The last one made is reused for
    the same [chat] and [paging]. *)
val create : paging:Paging.t -> Chat.t -> t

(** Entry [n]'s, if it is a message of the context (not a [!!command]). *)
val span : t -> int -> Span.t option

(** The entry whose message is [index]. *)
val entry_of_index : t -> int -> int option

(** The entry whose message, or one of its calls' results, is [index]. *)
val entry_at : t -> int -> int option

(** The newest entry that is a message of the context. *)
val newest : t -> int option

(** Where a divider [divider] messages into the context goes: after this
    entry ([Chat.first chat - 1]: before the first loaded); [None] when
    that is not loaded, or not between two entries. *)
val after : t -> divider:int -> int option
