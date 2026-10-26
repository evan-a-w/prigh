open! Core
open! Import

(** The messages a session's compaction carries over ahead of its summary:
    the user's own (shortened when long) and their latest (as it is). *)

module Pin : sig
  type t =
    { id : string (** of the message's entry *)
    ; shortened : bool (** shown through {!shorten} *)
    }
  [@@deriving sexp, jsonaf, equal]
end

(** As a compaction entry records them. Files from before there could be
    more than one have a single id (the user's latest message, not
    shortened), which reads as one pin. *)
type t = Pin.t list [@@deriving sexp, jsonaf, equal]

(** How much of a shortened message's text is kept. *)
val max_chars : int

(** The message with its text cut to the first [max_chars] characters and a
    note of how many were left out ([[... N characters omitted]]). Short
    messages, and those already cut, are unchanged. *)
val shorten : Message.t -> Message.t
