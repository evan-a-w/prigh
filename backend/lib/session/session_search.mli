open! Core

(** Searching the text of saved sessions ([search_sessions]): their names and
    what the user and the assistant wrote (not tool calls, their results or
    thinking).

    Text and queries are split into words (runs of ASCII letters and digits,
    or non-ASCII bytes; ASCII lowercased). A query word matches a word of the
    text exactly (10), as its prefix (8), inside it (6, query words of 3
    letters or more) or with one typo against a prefix of it (4, query words
    of 4 letters or more: Damerau-Levenshtein distance 1). Each is weighted by
    where it is: the name 4, the first user message 3, other messages 2. A
    session's score is the sum over the query's words of each one's best match
    in it; every word must match somewhere.

    The index is kept in memory and brought up to date on each search: only
    the session files whose size, modification time or name changed are read
    again; an inverted index (word to sessions) means a query looks at each
    distinct word once rather than at every text. The backend builds it in
    the background as it starts ({!index}), so that the first search does
    not read every session. *)

type t

val create : unit -> t

module Snippet : sig
  (** Part of a message around a match, whitespace collapsed, ["…"] where it
      is cut; [ranges] are the matches in it, (byte offset, length), sorted
      and disjoint. *)
  type t =
    { text : string
    ; ranges : (int * int) list
    }
  [@@deriving sexp_of]
end

module Hit : sig
  type t =
    { id : string
    ; path : string
    ; score : int
    ; snippet : Snippet.t option (** [None] when only the name matches *)
    }
  [@@deriving sexp_of]

  val to_json : t -> Jsonaf.t
end

(** The best [limit] matches among [sessions] (and only those: the index
    forgets the others), best first, the most recently active first among
    equals. [load] reads a session whose file changed since it was indexed
    ([None]: left out). No hits for a query without words. *)
val search
  :  t
  -> sessions:Session.Summary.t list
  -> load:(Session.Summary.t -> Session.t option)
  -> query:string
  -> limit:int
  -> Hit.t list

(** Brings the index up to date with [sessions ()] as [search] would,
    calling [yield] after each session so that other work goes on meanwhile.
    A [search] meanwhile waits for it to finish, then answers from the whole
    index; another [index] meanwhile waits for this one. *)
val index
  :  t
  -> sessions:(unit -> Session.Summary.t list)
  -> load:(Session.Summary.t -> Session.t option)
  -> yield:(unit -> unit)
  -> unit

(** How many sessions are in the index. *)
val indexed : t -> int

(** The words of [s], as text and queries are split. *)
val words : string -> string list
