open! Core
open! Import

(** Searching sessions: the query is split into words ({!words}), and a
    session matches when every word matches some word of its text, in any
    order and any field. A query word matches a text word exactly, as its
    prefix, inside it (3+ letters), with one typo (4+ letters: an edit,
    insertion, deletion or swap of adjacent letters against the word's
    start) or as a subsequence of it from its first letter (3+ letters),
    best to worst; never across words. Each word scores its best match
    times the weight of its field ({!Field}); ties go to the most recently
    updated session. The backend's [search_sessions] scores messages the
    same way, so its hits ({!P.Session_hits}) merge with these. *)

module Field : sig
  type t =
    | Name (** the session's name, or its title *)
    | Prompt (** its first prompt *)
    | Text (** its description; in the backend, its later messages *)
    | Place (** its cwd, repository, branch and worktrees *)
  [@@deriving sexp_of, equal]

  val weight : t -> int
end

module Kind : sig
  type t =
    | Exact
    | Prefix
    | Substring
    | Typo
    | Subsequence
  [@@deriving sexp_of, equal]

  val score : t -> int
end

(** Why a query word matched: its best field and how. *)
module Why : sig
  type t =
    { word : string
    ; field : Field.t
    ; kind : Kind.t
    }
  [@@deriving sexp_of, equal]
end

module Match : sig
  type t =
    { score : int
    ; ranges : (int * int) list list
      (** per field given, [(start, length)] in bytes of what matched *)
    ; why : Why.t list (** per query word *)
    }
  [@@deriving sexp_of, equal]
end

(** The query's words: runs of letters and digits (any non-ASCII byte is a
    letter), lowercased, each once. *)
val words : string -> string list

(** [None] when some word of [query] matches nothing; [Some] with score 0 for
    a query without words. *)
val score : query:string -> (Field.t * string) list -> Match.t option

(** [text] with [(start, length)] [ranges] cut out: the plain and the matched
    pieces, in order. *)
val pieces : string -> (int * int) list -> ([ `Plain | `Match ] * string) list

module Result : sig
  type t =
    { session : P.Session_summary.t
    ; score : int
    ; title : (int * int) list (** what matched in {!title} *)
    ; snippet : P.Session_hits.Snippet.t option
      (** from the backend's content search, when it found the session *)
    ; why : Why.t list
    }
  [@@deriving sexp_of, equal]
end

(** The name, else the description or first prompt. *)
val title : P.Session_summary.t -> string

(** The sessions matching [query], best first, merged with the hits of the
    backend's content search when [content] is for the same words (a reply
    to an older query is ignored); all of them, in the given order, for an
    empty query. *)
val sessions
  :  ?content:P.Session_hits.t
  -> query:string
  -> P.Session_summary.t list
  -> Result.t list
