open! Core

(** Fuzzy matching of file paths, as Go to file does it: the query's letters
    in order anywhere in the path (case-insensitive, spaces ignored), ranked
    by where they fall: in the file's name over its directories, at the
    start of a name or a word ([/ _ - .], camelCase), in runs, and shorter
    paths first. Fast enough for a few hundred thousand paths a keystroke. *)

module Match : sig
  type t =
    { path : string
    ; score : int
    ; positions : int array (** byte offsets of the letters matched *)
    }
  [@@deriving sexp_of]
end

(** [None] when the query's letters are not in the path, in order. *)
val score : query:string -> string -> Match.t option

(** The paths that match, best first (equal scores: shorter, then in
    alphabetical order). *)
val rank : query:string -> string array -> Match.t array

(** Whether every path matching [query'] matches [query] too ([query]'s
    letters begin [query']'s), so [query']'s matches are among [query]'s. *)
val narrows : query:string -> query':string -> bool
