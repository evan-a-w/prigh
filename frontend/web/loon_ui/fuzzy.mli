(** Fuzzy matching, as in a command palette: the query's characters appear
    in order in the text (case-insensitive). *)

open! Import

(** [None] when [query] does not match [text]; otherwise a score, higher for
    better matches (consecutive characters, word starts, a prefix, shorter
    texts). The empty query matches everything with score 0. *)
val score : query:string -> string -> int option

(** The best score of the query against any of the texts. *)
val best : query:string -> string list -> int option
