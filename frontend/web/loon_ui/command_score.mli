(** cmdk's ranking (the [command-score] package): how well a search matches
    an item's value, from 0 (no match) to 1 (equal). The search's characters
    must appear in order; matches that continue a run, or start a word
    (after a space, a dash, or punctuation such as [/], [_], [.] or [@]) score higher, skipped
    characters and case differences lower; an adjacent transposition still
    matches, weakly. *)

open! Import

val score : ?keywords:string list -> string -> search:string -> float
