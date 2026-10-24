open! Core

(** Levenshtein distance, ignoring case. *)
val caseless : string -> string -> int

(** The (at most [n], default 3) [candidates] closest to [query]. *)
val closest : ?n:int -> string list -> string -> string list
