open! Core

(** Subsequence matching with ranking: prefix > word start > substring >
    subsequence, subsequences by how many of their letters are adjacent or
    start words; ties broken by shorter candidate. Case-insensitive. *)

val score : query:string -> string -> int option

(** Candidates that match, best first, stable for equal scores. *)
val rank : query:string -> 'a list -> key:('a -> string) -> 'a list

(** Where [query] matches [candidate], as {!score} matches it: the byte
    offsets of the letters matched, in order; [None] when it does not. *)
val positions : query:string -> string -> int list option
