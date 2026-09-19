open! Core

(** Subsequence matching with ranking: prefix > word start > substring >
    subsequence; ties broken by shorter candidate. Case-insensitive. *)

val score : query:string -> string -> int option

(** Candidates that match, best first, stable for equal scores. *)
val rank : query:string -> 'a list -> key:('a -> string) -> 'a list
