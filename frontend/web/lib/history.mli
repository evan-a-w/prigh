open! Core

(** Sent prompts, recalled with Up and Down in the editor. *)

type t [@@deriving sexp_of]

val empty : t

(** Newest first, as saved. *)
val of_list : string list -> t

val to_list : t -> string list

(** Records a sent prompt (no consecutive duplicates; at most 100) and stops
    browsing. *)
val add : t -> string -> t

(** The older entry, remembering [draft] when browsing starts; [None] at the
    oldest. *)
val older : t -> draft:string -> (t * string) option

(** The newer entry, or the remembered draft past the newest; [None] when not
    browsing. *)
val newer : t -> (t * string) option

val browsing : t -> bool
