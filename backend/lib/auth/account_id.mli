open! Core
open! Import

(** Names one of a provider's logins (accounts): [main] is the one stored
    under the provider's own key in auth.json (pi's), others are
    [[a-z0-9-]{1,32}] slugs of their labels ([work], [personal-2]). *)

type t [@@deriving sexp, equal, compare]

val main : t
val is_main : t -> bool
val to_string : t -> string

(** Errors say what an id may contain. *)
val of_string : string -> t Or_error.t

(** A new id for an account labelled [label]: its slug (lowercase letters
    and digits, other characters as [-]), made unique among [taken] with a
    [-2], [-3], ... suffix; [account] when the label has no letter or
    digit. Never [main]. *)
val of_label : string -> taken:t list -> t
