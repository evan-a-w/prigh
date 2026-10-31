open! Core
open! Import

(** Reading the hosts' JSON leniently: a missing or ill-typed field is a
    default rather than an error. *)

val path : Json.t -> string list -> Json.t option
val str : Json.t -> string list -> string
val str_opt : Json.t -> string list -> string option
val int : Json.t -> string list -> int option
val bool : Json.t -> string list -> bool

(** The [field] of each object of the array at [names] ([labels] →
    [name]). *)
val strings : Json.t -> string list -> field:string -> string list

val number : int -> Json.t
val strings_json : string list -> Json.t
