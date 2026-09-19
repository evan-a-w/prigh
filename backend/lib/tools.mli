open! Core

(** The fixed set of built-in tools. *)

val all : Tool.t list
val find : string -> Tool.t option
val specs : Tool.t list -> Tool_spec.t list
