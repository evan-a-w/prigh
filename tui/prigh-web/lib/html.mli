open! Core
open! Import

(** Small constructors for the transcript's DOM. Children equal to
    [Node.none] are dropped, so optional parts leave no trace. *)

val present : Node.t list -> Node.t list

(** A [<div>] with [cls] as its class attribute. *)
val div : string -> Node.t list -> Node.t

(** A [<span>] of text with [cls] as its class attribute. *)
val span : string -> string -> Node.t

(** A [<details>], closed, whose summary is a [label] and a one-line
    [preview]. *)
val folded : cls:string -> label:string -> preview:string -> Node.t -> Node.t

(** The first non-blank line of [s]. *)
val first_line : string -> string

val plural : int -> string -> string
