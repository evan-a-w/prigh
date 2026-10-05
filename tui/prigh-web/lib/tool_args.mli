open! Core
open! Import

(** A tool call's arguments, read leniently: while the model is still
    streaming them they are an unfinished JSON object, from which complete and
    partial string fields are still extracted. *)

type t

val of_call : Tool_call.t -> t

(** A string field; while streaming, its text so far. *)
val string : t -> string -> string option

val bool : t -> string -> bool option
val int : t -> string -> int option
val strings : t -> string -> string list

(** [(old_text, new_text)] pairs of an [edit] call. *)
val edits : t -> (string * string) list
