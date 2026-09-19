open! Core
open! Import

(** Typed accessors over a JSON-object tool argument. Missing or ill-typed
    values raise [Invalid], which [Tool.execute] turns into an error result. *)

exception Invalid of string

val string : Json.t -> string -> string
val string_opt : Json.t -> string -> string option
val int_opt : Json.t -> string -> int option
val bool_opt : Json.t -> string -> bool option
val list_opt : Json.t -> string -> Json.t list option
val string_list_opt : Json.t -> string -> string list option

(** Builds a JSON schema for an object with the given properties. *)
val schema
  :  ?required:string list
  -> (string * [ `String | `Integer | `Boolean | `Array of Json.t ] * string)
       list
  -> Json.t
