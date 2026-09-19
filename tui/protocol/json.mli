open! Core

(** Decoding helpers over [Jsonaf.t] with field-path error messages. *)

type t = Jsonaf.t [@@deriving sexp_of]

val equal : t -> t -> bool
val str : string -> t
val int : int -> t
val float : float -> t
val bool : bool -> t
val obj : (string * t) list -> t
val to_string : t -> string
val parse : string -> t Or_error.t

(** Field lookup; [None] when absent or [`Null]. *)
val field : t -> string -> t option

val string_field : t -> string -> string Or_error.t
val string_opt_field : t -> string -> string option Or_error.t
val int_field : t -> string -> int Or_error.t
val float_field : t -> string -> float Or_error.t
val bool_field : t -> string -> bool Or_error.t
val list_field : t -> string -> f:(t -> 'a Or_error.t) -> 'a list Or_error.t
val object_field : t -> string -> t Or_error.t
val to_string_or_error : t -> string Or_error.t
