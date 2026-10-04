open! Core

(** The end of a stream of output: the last [capacity] bytes plus the total
    byte count. *)

type t

(** [capacity] defaults to 1 MB. *)
val create : ?capacity:int -> unit -> t

val add : t -> string -> unit
val total_bytes : t -> int

(** The retained lines, oldest first, without a partial first line when older
    output was dropped; a trailing unterminated line is included. *)
val lines : t -> string list

(** The last non-blank line. *)
val last_line : t -> string option
