open! Core

(** Bounds text by both line and byte count for inclusion in tool results. *)

type t =
  { text : string
  ; truncated : bool
  ; total_lines : int
  ; total_bytes : int
  }
[@@deriving sexp_of]

val count_lines : string -> int
val default_max_lines : int
val default_max_bytes : int

(** Keeps the beginning. *)
val head : ?max_lines:int -> ?max_bytes:int -> string -> t

(** Keeps the end; useful for command output where the conclusion matters. *)
val tail : ?max_lines:int -> ?max_bytes:int -> string -> t
