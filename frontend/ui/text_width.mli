open! Core

(** Terminal display width; platform-neutral so layout can be tested. *)

val uchar : Uchar.t -> int
val string : string -> int

(** Splits a string into display-width-aware pieces: [uchars s] lists each
    scalar value with its width (0 for combining marks). *)
val uchars : string -> (string * int) list

(** Longest prefix that fits in [width] columns and the remainder. *)
val take : string -> width:int -> string * string

(** Truncates to [width] columns, appending [ellipsis] if anything was cut. *)
val truncate : ?ellipsis:string -> string -> width:int -> string

val pad_right : string -> width:int -> string
