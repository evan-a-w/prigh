open! Core
open! Import

(** Tool output as a monospace block. Beyond [head + tail] lines the middle
    is folded behind a "N more lines" toggle (a [<details>], no script). *)
val view : ?error:bool -> head:int -> tail:int -> string -> Node.t

(** The number of lines of [text], ignoring a trailing newline. *)
val line_count : string -> int
