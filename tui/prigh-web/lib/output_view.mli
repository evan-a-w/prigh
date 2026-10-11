open! Core
open! Import

(** Tool output as a monospace block. Beyond [head + tail] lines the middle
    folds behind a "Show all N lines" toggle of the [fold] with that key,
    unless it is expanded, when everything shows above a toggle back. *)
val view
  :  ?error:bool
  -> fold:Fold.t * string
  -> head:int
  -> tail:int
  -> string
  -> Node.t

(** Output still streaming: its last [tail] lines, under how many came
    before. [lines]: the newlines in all of it, of which [text] may be only
    the end. *)
val live : lines:int -> tail:int -> string -> Node.t

(** The number of lines of [text], ignoring a trailing newline. *)
val line_count : string -> int
