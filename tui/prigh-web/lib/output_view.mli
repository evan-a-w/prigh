open! Core
open! Import

(** Tool output as a monospace block. Beyond [lines] lines it is clipped to
    them, faded where it goes on, over a "Show all N lines" toggle of the
    [fold] with that key; expanded, everything shows above a toggle back.
    [from_end] (by default [error], where the error is at the end) shows the
    last lines rather than the first. *)
val view
  :  ?error:bool
  -> ?from_end:bool
  -> fold:Fold.t * string
  -> lines:int
  -> string
  -> Node.t

(** Output still streaming: its last [tail] lines, under how many came
    before. [lines]: the newlines in all of it, of which [text] may be only
    the end. *)
val live : lines:int -> tail:int -> string -> Node.t

(** The number of lines of [text], ignoring a trailing newline. *)
val line_count : string -> int
