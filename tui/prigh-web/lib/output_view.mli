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
  -> ?elided:Elided.t
       (** [text] is cut: its first and last lines; expanded, a button
           ([load_attribute]) fetches the rest *)
  -> fold:Fold.t * string
  -> lines:int
  -> string
  -> Node.t

(** Output still streaming: its last [tail] lines, under how many came
    before. [lines]: the newlines in all of it, of which [text] may be only
    the end. [missed]: some of it streamed while the page did not follow. *)
val live : lines:int -> missed:bool -> tail:int -> string -> Node.t

(** The number of lines of [text], ignoring a trailing newline. *)
val line_count : string -> int

(** The attribute holding the [Fold] key of the output a button loads whole:
    the page listens for clicks on it. *)
val load_attribute : string
