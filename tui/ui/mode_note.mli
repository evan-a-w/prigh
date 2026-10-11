open! Core
open! Import

(** The backend tells the model about plan mode in a note at the start of a
    user message ([\[Plan mode is on: ...\]] or [\[Plan mode is off: ...\]]),
    or in a message of its own between turns. Frontends show it as one short
    line instead. *)

(** Whether the text starts with a plan mode note (on or off) and the text
    after it. *)
val split : string -> (bool * string) option

(** [plan mode on: the model was told to plan, not change files] and the
    like. *)
val summary : on:bool -> string
