open! Core

(** A subagent's report as the backend words it: the text, then a line like
    [\[subagent: 2 turns, 10 in / 5 out tokens, $0.0012\]]. *)

type t =
  { text : string (** without the stats line *)
  ; stats : string option (** e.g. [2 turns, 10 in / 5 out tokens, $0.0012] *)
  }
[@@deriving sexp_of]

val of_string : string -> t

(** The agent id in a background [subagent] call's result,
    [started agent a1 (...); ...]. *)
val started : string -> string option
