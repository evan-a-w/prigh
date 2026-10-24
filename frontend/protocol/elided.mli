open! Core

(** Where [get_messages_page] cut a long tool output: the text kept is its
    first lines then its last, the first [at] bytes being the first lines.
    [lines] and [bytes] are those of the whole output ([get_tool_output]). *)
type t =
  { at : int
  ; lines : int
  ; bytes : int
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** The first lines and the last of [text], as cut at [t]. *)
val split : t -> string -> string * string
