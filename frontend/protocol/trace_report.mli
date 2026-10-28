open! Core

(** The [trace] method's reply: the session's trace summarised (the text
    [prigh trace] prints), the level it records at ([off], [default] or
    [detailed]), how many spans it holds, and with [perfetto] the Chrome
    trace event format, to download for Perfetto. *)

type t =
  { summary : string
  ; level : string
  ; spans : int
  ; perfetto : Json.t option
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
