open! Core
open! Import

type t =
  { name : string
  ; description : string
  ; parameters : Json.t
  ; parallel_safe : bool
  ; destructive : bool
  ; on_host : bool
    (** Runs on the session's active tool host (filesystem and process
          tools) rather than always in the backend (e.g. subagent). *)
  ; freeform : string option [@sexp.option]
    (** A Lark grammar: providers with custom tools (OpenAI Responses) let
        the model write the input as free text matching it rather than JSON.
        Such tools take one string parameter, [input], which is how the
        text reaches [run] and how other providers send it. *)
  }
[@@deriving sexp_of]
