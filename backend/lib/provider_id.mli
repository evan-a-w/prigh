open! Core

type t =
  | Anthropic
  | Openai
  | Openai_codex
  | Deepseek
[@@deriving sexp, equal, compare, enumerate]

val to_string : t -> string
val of_string : string -> t option
val display_name : t -> string
