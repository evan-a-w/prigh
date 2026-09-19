open! Core
open! Import

type t =
  | Anthropic
  | Openai
  | Openai_codex
  | Deepseek
[@@deriving sexp, equal, compare, enumerate]

let to_string = function
  | Anthropic -> "anthropic"
  | Openai -> "openai"
  | Openai_codex -> "openai-codex"
  | Deepseek -> "deepseek"
;;

let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)

let display_name = function
  | Anthropic -> "Anthropic"
  | Openai -> "OpenAI"
  | Openai_codex -> "OpenAI Codex (ChatGPT)"
  | Deepseek -> "DeepSeek"
;;
