open! Core
open! Import

type t =
  | Anthropic
  | Openai
  | Openai_codex
  | Deepseek
  | Gemini
  | Custom of string
[@@deriving sexp, equal, compare]

let builtins = [ Anthropic; Openai; Openai_codex; Deepseek; Gemini ]

let to_string = function
  | Anthropic -> "anthropic"
  | Openai -> "openai"
  | Openai_codex -> "openai-codex"
  | Deepseek -> "deepseek"
  | Gemini -> "gemini"
  | Custom name -> name
;;

let of_builtin_string s =
  List.find builtins ~f:(fun t -> String.equal (to_string t) s)
;;

let display_name = function
  | Anthropic -> "Anthropic"
  | Openai -> "OpenAI"
  | Openai_codex -> "OpenAI Codex (ChatGPT)"
  | Deepseek -> "DeepSeek"
  | Gemini -> "Google Gemini"
  | Custom name -> name
;;

let is_custom = function
  | Custom _ -> true
  | Anthropic | Openai | Openai_codex | Deepseek | Gemini -> false
;;
