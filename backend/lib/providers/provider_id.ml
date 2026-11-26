open! Core
open! Import

type t =
  | Anthropic
  | Openai
  | Openai_codex
  | Deepseek
  | Google_antigravity
  | Custom of string
[@@deriving sexp, equal, compare]

let builtins = [ Anthropic; Openai; Openai_codex; Deepseek; Google_antigravity ]

let to_string = function
  | Anthropic -> "anthropic"
  | Openai -> "openai"
  | Openai_codex -> "openai-codex"
  | Deepseek -> "deepseek"
  | Google_antigravity -> "google-antigravity"
  | Custom name -> name
;;

let of_builtin_string s =
  List.find builtins ~f:(fun t -> String.equal (to_string t) s)
;;

let display_name = function
  | Anthropic -> "Anthropic"
  | Openai -> "OpenAI API"
  | Openai_codex -> "ChatGPT"
  | Deepseek -> "DeepSeek"
  | Google_antigravity -> "Antigravity (Google)"
  | Custom name -> name
;;

let is_custom = function
  | Custom _ -> true
  | Anthropic | Openai | Openai_codex | Deepseek | Google_antigravity -> false
;;
