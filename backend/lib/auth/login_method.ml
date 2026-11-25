open! Core
open! Import

type t =
  | Api_key
  | Oauth
[@@deriving sexp, equal, enumerate]

let to_string = function
  | Api_key -> "api_key"
  | Oauth -> "oauth"
;;

let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)

let label provider t =
  match provider, t with
  | Provider_id.Anthropic, Oauth -> "Anthropic (Claude Pro/Max)"
  | Anthropic, Api_key -> "Anthropic API key"
  | Openai, Api_key -> "OpenAI API key"
  | Openai, Oauth -> "OpenAI OAuth"
  | Openai_codex, Oauth -> "OpenAI (ChatGPT Plus/Pro)"
  | Openai_codex, Api_key -> "OpenAI Codex API key"
  | Deepseek, Api_key -> "DeepSeek API key"
  | Deepseek, Oauth -> "DeepSeek OAuth"
  | Custom name, Api_key -> name ^ " API key"
  | Custom name, Oauth -> name ^ " OAuth"
;;

let of_credential : Credential.t -> t = function
  | Api_key _ -> Api_key
  | Oauth _ -> Oauth
;;
