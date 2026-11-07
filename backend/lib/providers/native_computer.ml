open! Core
open! Import

type t =
  | Anthropic_toolset
  | Anthropic_tool of
      { version : string
      ; beta : string
      ; zoom : bool
      }
  | Openai
[@@deriving sexp_of, equal]

let computer_20251124 =
  Anthropic_tool
    { version = "computer_20251124"
    ; beta = "computer-use-2025-11-24"
    ; zoom = true
    }
;;

let computer_20250124 =
  Anthropic_tool
    { version = "computer_20250124"
    ; beta = "computer-use-2025-01-24"
    ; zoom = false
    }
;;

let family id ~prefixes =
  List.exists prefixes ~f:(fun prefix ->
    String.equal id prefix || String.is_prefix id ~prefix:(prefix ^ "-"))
;;

(* As of the computer use tool's docs (November 2026). *)
let of_model (provider : Provider_id.t) id =
  match provider with
  | Anthropic ->
    if
      family
        id
        ~prefixes:[ "claude-opus-5-5"; "claude-sonnet-5-5"; "claude-haiku-5-5" ]
    then Some Anthropic_toolset
    else if
      family
        id
        ~prefixes:
          [ "claude-opus-4-5"
          ; "claude-opus-4-6"
          ; "claude-opus-4-7"
          ; "claude-opus-4-8"
          ; "claude-opus-5"
          ; "claude-sonnet-4-6"
          ; "claude-sonnet-5"
          ; "claude-fable-5"
          ; "claude-mythos-5"
          ]
    then Some computer_20251124
    else if family id ~prefixes:[ "claude-sonnet-4-5"; "claude-haiku-4-5" ]
    then Some computer_20250124
    else None
  | Openai ->
    if
      List.exists [ "gpt-5.4"; "gpt-5.5"; "gpt-5.6"; "gpt-6" ] ~f:(fun prefix ->
        String.is_prefix id ~prefix)
    then Some Openai
    else None
  | Openai_codex | Deepseek | Custom _ -> None
;;

let form = function
  | Anthropic_toolset -> "anthropic_toolset"
  | Anthropic_tool _ -> "anthropic"
  | Openai -> "openai"
;;
