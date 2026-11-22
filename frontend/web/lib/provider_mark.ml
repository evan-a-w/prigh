open! Core
open! Import

let icon provider : Html.Icon.t =
  match provider with
  | "openai" | "openai-codex" -> Openai
  | "anthropic" -> Anthropic
  | "deepseek" -> Deepseek
  | _ -> Spark
;;

let view ?(cls = "") provider =
  Html.icon ~cls:(String.strip ("provider-mark " ^ cls)) (icon provider)
;;
