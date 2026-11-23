open! Core
open! Import

let icon provider : Html.Icon.t option =
  let p = String.lowercase provider in
  let has s = String.is_substring p ~substring:s in
  if has "openai" || has "codex" || has "chatgpt"
  then Some Openai
  else if has "anthropic" || has "claude"
  then Some Anthropic
  else if has "deepseek"
  then Some Deepseek
  else if has "gemini" || has "google" || has "vertex"
  then Some Gemini
  else if has "openrouter"
  then Some Openrouter
  else if has "ollama"
  then Some Ollama
  else if has "xai" || has "grok"
  then Some Xai
  else if has "mistral"
  then Some Mistral
  else if has "groq"
  then Some Groq
  else if has "qwen" || has "dashscope" || has "alibaba"
  then Some Qwen
  else if has "moonshot" || has "kimi"
  then Some Moonshot
  else None
;;

let letter provider =
  match String.find provider ~f:Char.is_alphanum with
  | Some c -> String.of_char (Char.uppercase c)
  | None -> "?"
;;

let view ?(cls = "") provider =
  match icon provider with
  | Some i -> Html.icon ~cls:(String.strip ("provider-mark " ^ cls)) i
  | None ->
    Node.span
      ~attrs:
        [ Attr.class_ (String.strip ("provider-mark provider-letter " ^ cls))
        ; Attr.create "aria-hidden" "true"
        ]
      [ Node.text (letter provider) ]
;;
