open! Core
open! Import

(** A provider's logo (Lobe Icons' marks, see [Html.Icon]), recognised by
    its name: OpenAI (also its ChatGPT sign-in), Anthropic, DeepSeek, Google,
    OpenRouter, Ollama, xAI, Mistral, Groq, Qwen and Moonshot, also as the
    name of a custom provider ("my-openrouter"). [None] for any other. *)
val icon : string -> Html.Icon.t option

(** The logo, or the name's first letter for a provider without one. *)
val view : ?cls:string -> string -> Node.t
