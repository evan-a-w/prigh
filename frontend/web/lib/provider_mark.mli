open! Core
open! Import

(** A provider's logo (Lobe Icons' marks, see [Html.Icon]), recognised by
    its name: OpenAI (also its ChatGPT sign-in), Anthropic, DeepSeek, Google,
    OpenRouter, Ollama, xAI, Mistral, Groq, Qwen and Moonshot, also as the
    name of a custom provider ("my-openrouter"). [None] for any other. *)
val icon : string -> Html.Icon.t option

(** What {!view} takes for a model: an ACP agent's model ([provider]
    [acp:NAME]) is shown by its maker, recognised by its [name] ("Claude
    Sonnet 4.6", "GPT-OSS 120B"), else by the agent. *)
val of_model : provider:string -> name:string -> string

(** The first letter of a provider's name, an ACP agent's ([acp:NAME])
    being its NAME's. *)
val letter : string -> string

(** The logo, or the name's first letter for a provider without one. *)
val view : ?cls:string -> string -> Node.t
