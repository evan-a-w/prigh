open! Core
open! Import

(** [web_search]: a provider-side ("server") tool. Providers that have one
    (Anthropic's [web_search], OpenAI Responses' [web_search]) are sent their
    native tool instead of a function, run the searches themselves and stream
    them back as [Server_tool] content; models of other providers do not get
    the tool at all ({!Tools.for_model}). *)

val name : string
val tool : Tool.t

(** Whether [model]'s provider runs web searches itself for it (not
    OpenAI's older models: [gpt-4], [gpt-4.1-nano], [o1], ...). *)
val supported : Model.t -> bool

val is_spec : Tool_spec.t -> bool

(** The readable summary of a search's results: one numbered [title — url]
    line each (with the page's age when known), or [no results]. *)
val results_text : (string * string * string option) list -> string
