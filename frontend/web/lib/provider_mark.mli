open! Core
open! Import

(** A provider's mark: OpenAI's knot (also for its ChatGPT sign-in),
    Anthropic's A, DeepSeek's whale, a spark for any other (a custom
    provider). *)

val icon : string -> Html.Icon.t
val view : ?cls:string -> string -> Node.t
