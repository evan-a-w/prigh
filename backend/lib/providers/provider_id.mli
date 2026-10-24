open! Core

(** A model provider: one of the built-in ones, or a custom OpenAI-compatible
    endpoint defined in [config.json] (see {!Custom_provider}), named by the
    user. *)

type t =
  | Anthropic
  | Openai
  | Openai_codex
  | Deepseek
  | Custom of string
[@@deriving sexp, equal, compare]

val builtins : t list

(** The name used in model keys, [auth.json] and commands. *)
val to_string : t -> string

(** Built-in providers only: custom names are only known with the config. *)
val of_builtin_string : string -> t option

val display_name : t -> string
val is_custom : t -> bool
