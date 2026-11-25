open! Core
open! Import

(** How a provider is logged in to: an API key or an OAuth subscription. *)

type t =
  | Api_key
  | Oauth
[@@deriving sexp, equal, enumerate]

val to_string : t -> string
val of_string : string -> t option

(** ["OpenAI (ChatGPT Plus/Pro)"], ["Anthropic API key"]: also an account's
    label until the user names it. *)
val label : Provider_id.t -> t -> string

val of_credential : Credential.t -> t
