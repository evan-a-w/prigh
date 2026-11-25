open! Core
open! Import

(** Which login an agent CLI runs a user's composer sessions on: its own (on
    the session's tool host), or one of prigh's logins (the user's own or
    one shared with them), handed to the CLI for the session
    ({!Cli_credential}). Kept per user and adapter in config.json, as
    [adapters.ADAPTER.login] (absent: its own). *)

type t =
  | Own
  | Prigh of Account_ref.t
[@@deriving sexp_of, equal]

(** The provider whose logins an adapter can run on: [codex] runs on
    [openai-codex] (ChatGPT), [claude] on [anthropic] (Claude Pro/Max or an API key); [None]: only on its own. *)
val provider : adapter:string -> Provider_id.t option

(** The adapters that can run on one of prigh's logins. *)
val adapters : string list

(** [None] (JSON [null]) for [Own], else the login's ref. *)
val to_json : t -> Json.t

val of_json : Json.t -> t Or_error.t

(** Every adapter's choice the file names (others are [Own]); an unreadable
    file names none. *)
val load : home:string -> (string * t) list

val find : home:string -> adapter:string -> t

(** Errors for an adapter that runs only on its own login. *)
val save : home:string -> adapter:string -> t -> unit Or_error.t
