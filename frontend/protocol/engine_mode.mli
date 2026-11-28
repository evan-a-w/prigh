open! Core

(** The three execution modes for a session's engine:
    - {!Native}: prigh's built-in harness calling the model directly
    - {!Bridge}: an external agent running on the prigh server, bridging
      tools to the session's tool host over MCP ([backend:*])
    - {!Host}: an external agent running directly on the active tool host
      ([codex], [claude], [acp:*]) *)
type t =
  | Native
  | Bridge
  | Host
[@@deriving sexp, equal, compare]

val all : t list

(** ["native"], ["bridge"], ["host"]. *)
val to_string : t -> string

val of_string : string -> t option

(** ["Native"], ["Bridge"], ["Host"]. *)
val label : t -> string

(** One-line description of what runs in this mode. *)
val describe : t -> string

val of_adapter : string option -> t
val of_provider : string -> t
val of_engine : Session_engine.t -> t

(** The subset of {!all} that have at least one deployed engine or adapter. *)
val deployed_modes : Deployed_engines.t -> t list

(** The choices from {!Deployed_engines.choices} that belong to [t]. *)
val choices_for_mode : t -> Deployed_engines.t -> Deployed_engines.Choice.t list

(** Formats a model name with its mode tag:
    ["<name> @ native"], ["<name> @ bridge"], or ["<name> @ host"]. *)
val format_model : name:string -> provider:string -> string
