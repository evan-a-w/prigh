open! Core
module P = Prigh_protocol

(** JSON fixtures shared by the pure state-machine tests and the real-driver
    frame snapshots, so both drive the app from the same backend replies. *)

val model_json
  :  ?provider:string
  -> ?supports_thinking:bool
  -> ?context_window:int
  -> string
  -> string
  -> string

val models_json : string

val state_json
  :  ?model:string
  -> ?running:bool
  -> ?cwd:string
  -> ?git_branch:string
  -> ?thinking:string
  -> ?context_tokens:int
  -> ?cost_usd:float
  -> ?session_name:string
  -> unit
  -> string

val state
  :  ?model:string
  -> ?running:bool
  -> ?cwd:string
  -> ?git_branch:string
  -> ?thinking:string
  -> ?context_tokens:int
  -> ?cost_usd:float
  -> ?session_name:string
  -> unit
  -> P.State.t

val sessions_json : string
val entries_json : string
val tree_json : string
val stats_json : string
val auth_json : string
val config_json : string
val messages_json : string
val assistant : ?stop:string -> string -> P.Message.t
val partial : P.Message.Assistant.t
val tool_call : ?name:string -> ?arguments:string -> string -> P.Tool_call.t

val tool_result
  :  ?name:string
  -> ?is_error:bool
  -> id:string
  -> string
  -> P.Message.Tool_result.t
