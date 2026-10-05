open! Core
open! Import

(** The pi web frontend's wire shapes (pi's RPC protocol, one JSON object per
    WebSocket frame) built from prigh's [Rpc_json] output. Everything here is
    JSON to JSON so the adapter can sit on top of [Rpc_server.handle]. *)

(** pi's thinking levels ([off], [minimal], [low], [medium], [high], [xhigh],
    [max]) against prigh's ([off], [on], [low], [high], [max]). *)
module Thinking_level : sig
  val of_prigh : string -> string
  val to_prigh : string -> string

  (** The levels a model offers, in cycling order. *)
  val available : supports_thinking:bool -> string list

  val next : supports_thinking:bool -> current:string -> string
end

(** A prigh message as a pi [AgentMessage] with the given [timestamp] (pi keys
    messages by role and timestamp). *)
val message : timestamp:int -> Json.t -> Json.t

(** [Rpc_json.state] as pi's [RpcSessionState]. *)
val session_state : Json.t -> Json.t

(** [Rpc_json.model] as pi's [Model]. *)
val model : Json.t -> Json.t

(** pi's [SessionStats] from prigh's [session_stats] result and the state. *)
val session_stats : state:Json.t -> stats:Json.t -> Json.t

(** The [get_commands] result: the builtins pi's web UI handles itself plus
    the prigh-specific commands the adapter runs. *)
val commands : Json.t

(** Names of the slash commands the adapter runs server-side. *)
val server_commands : string list

(** The [content] list of a tool result. *)
val tool_result_content : ?images:Json.t list -> string -> Json.t

(** A prigh message's or result's [images] as pi image blocks. *)
val image_blocks : Json.t -> Json.t list

(** A pi command's [images] as prigh's. *)
val prigh_images : Json.t -> Json.t list

(** A tool call's [arguments] string as a JSON object (pi wants an object;
    a partial or invalid string becomes [{}]). *)
val arguments_object : string -> Json.t

val response : id:Json.t -> command:string -> Json.t Or_error.t -> Json.t
val event : string -> (string * Json.t) list -> Json.t

(** [extension_ui_request] with the given method and fields. *)
val ui_request : id:string -> meth:string -> (string * Json.t) list -> Json.t

val notify : ?kind:string -> string -> Json.t

(** A pi [custom] message shown in the chat (markdown). *)
val custom_message : timestamp:int -> kind:string -> string -> Json.t
