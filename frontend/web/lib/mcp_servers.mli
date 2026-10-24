open! Core
open! Import

(** [/mcp]: the MCP servers ([list_mcp], [mcp_approve]) as a picker, and what
    to say about them. *)

(** A server's picker item id. *)
val id : Mcp_server.t -> string

val find : Mcp_list.t -> id:string -> Mcp_server.t option

(** Where to configure servers, when there are none. *)
val none : string

(** The servers, those to act on (needing approval, failed) first; the
    highlight on [highlight] (an [id]). *)
val picker : ?highlight:string -> Mcp_list.t -> Dialog.t

(** A one-line report of [list_mcp]'s result (after [/mcp reconnect]), and
    whether something is wrong. *)
val summary : Mcp_list.t -> string * [ `Error of bool ]

(** What became of the server [id] after approving or restarting it, and
    whether it failed. *)
val outcome : Mcp_list.t -> id:string -> string * [ `Error of bool ]
