open! Core
open! Import

(** What a tool host reports about its MCP servers ([$mcp_servers]), and
    the prigh tools made from it. *)

module Status : sig
  type t =
    | Ready
    | Failed of string
    | Needs_approval
  [@@deriving sexp_of]
end

module Server : sig
  type t =
    { name : string
    ; source : string
    ; project : bool
    ; status : Status.t
    ; tools : Mcp_client.Tool.t list
    }
  [@@deriving sexp_of]
end

module Listing : sig
  type t =
    { servers : Server.t list
    ; problems : string list
    }
  [@@deriving sexp_of]

  val empty : t
  val of_hub : Mcp_hub.Server_status.t list * string list -> t
  val to_json : t -> Json.t
  val of_json : Json.t -> t Or_error.t

  (** The RPC's [list_mcp] result: tools under their prigh names. *)
  val to_rpc_json : t -> Json.t

  (** One line per problem, failed server and server awaiting approval,
      each saying what to do. *)
  val notices : t -> string list
end

(** [mcp__SERVER__TOOL], with characters providers reject replaced by [_]
    and cut to 64 characters. *)
val tool_name : server:string -> tool:string -> string

(** The tools of the ready servers; [call] runs one on the host. Names that
    collide after sanitising keep the first. *)
val tools
  :  Listing.t
  -> call:
       (Tool.Context.t
        -> source:string
        -> server:string
        -> tool:string
        -> Json.t
        -> Tool_result.t)
  -> Tool.t list
