open! Core

(** The result of [list_mcp] and [mcp_approve]: every configured MCP server and
    the configuration problems found. *)
type t =
  { servers : Mcp_server.t list
  ; problems : string list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
