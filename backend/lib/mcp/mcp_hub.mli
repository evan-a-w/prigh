open! Core
open! Import

(** The MCP servers running on one tool host (the backend, or a [prigh
    tool-host] process), shared by every session that uses the host. A
    server is started the first time a session needs it and kept, keyed by
    {!Mcp_config.Server.key}, so editing its definition starts a new one;
    a failed start is remembered (not retried on every turn) until
    [reconnect].

    HTTP servers get the OAuth access tokens the backend sends with each
    request ([~tokens], by server URL; {!Mcp_auth}): a server that wanted a
    sign-in starts again once its token changes, and one whose token went
    away (signed out) is restarted without it. *)

module Status : sig
  type t =
    | Ready of Mcp_client.Tool.t list
    | Failed of string
    | Needs_approval
    | Needs_sign_in of string
    (** an HTTP server that advertises OAuth answered 401 *)
  [@@deriving sexp_of]
end

module Server_status : sig
  type t =
    { server : Mcp_config.Server.t
    ; status : Status.t
    }
  [@@deriving sexp_of]
end

type t

val create : env:Env.t -> sw:Switch.t -> unit -> t

(** The servers for [cwd] ({!Mcp_config.discover}, with the [settings]'
    servers) with their tools, starting approved ones not running yet
    (concurrently; concurrent callers share a start). With [reconnect],
    failed and dead servers are started again. Also returns the
    configuration problems. Servers from earlier settings that [settings]
    no longer defines as they ran (removed, disabled or edited) are stopped:
    the backend sends one user's settings to a host. *)
val servers
  :  t
  -> ?reconnect:bool
  -> ?settings:Json.t
  -> ?tokens:(string * string) list
  -> cwd:string
  -> home:string
  -> unit
  -> Server_status.t list * string list

(** Calls [tool] on the server named [server] defined in [source] (a path
    from {!Mcp_config.Server.source}), starting it if needed. *)
val call
  :  t
  -> cancel:Cancellation.t
  -> settings:Json.t (** the servers from the settings *)
  -> ?tokens:(string * string) list
  -> source:string
  -> server:string
  -> home:string
  -> tool:string
  -> arguments:Json.t
  -> unit
  -> Tool_result.t

(** Stops every server. *)
val close : t -> unit
