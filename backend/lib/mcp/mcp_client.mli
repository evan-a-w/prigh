open! Core
open! Import

(** A connection to one MCP server (protocol 2025-06-18, JSON-RPC 2.0) over
    stdio (a child process speaking newline-delimited JSON on its
    stdin/stdout, its stderr kept for error messages) or streamable HTTP
    (each message POSTed; the reply is JSON or an SSE stream; the
    [Mcp-Session-Id] header is echoed). Only tools are used. *)

module Tool : sig
  type t =
    { name : string
    ; description : string
    ; input_schema : Json.t
    ; read_only : bool (** [annotations.readOnlyHint] *)
    }
  [@@deriving sexp_of]
end

type t

module Connect_error : sig
  type t =
    { message : string (** what failed, with the end of a process's stderr *)
    ; needs_sign_in : bool
      (** an HTTP server answered 401 and advertises OAuth
          ({!Mcp_oauth.discover}) *)
    }
  [@@deriving sexp_of]
end

(** Starts the server (as a process in [sw], in [server.dir]) and performs
    the [initialize] handshake, giving up after [timeout] (default 60s).
    An HTTP server's requests carry [Authorization: Bearer (bearer ())]
    (read at every request) unless its [headers] have an [Authorization]. *)
val connect
  :  env:Env.t
  -> sw:Switch.t
  -> ?timeout:Time_ns.Span.t
  -> ?bearer:(unit -> string option)
  -> Mcp_config.Server.t
  -> (t, Connect_error.t) Result.t

(** [tools/list], following pagination. Cached until the server sends
    [notifications/tools/list_changed]. *)
val tools : t -> Tool.t list Or_error.t

(** [tools/call]: text content joined, images as images, other content as
    JSON; [isError] makes an error result. Cancelling sends
    [notifications/cancelled] and returns a [[cancelled]] error. *)
val call
  :  t
  -> cancel:Cancellation.t
  -> tool:string
  -> arguments:Json.t
  -> Tool_result.t

(** [Some reason] once the connection is unusable (the process exited, the
    stream closed). *)
val failure : t -> string option

(** Whether it failed because the server wants a (new) OAuth sign-in. *)
val needs_sign_in : t -> bool

(** Stops the server: closes its stdin, then kills its process group. *)
val close : t -> unit
