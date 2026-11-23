open! Core
open! Import

(** [prigh mcp-bridge]: the stdio MCP server an external harness on a tool
    host (Codex, an ACP agent) starts to reach prigh's injected tools
    ({!Injected_tools}). It connects to the Unix socket named by
    [$PRIGH_MCP_SOCKET], which the tool host listens on for that harness's
    process and relays to the backend, and copies bytes both ways: the
    JSON-RPC itself is the backend's business. *)

(** ["PRIGH_MCP_SOCKET"] *)
val socket_env : string

(** Copies [stdin] to [socket] and [socket] to [stdout] until the socket's
    side ends (the backend is done); at the end of [stdin] it shuts the
    socket's sending side and waits for the rest of the replies. *)
val relay
  :  stdin:_ Eio.Flow.source
  -> stdout:_ Eio.Flow.sink
  -> [> Eio.Flow.two_way_ty ] Eio.Resource.t
  -> unit

(** Connects to [socket] and {!relay}s this process's stdio; an error says
    what is wrong (no socket: prigh mcp-bridge is started by a harness that
    prigh runs, not by hand). *)
val run : Eio_unix.Stdenv.base -> socket:string option -> unit Or_error.t
