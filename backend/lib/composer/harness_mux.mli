open! Core
open! Import

(** Multiplexes multiple independent prigh sessions over a single long-lived
    external harness process speaking JSON-RPC over stdio (ACP or Codex
    [app-server]).

    The multiplexer runs [initialize] once on the physical process and caches
    its response. Each leased virtual {!Line_transport.t} gets:
    - an immediate replay of the cached [initialize] response;
    - unique wire JSON-RPC request IDs, rewritten back on response;
    - strict per-session routing of notifications and server requests by ACP
      [sessionId] or Codex [threadId] (including Codex-internal subagent
      threads);
    - an isolated per-session MCP route over the shared host MCP socket
      ({!Line_transport.Mcp.t.route});
    - graceful session teardown on {!Line_transport.stop} ([session/close] for
      ACP, [thread/unsubscribe] for Codex) without killing the shared process.

    When the physical process exits or crashes, every active virtual session
    receives EOF and the physical {!Line_transport.Exit.t}. *)

module Protocol : sig
  type t =
    | Acp
    | Codex
  [@@deriving sexp_of, equal]
end

type t

(** Starts reading [transport], runs the one-time [initialize] handshake
    (and [initialized] for Codex), and determines whether the process can
    safely be pooled:
    - ACP requires [sessionCapabilities.close] and routed MCP support (when an
      MCP side channel is present);
    - Codex requires routed MCP support (when an MCP side channel is present).

    Even when [poolable] is [false] (or the process exited before answering
    [initialize]), returns [Ok (t, false)] so the initiating caller can still
    lease a single virtual transport and receive the exact handshake error or
    exit status. *)
val create
  :  sw:Switch.t
  -> protocol:Protocol.t
  -> ?request_timeout:Time_float.Span.t
  -> ?on_empty:(t -> unit)
  -> ?on_drain:(t -> unit)
  -> ?on_exit:(t -> unit)
  -> Line_transport.t
  -> (t * bool) Or_error.t

(** Leases a virtual {!Line_transport.t} for one session on this process. *)
val lease : t -> Line_transport.t

(** Number of currently active leased sessions. *)
val active_sessions : t -> int

(** Whether the underlying physical process is still alive. *)
val alive : t -> bool

(** Stops the underlying physical process and ends all active virtual
    sessions. *)
val stop : t -> unit
